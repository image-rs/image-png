use png::{Decoder, DecodingError};
use std::io::Write;

mod pipe {
    use std::cell::RefCell;
    use std::io::{BufRead, Cursor, Read, Result, Seek, SeekFrom, Write};
    use std::rc::Rc;

    pub fn create() -> (impl BufRead + Seek, impl Write) {
        let write_end = Pipe {
            buf: Rc::new(RefCell::new(Cursor::new(Vec::new()))),
            long_lived_fill_buf: Vec::new(),
        };
        let read_end = write_end.clone();
        (read_end, write_end)
    }

    #[derive(Clone)]
    struct Pipe {
        buf: Rc<RefCell<Cursor<Vec<u8>>>>,
        long_lived_fill_buf: Vec<u8>,
    }

    impl Read for Pipe {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            self.buf.borrow_mut().read(buf)
        }
    }

    impl BufRead for Pipe {
        fn fill_buf(&mut self) -> Result<&[u8]> {
            let mut buf = self.buf.borrow_mut();

            // `short_lived_fill_buf` lives as long as the `buf` borrow.
            let short_lived_fill_buf = buf.fill_buf()?;

            // Copy `short_lived_fill_buf` into a longer-lived buffer.
            self.long_lived_fill_buf.clear();
            self.long_lived_fill_buf
                .extend_from_slice(short_lived_fill_buf);
            Ok(&self.long_lived_fill_buf)
        }

        fn consume(&mut self, amount: usize) {
            self.buf.borrow_mut().consume(amount)
        }
    }

    impl Seek for Pipe {
        fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
            self.buf.borrow_mut().seek(pos)
        }
    }

    impl Write for Pipe {
        fn write(&mut self, buf: &[u8]) -> Result<usize> {
            self.buf.borrow_mut().get_mut().extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> Result<()> {
            self.buf.borrow_mut().flush()
        }
    }

    #[test]
    fn test_pipe() {
        let (input, mut output) = create();
        write!(&mut output, "foobar").unwrap();

        let input = std::io::read_to_string(input).unwrap();
        assert_eq!(input, "foobar");
    }
}

/// Regression test for a scenario that was:
///
/// * Discovered:
///     - When testing via `cargo fuzz run buf_independent` and discussed at
///       https://github.com/image-rs/image-png/pull/640#discussion_r2602412407
///     - In the wild: https://github.com/image-rs/image-png/issues/605
/// * Fixed by https://github.com/image-rs/image-png/pull/662
///
/// In this scenario:
///
/// * Decoding the full PNG (see `baseline_reader` or `byte_by_byte_reader`
///   in `fuzz/fuzz_targets/buf_independent.rs`) would
///       - Result in
///         `Err(Format(FormatError { inner: CorruptFlateStream { err: InsufficientInput } }))`
///         before https://github.com/image-rs/image-png/pull/662
///       - Result in `Ok(_)` afterwards
/// * Decoding streaming/partial input (see `intermittent_eofs_reader` in
///   `fuzz/fuzz_targets/buf_independent.rs`) would miss the `FormatError`
///   and return `Ok(_)`.
///
/// The input file here has an IDAT payload that covers all image rows, but
/// is truncated without properly terminating the zlib stream.
#[test]
fn test_partial_decode_with_unterminated_redundant_zlib_tail() {
    static CONTENT: &[u8] = &[
        // PNG header
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,
        // IHDR chunk (width=34, height=3, bits=8, color=6, interlaced)
        0x00, 0x00, 0x00, 0x0d, // length
        0x49, 0x48, 0x44, 0x52, // IHDR
        0x00, 0x00, 0x00, 0x22, 0x00, 0x00, 0x00, 0x03, 0x08, 0x06, 0x00, 0x00, 0x01, // data
        0x81, 0xb0, 0xed, 0xc7, // crc
        // IDAT chunk (truncated/incomplete zlib stream)
        0x00, 0x00, 0x00, 0x23, // length
        0x49, 0x44, 0x41, 0x54, // IHDR
        0x78, 0x5e, 0x63, 0x64, 0x06, 0x82, 0x01, 0x00, 0x83, 0x8b, // data
        0x21, 0xcc, 0x47, 0x0d, 0x0a, 0x64, 0x06, 0x89, 0x50, 0x4e, // data
        0x82, 0x8b, 0x21, 0x47, 0x0d, 0x0a, 0x41, 0x54, 0x78, 0x5e, // data
        0x63, 0x64, 0x06, 0x89, 0x50, // data
        0x08, 0x42, 0x67, 0x27, // crc
        // IEND chunk (to signal end of IDAT/zlib stream)
        0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82,
    ];

    // Initially only the first 80 bytes are available
    // (after the whole IDAT chunk, but before IEND chunk,
    // so not far enough to detect the end of IDAT/zlib stream).
    let (input, mut output) = pipe::create();
    let (prefix, suffix) = CONTENT.split_at(80);
    output.write_all(prefix).unwrap();

    // Decoding partial input should result in `UnexpectedEof`.
    let mut reader = Decoder::new(input).read_info().unwrap();
    let mut image = vec![0; reader.output_buffer_size().unwrap()];
    let prefix_result = reader.next_frame(&mut image);
    let DecodingError::IoError(prefix_err) = prefix_result.unwrap_err() else {
        panic!("Unexpected error variant");
    };
    assert_eq!(prefix_err.kind(), std::io::ErrorKind::UnexpectedEof);

    // Write the remaining input bytes and verify that no error is reported
    // (i.e. that the remaining `IDAT` payload is ignored).
    output.write_all(suffix).unwrap();
    let suffix_result = reader.next_frame(&mut image);
    assert!(suffix_result.is_ok());
}

/// Regression test for https://github.com/image-rs/image-png/issues/639
#[test]
fn test_partial_decode_success() {
    // The first 0x8D bytes from the following test image from Skia:
    // resources/images/apng-test-suite--dispose-ops--none-basic.png
    let partial_png: &[u8] = &[
        0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44,
        0x52, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x40, 0x08, 0x06, 0x00, 0x00, 0x00, 0xd2,
        0xd6, 0x7f, 0x7f, 0x00, 0x00, 0x00, 0x08, 0x61, 0x63, 0x54, 0x4c, 0x00, 0x00, 0x00, 0x03,
        0x00, 0x00, 0x00, 0x01, 0xb9, 0xea, 0x8a, 0x56, 0x00, 0x00, 0x00, 0x1a, 0x66, 0x63, 0x54,
        0x4c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x64, 0x00, 0x01, 0x26, 0x12, 0x2f,
        0xe0, 0x00, 0x00, 0x00, 0x93, 0x49, 0x44, 0x41, 0x54, 0x78, 0x9c, 0xed, 0xd2, 0xa1, 0x01,
        0x00, 0x30, 0x10, 0x84, 0xb0, 0xdb, 0x7f, 0xe9, 0xef, 0x18, 0x15, 0x44, 0xc4, 0x23, 0xd8,
        0x6d, 0x47, 0xd7, 0x7e, 0x07, 0x60, 0x00, 0x0c, 0x80, 0x01, 0x30, 0x00, 0x06, 0xc0, 0x00,
        0x18, 0x00, 0x03, 0x60, 0x00, 0x0c,
    ];

    let mut reader = Decoder::new(std::io::Cursor::new(partial_png))
        .read_info()
        .unwrap();
    let mut row = vec![0; reader.output_buffer_size().unwrap()];

    // The first 10 rows should decode successfully
    // (even though we only have the first 0x8D bytes of the full PNG).
    for i in 0..10 {
        let result = reader.read_row(&mut row);
        assert!(matches!(result, Ok(_)), "{result:?} at {i}");
    }

    // Attempting to decode the 11th row should return `UnexpectedEof` error.
    let result = reader.read_row(&mut row);
    let DecodingError::IoError(err) = result.unwrap_err() else {
        panic!("Unexpected error variant");
    };
    assert_eq!(err.kind(), std::io::ErrorKind::UnexpectedEof);
}
