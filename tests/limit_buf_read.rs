use std::fs;
use std::io::{self, BufReader, Cursor, Error, ErrorKind, Read};

use png::{Decoder, LimitBufRead, LimitBufReader};

/// A mock [`Read`] stream that rejects reads over the given `limit`.
///
/// This replicates integrations where a buffering layer above a blocking pipe attempts to satisfy
/// the requested size instead of returning a short read.
struct BlockOnLimit {
    inner: Cursor<Vec<u8>>,
    limit: usize,
    read_bytes: usize,
}

impl BlockOnLimit {
    fn new(data: Vec<u8>, limit: usize) -> Self {
        Self {
            inner: Cursor::new(data),
            limit,
            read_bytes: 0,
        }
    }
}

impl Read for BlockOnLimit {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let remaining = self.limit - self.read_bytes;
        if buf.len() > remaining {
            return Err(Error::new(
                ErrorKind::WouldBlock,
                "Blocked on eager overshoot request",
            ));
        }
        let n = self.inner.read(buf)?;
        self.read_bytes += n;
        Ok(n)
    }
}

fn decode_image<R: LimitBufRead>(reader: R) -> Result<(), png::DecodingError> {
    let decoder = Decoder::new(reader);
    let mut reader = decoder.read_info()?;
    let mut buf = vec![0; reader.output_buffer_size().unwrap()];
    reader.next_frame(&mut buf)?;
    reader.finish()?;
    Ok(())
}

#[test]
fn test_buf_reader_overshoots() {
    let png_data = fs::read("tests/pngsuite/basi0g01.png").unwrap();
    let mut concat_data = Vec::new();
    concat_data.extend_from_slice(&png_data);
    concat_data.extend_from_slice(&png_data);

    let mut cursor = Cursor::new(concat_data);
    let buf_reader = BufReader::new(&mut cursor);
    decode_image(buf_reader).expect("Standard BufReader decode failed");

    assert_eq!(cursor.position(), (png_data.len() * 2) as u64);
}

#[test]
fn test_buf_reader_blocks_on_blocking_source() {
    let png_data = fs::read("tests/pngsuite/basi0g01.png").unwrap();
    let png_len = png_data.len();
    let blocking_source = BlockOnLimit::new(png_data, png_len);

    let buf_reader = BufReader::new(blocking_source);
    let res = decode_image(buf_reader);

    assert!(res.is_err());
    let err = res.unwrap_err();
    assert!(matches!(
        err,
        png::DecodingError::IoError(ref io_err) if io_err.kind() == ErrorKind::WouldBlock
    ));
}

#[test]
fn test_limit_buf_reader_does_not_overshoot() {
    let png_data = fs::read("tests/pngsuite/basi0g01.png").unwrap();
    let mut concat_data = Vec::new();
    concat_data.extend_from_slice(&png_data);
    concat_data.extend_from_slice(&png_data);

    let mut cursor = Cursor::new(concat_data);
    let limited_reader = LimitBufReader::new(&mut cursor);
    decode_image(limited_reader).expect("LimitBufReader decode failed");

    assert_eq!(cursor.position(), png_data.len() as u64);
}

#[test]
fn test_limit_buf_reader_does_not_block_on_blocking_source() {
    let png_data = fs::read("tests/pngsuite/basi0g01.png").unwrap();
    let png_len = png_data.len();
    let blocking_source = BlockOnLimit::new(png_data, png_len);

    let limited_reader = LimitBufReader::new(blocking_source);
    decode_image(limited_reader).expect("LimitBufReader decode failed");
}
