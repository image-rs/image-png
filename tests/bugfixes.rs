use std::io::Cursor;
use std::{fs::File, io::BufReader};

use png::{DecodeOptions, Decoder, DecodingError};

#[test]
fn issue_430() {
    let file = File::open("tests/bugfixes/issue#430.png").unwrap();

    let mut decode_options = DecodeOptions::default();
    decode_options.set_skip_ancillary_crc_failures(false);

    let decoder = Decoder::new_with_options(BufReader::new(file), decode_options).read_info();

    assert!(
        matches!(decoder, Err(DecodingError::Format(_))),
        "Decoding of iCCP chunk with invalid CRC should have failed with 'skip_ancillary_crc' disabled."
    );
}

/// Ten decompressed rows, then `IEND` while the image header still expects forty.
/// The inflater reports the truncated stream only after those rows are in the
/// buffer. `next_row` used to return that error before yielding any of them.
#[test]
fn corrupt_idat_yields_rows_decoded_before_the_error() {
    const WIDTH: usize = 4;
    const HEIGHT: usize = 40;
    const GOOD_ROWS: usize = 10;
    let mut raw = Vec::with_capacity(GOOD_ROWS * (1 + WIDTH));
    for row in 0..GOOD_ROWS {
        raw.push(0);
        raw.extend(std::iter::repeat((row + 1) as u8).take(WIDTH));
    }

    // Sync flush publishes the rows and leaves the zlib stream unfinished, so
    // reaching `IEND` is a truncated stream rather than a clean end.
    let mut compress = flate2::Compress::new(flate2::Compression::none(), true);
    let mut good = vec![0u8; raw.len() + 64];
    let status = compress
        .compress(&raw, &mut good, flate2::FlushCompress::Sync)
        .unwrap();
    assert_eq!(status, flate2::Status::Ok);
    let produced = compress.total_out() as usize;
    good.truncate(produced);

    let mut ihdr = Vec::new();
    ihdr.extend_from_slice(&(WIDTH as u32).to_be_bytes());
    ihdr.extend_from_slice(&(HEIGHT as u32).to_be_bytes());
    ihdr.extend_from_slice(&[8, 0, 0, 0, 0]);

    let mut png_bytes = vec![137, 80, 78, 71, 13, 10, 26, 10];
    png_bytes.extend(png_chunk(b"IHDR", &ihdr));
    png_bytes.extend(png_chunk(b"IDAT", &good));
    png_bytes.extend(png_chunk(b"IEND", &[]));

    let mut reader = Decoder::new(Cursor::new(png_bytes)).read_info().unwrap();
    let mut rows = 0usize;
    let error = loop {
        match reader.next_row() {
            Ok(Some(row)) => {
                let expected = (rows + 1) as u8;
                assert!(row.data().iter().all(|byte| *byte == expected));
                rows += 1;
            }
            Ok(None) => panic!("corrupt image ended without an error after {rows} rows"),
            Err(error) => break error,
        }
    };
    assert_eq!(
        rows, GOOD_ROWS,
        "rows decompressed before the truncated stream were dropped; error was {error}"
    );
    assert!(matches!(error, DecodingError::Format(_)), "{error}");
}

fn png_chunk(kind: &[u8; 4], data: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&(data.len() as u32).to_be_bytes());
    out.extend_from_slice(kind);
    out.extend_from_slice(data);
    let mut hasher = crc32fast::Hasher::new();
    hasher.update(kind);
    hasher.update(data);
    out.extend_from_slice(&hasher.finalize().to_be_bytes());
    out
}
