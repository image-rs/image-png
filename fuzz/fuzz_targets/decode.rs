#![no_main]

use libfuzzer_sys::fuzz_target;
use png::Transformations;
use std::io::Cursor;

#[inline(always)]
fn png_decode(
    data: &[u8],
    transformations: Transformations,
) -> Result<(Option<png::OutputInfo>, Vec<u8>), ()> {
    let limits = png::Limits { bytes: 1 << 16 };
    let mut decoder = png::Decoder::new_with_limits(Cursor::new(data), limits);
    decoder.set_transformations(transformations);
    let mut reader = decoder.read_info().map_err(|_| ())?;

    let buffer_size = reader.output_buffer_size().ok_or(())?;
    if buffer_size > 5_000_000 {
        return Err(());
    }

    let mut img_data = vec![0u8; buffer_size];

    let mut last_info = None;
    while let Ok(info) = reader.next_frame(&mut img_data) {
        last_info = Some(info);
    }

    Ok((last_info, img_data))
}

fuzz_target!(|data: &[u8]| {
    // The last byte of the input selects one of the distinct combinations of the output
    // transformations (`ALPHA` implies `EXPAND`), so that the row transformations of
    // `decoder/transform` are fuzzed, not only `copy_row`. The byte stays in the input, so a PNG
    // file is a valid input as it is.
    let transformations = [
        Transformations::IDENTITY,
        Transformations::STRIP_16,
        Transformations::EXPAND,
        Transformations::EXPAND | Transformations::STRIP_16,
        Transformations::ALPHA,
        Transformations::ALPHA | Transformations::STRIP_16,
    ];
    let Some(&selector) = data.last() else {
        return;
    };
    let _ = png_decode(
        data,
        transformations[usize::from(selector) % transformations.len()],
    );
});
