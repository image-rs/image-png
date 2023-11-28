// SPDX-FileCopyrightText: 2023 Lukas Schattenhofer <lukas.schattenhofer@ose-germany.de>
// SPDX-License-Identifier: MIT OR Apache-2.0

/// Tests "editing"/re-encoding of an image:
/// decoding, editing, re-encoding

use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
pub type BoxResult<T> = Result<T, Box<dyn std::error::Error + Send + Sync>>;

fn main() -> BoxResult<()> {
    // # Decode
    // Read image called test.png in the target folder of the library
    let path = Path::new(r"./target/test.png");
    // The decoder is a build for reader and can be used to set various decoding options
    // via `Transformations`. The default output transformation is `Transformations::IDENTITY`.
    let decoder = png::Decoder::new(File::open(path)?);
    let mut reader = decoder.read_info()?;
    // Allocate the output buffer.
    let mut buf = vec![0; reader.output_buffer_size()];
    let png_info = reader.info();
    dbg!(png_info);

    // # Encode
    let path = Path::new(r"./target/test_modified.png");
    let file = File::create(path)?;
    let ref mut w = BufWriter::new(file);
    // With previous info
    let mut encoder = png::Encoder::new_with_info(w, png_info.clone());
    // Edit some attribute
    encoder.add_text_chunk(
        "Testing tEXt".to_string(),
        "This is a tEXt chunk that will appear before the IDAT chunks.".to_string(),
    )?;
    //encoder.set_color(png::ColorType::Rgba);
    //encoder.set_depth(png::BitDepth::Eight);
    //// 1.0 / 2.2, scaled by 100000
    //encoder.set_source_gamma(png::ScaledFloat::from_scaled(45455));
    //// 1.0 / 2.2, unscaled, but rounded
    //encoder.set_source_gamma(png::ScaledFloat::new(1.0 / 2.2));
    //// Using unscaled instantiation here
    //let source_chromaticities = png::SourceChromaticities::new(
    //(0.31270, 0.32900),
    //(0.64000, 0.33000),
    //(0.30000, 0.60000),
    //(0.15000, 0.06000)
    //);
    //encoder.set_source_chromaticities(source_chromaticities);

    // Save picture with changed info
    let mut writer = encoder.write_header()?;
    let mut counter = 0u8;
    while let Ok(info) = reader.next_frame(&mut buf) {
        let bytes = &buf[..info.buffer_size()];
        writer.write_image_data(&bytes)?;
        counter += 1;
        println!("Written frame: {}", counter);
    }
    Ok(())
}
