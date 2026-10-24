//! Transforming a decompressed, unfiltered row into the final output.

mod palette;

use crate::{BitDepth, ColorType, DecodingError, Info, Transformations};

use super::stream::FormatErrorInner;

/// Type of a function that can transform a decompressed, unfiltered row (the
/// 1st argument) into the final pixels (the 2nd argument), optionally using
/// image metadata (e.g. PLTE data can be accessed using the 3rd argument).
///
/// TODO: If some precomputed state is needed (e.g. to make `expand_paletted...`
/// faster) then consider changing this into `Box<dyn Fn(...)>`.
pub type TransformFn = Box<dyn Fn(&[u8], &mut [u8], &Info) + Send + Sync>;

/// Returns a transformation function that should be applied to image rows based
/// on 1) decoded image metadata (`info`) and 2) the transformations requested
/// by the crate client (`transform`).
pub fn create_transform_fn(
    info: &Info,
    transform: Transformations,
) -> Result<TransformFn, DecodingError> {
    let color_type = info.color_type;
    let bit_depth = info.bit_depth as u8;
    let trns = info.trns.is_some() || transform.contains(Transformations::ALPHA);
    let expand =
        transform.contains(Transformations::EXPAND) || transform.contains(Transformations::ALPHA);
    let strip16 = bit_depth == 16 && transform.contains(Transformations::STRIP_16);
    match color_type {
        ColorType::Indexed if expand => {
            if info.palette.is_none() {
                Err(DecodingError::Format(
                    FormatErrorInner::PaletteRequired.into(),
                ))
            } else if let BitDepth::Sixteen = info.bit_depth {
                // This should have been caught earlier but let's check again. Can't hurt.
                Err(DecodingError::Format(
                    FormatErrorInner::InvalidColorBitDepth {
                        color_type: ColorType::Indexed,
                        bit_depth: BitDepth::Sixteen,
                    }
                    .into(),
                ))
            } else {
                Ok(if trns {
                    palette::create_expansion_into_rgba8(info)
                } else {
                    palette::create_expansion_into_rgb8(info)
                })
            }
        }
        ColorType::Grayscale | ColorType::GrayscaleAlpha if bit_depth < 8 && expand => {
            Ok(Box::new(if trns {
                expand_gray_u8_with_trns
            } else {
                expand_gray_u8
            }))
        }
        ColorType::Grayscale | ColorType::Rgb if expand && trns => {
            Ok(Box::new(if bit_depth == 8 {
                if color_type == ColorType::Rgb {
                    expand_trns_line::<3>
                } else {
                    expand_trns_line::<1>
                }
            } else if strip16 {
                expand_trns_and_strip_line16
            } else {
                assert_eq!(bit_depth, 16);
                expand_trns_line16
            }))
        }
        ColorType::Grayscale | ColorType::GrayscaleAlpha | ColorType::Rgb | ColorType::Rgba
            if strip16 =>
        {
            Ok(Box::new(transform_row_strip16))
        }
        _ => Ok(Box::new(copy_row)),
    }
}

fn copy_row(row: &[u8], output_buffer: &mut [u8], _: &Info) {
    output_buffer.copy_from_slice(row);
}

fn transform_row_strip16(row: &[u8], output_buffer: &mut [u8], _: &Info) {
    for i in 0..row.len() / 2 {
        output_buffer[i] = row[2 * i];
    }
}

#[inline(always)]
fn unpack_bits<F>(input: &[u8], output: &mut [u8], channels: usize, bit_depth: u8, func: F)
where
    F: Fn(u8, &mut [u8]),
{
    // Only [1, 2, 4, 8] are valid bit depths
    assert!(matches!(bit_depth, 1 | 2 | 4 | 8));
    // Check that `input` is capable of producing a buffer as long as `output`:
    // number of shift lookups per bit depth * channels * input length
    assert!((8 / bit_depth as usize * channels).saturating_mul(input.len()) >= output.len());

    let mut buf_chunks = output.chunks_exact_mut(channels);
    let mut iter = input.iter();

    // `shift` iterates through the corresponding bit depth sequence:
    // 1 => &[7, 6, 5, 4, 3, 2, 1, 0],
    // 2 => &[6, 4, 2, 0],
    // 4 => &[4, 0],
    // 8 => &[0],
    //
    // `(0..8).step_by(bit_depth.into()).rev()` doesn't always optimize well so
    // shifts are calculated instead. (2023-08, Rust 1.71)

    if bit_depth == 8 {
        for (&curr, chunk) in iter.zip(&mut buf_chunks) {
            func(curr, chunk);
        }
    } else {
        let mask = ((1u16 << bit_depth) - 1) as u8;

        // These variables are initialized in the loop
        let mut shift = -1;
        let mut curr = 0;

        for chunk in buf_chunks {
            if shift < 0 {
                shift = 8 - bit_depth as i32;
                curr = *iter.next().expect("input for unpack bits is not empty");
            }

            let pixel = (curr >> shift) & mask;
            func(pixel, chunk);

            shift -= bit_depth as i32;
        }
    }
}

/// `N` is the number of 8-bit samples per pixel (1 for gray, 3 for RGB). With fixed-size
/// pixels, the per-pixel copy and key comparison compile to inline moves and compares
/// instead of `memcpy` and `memcmp` calls; for `N = 1` the loop also vectorizes.
fn expand_trns_line<const N: usize>(input: &[u8], output: &mut [u8], info: &Info) {
    let trns: Option<[u8; N]> = info.trns.as_deref().and_then(|t| t.try_into().ok());
    for (input, output) in input.chunks_exact(N).zip(output.chunks_exact_mut(N + 1)) {
        let pixel: [u8; N] = input.try_into().unwrap();
        output[..N].copy_from_slice(&pixel);
        output[N] = if Some(pixel) == trns { 0 } else { 0xFF };
    }
}

fn expand_trns_line16(input: &[u8], output: &mut [u8], info: &Info) {
    let channels = info.color_type.samples();
    let trns = info.trns.as_deref();
    for (input, output) in input
        .chunks_exact(channels * 2)
        .zip(output.chunks_exact_mut(channels * 2 + 2))
    {
        output[..channels * 2].copy_from_slice(input);
        if Some(input) == trns {
            output[channels * 2] = 0;
            output[channels * 2 + 1] = 0
        } else {
            output[channels * 2] = 0xFF;
            output[channels * 2 + 1] = 0xFF
        };
    }
}

fn expand_trns_and_strip_line16(input: &[u8], output: &mut [u8], info: &Info) {
    let channels = info.color_type.samples();
    let trns = info.trns.as_deref();
    for (input, output) in input
        .chunks_exact(channels * 2)
        .zip(output.chunks_exact_mut(channels + 1))
    {
        for i in 0..channels {
            output[i] = input[i * 2];
        }
        output[channels] = if Some(input) == trns { 0 } else { 0xFF };
    }
}

fn expand_gray_u8(row: &[u8], buffer: &mut [u8], info: &Info) {
    let scaling_factor = (255) / ((1u16 << info.bit_depth as u8) - 1) as u8;
    unpack_bits(row, buffer, 1, info.bit_depth as u8, |val, chunk| {
        chunk[0] = val * scaling_factor
    });
}

fn expand_gray_u8_with_trns(row: &[u8], buffer: &mut [u8], info: &Info) {
    let scaling_factor = (255) / ((1u16 << info.bit_depth as u8) - 1) as u8;
    let trns = info.trns.as_deref();
    unpack_bits(row, buffer, 2, info.bit_depth as u8, |pixel, chunk| {
        chunk[1] = if let Some(trns) = trns {
            if pixel == trns[0] {
                0
            } else {
                0xFF
            }
        } else {
            0xFF
        };
        chunk[0] = pixel * scaling_factor
    });
}

#[cfg(test)]
mod test {
    use crate::{BitDepth, ColorType, Info, Transformations};

    /// The previous, channel-generic version is used as a test oracle.
    fn oracle_expand_trns_line(input: &[u8], output: &mut [u8], info: &Info) {
        let channels = info.color_type.samples();
        let trns = info.trns.as_deref();
        for (input, output) in input
            .chunks_exact(channels)
            .zip(output.chunks_exact_mut(channels + 1))
        {
            output[..channels].copy_from_slice(input);
            output[channels] = if Some(input) == trns { 0 } else { 0xFF };
        }
    }

    #[test]
    fn expand_trns_line_matches_oracle() {
        for color_type in [ColorType::Grayscale, ColorType::Rgb] {
            let samples = color_type.samples();
            let key = vec![0x42; samples];
            for trns in [None, Some(&key[..])] {
                let info = Info {
                    color_type,
                    bit_depth: BitDepth::Eight,
                    trns: trns.map(Into::into),
                    ..Info::default()
                };
                let transform_fn =
                    super::create_transform_fn(&info, Transformations::ALPHA).unwrap();
                for width in [0usize, 1, 2, 3, 7, 16, 17, 64, 257] {
                    let mut row: Vec<u8> = (0..width * samples).map(|i| (i * 37) as u8).collect();
                    // Every third pixel is the tRNS key, so both alpha values are written.
                    for pixel in row.chunks_exact_mut(samples).step_by(3) {
                        pixel.copy_from_slice(&key);
                    }
                    let mut expected = vec![0u8; width * (samples + 1)];
                    oracle_expand_trns_line(&row, &mut expected, &info);
                    let mut actual = vec![0u8; width * (samples + 1)];
                    transform_fn(&row, &mut actual, &info);
                    assert_eq!(
                        actual, expected,
                        "{color_type:?} trns={trns:?} width={width}"
                    );
                }
            }
        }
    }
}
