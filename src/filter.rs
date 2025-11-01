use core::convert::TryInto;

use crate::{common::BytesPerPixel, Compression};

/// The byte level filter applied to scanlines to prepare them for compression.
///
/// Compression in general benefits from repetitive data. The filter is a content-aware method of
/// compressing the range of occurring byte values to help the compression algorithm. Note that
/// this does not operate on pixels but on raw bytes of a scanline.
///
/// Details on how each filter works can be found in the [PNG Book](http://www.libpng.org/pub/png/book/chapter09.html).
///
/// The default filter is `Adaptive`, which uses heuristics to select the best filter for every row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Filter {
    NoFilter,
    Sub,
    Up,
    Avg,
    Paeth,
    Adaptive,
    MinEntropy,
}

impl Default for Filter {
    fn default() -> Self {
        Filter::Adaptive
    }
}

impl From<RowFilter> for Filter {
    fn from(value: RowFilter) -> Self {
        match value {
            RowFilter::NoFilter => Filter::NoFilter,
            RowFilter::Sub => Filter::Sub,
            RowFilter::Up => Filter::Up,
            RowFilter::Avg => Filter::Avg,
            RowFilter::Paeth => Filter::Paeth,
        }
    }
}

impl Filter {
    pub(crate) fn from_simple(compression: Compression) -> Self {
        match compression {
            Compression::NoCompression => Filter::NoFilter, // with no DEFLATE filtering would only waste time
            Compression::Fastest => Filter::Up, // pairs well with FdeflateUltraFast, producing much smaller files while being very fast
            Compression::Fast => Filter::Adaptive,
            Compression::Balanced => Filter::Adaptive,
            Compression::High => Filter::Adaptive,
        }
    }
}

/// Unlike the public [Filter], does not include the "Adaptive" option
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum RowFilter {
    NoFilter = 0,
    Sub = 1,
    Up = 2,
    Avg = 3,
    Paeth = 4,
}

impl Default for RowFilter {
    fn default() -> Self {
        RowFilter::Up
    }
}

impl RowFilter {
    pub fn from_u8(n: u8) -> Option<Self> {
        match n {
            0 => Some(Self::NoFilter),
            1 => Some(Self::Sub),
            2 => Some(Self::Up),
            3 => Some(Self::Avg),
            4 => Some(Self::Paeth),
            _ => None,
        }
    }

    pub fn from_method(strat: Filter) -> Option<Self> {
        match strat {
            Filter::NoFilter => Some(Self::NoFilter),
            Filter::Sub => Some(Self::Sub),
            Filter::Up => Some(Self::Up),
            Filter::Avg => Some(Self::Avg),
            Filter::Paeth => Some(Self::Paeth),
            Filter::Adaptive | Filter::MinEntropy => None,
        }
    }
}

/// Optional module containing `portable_simd` versions of the most
/// important unfiltering algorithms. Enable using the `unstable` feature.
#[cfg(feature = "unstable")]
mod simd {
    use core::convert::TryInto;
    use core::simd::cmp::{SimdOrd, SimdPartialOrd};
    use core::simd::num::{SimdInt, SimdUint};
    use core::simd::{LaneCount, Simd, SupportedLaneCount};

    // Import the fastest arch-specific scalar implementations from the outer crate.
    #[cfg(not(target_arch = "x86_64"))]
    use crate::filter::filter_paeth as filter_paeth_chosen;
    #[cfg(target_arch = "x86_64")]
    use crate::filter::filter_paeth_stbi as filter_paeth_chosen;

    /// Paeth predictor specialized for AArch64 systems. Ported from the libpng
    /// implementation at
    /// https://github.com/pnggroup/libpng/blob/master/arm/filter_neon_intrinsics.c
    #[cfg(target_arch = "aarch64")]
    #[inline(always)]
    fn paeth_predictor_simd<const SIZE: usize>(
        a_i16: Simd<i16, SIZE>,
        b_i16: Simd<i16, SIZE>,
        c_i16: Simd<i16, SIZE>,
    ) -> Simd<u8, SIZE>
    where
        LaneCount<SIZE>: SupportedLaneCount,
    {
        let pa = (b_i16 - c_i16).abs();
        let pb = (a_i16 - c_i16).abs();
        let pc = (a_i16 + b_i16 - c_i16 - c_i16).abs();

        let mut nearest = a_i16;
        let mut min_dist = pa;

        // Tie-breaking: left, then above, then upper-left.
        let pb_lt_min = pb.simd_lt(min_dist);
        nearest = pb_lt_min.select(b_i16, nearest);
        min_dist = pb_lt_min.select(pb, min_dist);

        let pc_lt_min = pc.simd_lt(min_dist);
        nearest = pc_lt_min.select(c_i16, nearest);

        nearest.cast::<u8>()
    }

    /// Paeth predictor based on the `filter_paeth_stbi` formulation, which
    /// performs better on x86_64 systems.
    #[cfg(not(target_arch = "aarch64"))]
    #[inline(always)]
    fn paeth_predictor_simd<const SIZE: usize>(
        a_i16: Simd<i16, SIZE>,
        b_i16: Simd<i16, SIZE>,
        c_i16: Simd<i16, SIZE>,
    ) -> Simd<u8, SIZE>
    where
        LaneCount<SIZE>: SupportedLaneCount,
    {
        let thresh = c_i16 * Simd::splat(3) - (a_i16 + b_i16);
        let lo = a_i16.simd_min(b_i16);
        let hi = a_i16.simd_max(b_i16);
        let t0 = hi.simd_le(thresh).select(lo, c_i16);
        thresh.simd_le(lo).select(hi, t0).cast::<u8>()
    }

    /// Core kernel for 3bpp Paeth unfiltering. Takes a `b_vec` consisting of
    /// 16 RGB above the starting offset, a `c_vec_initial` which is the
    /// previous pixel from before the starting offset, and a `current_a`
    /// which is the previous pixel to the left of `x_out`, which is the 16
    /// pixels we're unfiltering. Returns the next `a` pixel and also updates
    /// `x_out`.
    #[inline(always)]
    fn process_paeth_chunk_bpp3_s48(
        // Previous `a` pixel from the last pump, or zero if this is the first.
        mut current_a: Simd<u8, 3>,
        // 48 bytes from the row above.
        b_vec: &Simd<u8, 48>,
        // The last three bytes of the previous `b_vec`, or zero if this if the first.
        c_vec_initial: Simd<u8, 3>,
        // 48 bytes from the current row (filtered) also re-used as the output.
        x_out: &mut Simd<u8, 48>,
    ) -> Simd<u8, 3> {
        let x_in = *x_out;

        // Store the outputs of the caluation as we go along, then we can do a
        // a wide vectorized add at the end of the loop.
        let mut preds = [0u8; 48];

        // Shift b and sift in the lowest 3 elements from the previous pump
        // to form c.
        let mut c_vec = b_vec.shift_elements_right::<3>(0u8);
        c_vec.as_mut_array()[0..3].copy_from_slice(c_vec_initial.as_array());

        // For each RGB pixel in the 48-byte window, we a) extract the relevant
        // parts of a, b, c and input x, then b) apply the paeth predictor to
        // what's extracted, then c) merge them to form the 'a' pixel for the
        // next part of the calculation, and update the predictpr buffer.
        macro_rules! process_pixel {
            ($shift:expr) => {
                let a_i16 = current_a.cast::<i16>();
                let b_i16 = b_vec.extract::<$shift, 3>().cast::<i16>();
                let c_i16 = c_vec.extract::<$shift, 3>().cast::<i16>();
                let pred = paeth_predictor_simd(a_i16, b_i16, c_i16);
                current_a = x_in.extract::<$shift, 3>() + pred;
                // This is necessary to break the data dependency between the
                // output and the next pixel as much as possible.
                preds[$shift..$shift + 3].copy_from_slice(pred.as_array());
            };
        }

        process_pixel!(0);
        process_pixel!(3);
        process_pixel!(6);
        process_pixel!(9);
        process_pixel!(12);
        process_pixel!(15);
        process_pixel!(18);
        process_pixel!(21);
        process_pixel!(24);
        process_pixel!(27);
        process_pixel!(30);
        process_pixel!(33);
        process_pixel!(36);
        process_pixel!(39);
        process_pixel!(42);
        process_pixel!(45);

        // Commit the unfiltered result and return the next 'a' pixel.
        *x_out += Simd::from_array(preds);
        current_a
    }

    /// Applies Paeth unfiltering on the `current` pixel row using `prev` row,
    /// interpreting the input data as RGB.
    pub fn paeth_unfilter_3bpp(current: &mut [u8], prev: &[u8]) {
        const BPP: usize = 3;
        const STRIDE_BYTES: usize = 48; // 16 pixels * 3 bytes/pixel.

        // Use the standard convention of [c] [b]
        //                                [a] [x]
        // We load 48 bytes of each and use a sliding window approach to minimize loads/stores.
        // Whilst we cannot break the strict data dependency on [a], we can agressively unroll
        // the calculation and allow independent computation of the `pa` prediction variables.
        // Initially set these to zero.
        let mut a: Simd<u8, BPP> = Default::default(); // Left pixel (unfiltered)
        let mut c: Simd<u8, BPP> = Default::default(); // Upper-left pixel (unfiltered)

        // Decide the number of chunks and setup iterators for the SIMD body and scalar fallback.
        let mut current_iter = current.chunks_exact_mut(STRIDE_BYTES);
        let mut previous_iter = prev.chunks_exact(STRIDE_BYTES);
        let combined_iter = (&mut current_iter).zip(&mut previous_iter);

        for (chunk, prev_chunk) in combined_iter {
            let mut x: Simd<u8, STRIDE_BYTES> = Simd::<u8, STRIDE_BYTES>::from_slice(chunk);
            let b: Simd<u8, STRIDE_BYTES> = Simd::<u8, STRIDE_BYTES>::from_slice(prev_chunk);

            // Process the chunk using the SIMD helper, passing the initial `c`.
            a = process_paeth_chunk_bpp3_s48(a, &b, c, &mut x);

            // Update `c` for the next chunk: it's the upper-left of the next   chunk,
            // which corresponds to the upper-right of the current chunk's `b` vector.
            c = b.extract::<{ STRIDE_BYTES - BPP }, BPP>();

            // `TryInto` and `copy_to_slice` have similar performance here.
            x.copy_to_slice(chunk);
        }

        // Scalar remainder.
        let mut a_bpp = a.to_array();
        let mut c_bpp = c.to_array();
        for (chunk, b_bpp) in current_iter
            .into_remainder()
            .chunks_exact_mut(BPP)
            .zip(previous_iter.remainder().chunks_exact(BPP))
        {
            let new_chunk = [
                chunk[0].wrapping_add(filter_paeth_chosen(
                    a_bpp[0].into(),
                    b_bpp[0].into(),
                    c_bpp[0].into(),
                )),
                chunk[1].wrapping_add(filter_paeth_chosen(
                    a_bpp[1].into(),
                    b_bpp[1].into(),
                    c_bpp[1].into(),
                )),
                chunk[2].wrapping_add(filter_paeth_chosen(
                    a_bpp[2].into(),
                    b_bpp[2].into(),
                    c_bpp[2].into(),
                )),
            ];
            *TryInto::<&mut [u8; BPP]>::try_into(chunk).unwrap() = new_chunk;
            a_bpp = new_chunk;
            c_bpp = b_bpp.try_into().unwrap();
        }
    }
}

// This code path is used on non-x86_64 architectures but we allow dead code
// for the test module to be able to access it.
#[allow(dead_code)]
fn filter_paeth(a: u8, b: u8, c: u8) -> u8 {
    // On ARM this algorithm performs much better than the one above adapted from stb,
    // and this is the better-studied algorithm we've always used here,
    // so we default to it on all non-x86 platforms.
    let pa = (i16::from(b) - i16::from(c)).abs();
    let pb = (i16::from(a) - i16::from(c)).abs();
    let pc = ((i16::from(a) - i16::from(c)) + (i16::from(b) - i16::from(c))).abs();

    let mut out = a;
    let mut min = pa;

    if pb < min {
        min = pb;
        out = b;
    }
    if pc < min {
        out = c;
    }

    out
}

fn filter_paeth_stbi(a: i16, b: i16, c: i16) -> u8 {
    // Decoding optimizes better with this algorithm than with `filter_paeth`
    //
    // This formulation looks very different from the reference in the PNG spec, but is
    // actually equivalent and has favorable data dependencies and admits straightforward
    // generation of branch-free code, which helps performance significantly.
    //
    // Adapted from public domain PNG implementation:
    // https://github.com/nothings/stb/blob/5c205738c191bcb0abc65c4febfa9bd25ff35234/stb_image.h#L4657-L4668
    let thresh = c * 3 - (a + b);
    let lo = a.min(b);
    let hi = a.max(b);
    let t0 = if hi <= thresh { lo } else { c };
    let t1 = if thresh <= lo { hi } else { t0 };
    t1 as u8
}

fn filter_paeth_fpnge(a: u8, b: u8, c: u8) -> u8 {
    // This is an optimized version of the paeth filter from the PNG specification, proposed by
    // Luca Versari for [FPNGE](https://www.lucaversari.it/FJXL_and_FPNGE.pdf). It operates
    // entirely on unsigned 8-bit quantities, making it more conducive to vectorization.
    //
    //     p = a + b - c
    //     pa = |p - a| = |a + b - c - a| = |b - c| = max(b, c) - min(b, c)
    //     pb = |p - b| = |a + b - c - b| = |a - c| = max(a, c) - min(a, c)
    //     pc = |p - c| = |a + b - c - c| = |(b - c) + (a - c)| = ...
    //
    // Further optimizing the calculation of `pc` a bit tricker. However, notice that:
    //
    //        a > c && b > c
    //    ==> (a - c) > 0 && (b - c) > 0
    //    ==> pc > (a - c) && pc > (b - c)
    //    ==> pc > |a - c| && pc > |b - c|
    //    ==> pc > pb && pc > pa
    //
    // Meaning that if `c` is smaller than `a` and `b`, the value of `pc` is irrelevant. Similar
    // reasoning applies if `c` is larger than the other two inputs. Assuming that `c >= b` and
    // `c <= b` or vice versa:
    //
    //     pc = ||b - c| - |a - c|| =  |pa - pb| = max(pa, pb) - min(pa, pb)
    //
    let pa = b.max(c) - c.min(b);
    let pb = a.max(c) - c.min(a);
    let pc = if (a < c) == (c < b) {
        pa.max(pb) - pa.min(pb)
    } else {
        255
    };

    if pa <= pb && pa <= pc {
        a
    } else if pb <= pc {
        b
    } else {
        c
    }
}

pub(crate) fn unfilter(
    mut filter: RowFilter,
    tbpp: BytesPerPixel,
    previous: &[u8],
    current: &mut [u8],
) {
    use self::RowFilter::*;

    // If the previous row is empty, then treat it as if it were filled with zeros.
    if previous.is_empty() {
        if filter == Paeth {
            filter = Sub;
        } else if filter == Up {
            filter = NoFilter;
        }
    }

    // Auto-vectorization notes
    // ========================
    //
    // [2023/01 @okaneco] - Notes on optimizing decoding filters
    //
    // Links:
    // [PR]: https://github.com/image-rs/image-png/pull/382
    // [SWAR]: http://aggregate.org/SWAR/over.html
    // [AVG]: http://aggregate.org/MAGIC/#Average%20of%20Integers
    //
    // #382 heavily refactored and optimized the following filters making the
    // implementation nonobvious. These comments function as a summary of that
    // PR with an explanation of the choices made below.
    //
    // #382 originally started with trying to optimize using a technique called
    // SWAR, SIMD Within a Register. SWAR uses regular integer types like `u32`
    // and `u64` as SIMD registers to perform vertical operations in parallel,
    // usually involving bit-twiddling. This allowed each `BytesPerPixel` (bpp)
    // pixel to be decoded in parallel: 3bpp and 4bpp in a `u32`, 6bpp and 8pp
    // in a `u64`. The `Sub` filter looked like the following code block, `Avg`
    // was similar but used a bitwise average method from [AVG]:
    // ```
    // // See "Unpartitioned Operations With Correction Code" from [SWAR]
    // fn swar_add_u32(x: u32, y: u32) -> u32 {
    //     // 7-bit addition so there's no carry over the most significant bit
    //     let n = (x & 0x7f7f7f7f) + (y & 0x7f7f7f7f); // 0x7F = 0b_0111_1111
    //     // 1-bit parity/XOR addition to fill in the missing MSB
    //     n ^ (x ^ y) & 0x80808080                     // 0x80 = 0b_1000_0000
    // }
    //
    // let mut prev =
    //     u32::from_ne_bytes([current[0], current[1], current[2], current[3]]);
    // for chunk in current[4..].chunks_exact_mut(4) {
    //     let cur = u32::from_ne_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
    //     let new_chunk = swar_add_u32(cur, prev);
    //     chunk.copy_from_slice(&new_chunk.to_ne_bytes());
    //     prev = new_chunk;
    // }
    // ```
    // While this provided a measurable increase, @fintelia found that this idea
    // could be taken even further by unrolling the chunks component-wise and
    // avoiding unnecessary byte-shuffling by using byte arrays instead of
    // `u32::from|to_ne_bytes`. The bitwise operations were no longer necessary
    // so they were reverted to their obvious arithmetic equivalent. Lastly,
    // `TryInto` was used instead of `copy_from_slice`. The `Sub` code now
    // looked like this (with asserts to remove `0..bpp` bounds checks):
    // ```
    // assert!(len > 3);
    // let mut prev = [current[0], current[1], current[2], current[3]];
    // for chunk in current[4..].chunks_exact_mut(4) {
    //     let new_chunk = [
    //         chunk[0].wrapping_add(prev[0]),
    //         chunk[1].wrapping_add(prev[1]),
    //         chunk[2].wrapping_add(prev[2]),
    //         chunk[3].wrapping_add(prev[3]),
    //     ];
    //     *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
    //     prev = new_chunk;
    // }
    // ```
    // The compiler was able to optimize the code to be even faster and this
    // method even sped up Paeth filtering! Assertions were experimentally
    // added within loop bodies which produced better instructions but no
    // difference in speed. Finally, the code was refactored to remove manual
    // slicing and start the previous pixel chunks with arrays of `[0; N]`.
    // ```
    // let mut prev = [0; 4];
    // for chunk in current.chunks_exact_mut(4) {
    //     let new_chunk = [
    //         chunk[0].wrapping_add(prev[0]),
    //         chunk[1].wrapping_add(prev[1]),
    //         chunk[2].wrapping_add(prev[2]),
    //         chunk[3].wrapping_add(prev[3]),
    //     ];
    //     *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
    //     prev = new_chunk;
    // }
    // ```
    // While we're not manually bit-twiddling anymore, a possible takeaway from
    // this is to "think in SWAR" when dealing with small byte arrays. Unrolling
    // array operations and performing them component-wise may unlock previously
    // unavailable optimizations from the compiler, even when using the
    // `chunks_exact` methods for their potential auto-vectorization benefits.
    //
    // `std::simd` notes
    // =================
    //
    // In the past we have experimented with `std::simd` for unfiltering.  This
    // experiment was removed in https://github.com/image-rs/image-png/pull/585
    // because:
    //
    // * The crate's microbenchmarks showed that `std::simd` didn't have a
    //   significant advantage over auto-vectorization for most filters, except
    //   for Paeth unfiltering - see
    //   https://github.com/image-rs/image-png/pull/414#issuecomment-1736655668
    // * In the crate's microbenchmarks `std::simd` seemed to help with Paeth
    //   unfiltering only on x86/x64, with mixed results on ARM - see
    //   https://github.com/image-rs/image-png/pull/539#issuecomment-2512748043
    // * In Chromium end-to-end microbenchmarks `std::simd` either didn't help
    //   or resulted in a small regression (as measured on x64).  See
    //   https://crrev.com/c/6090592.
    // * Field trial data from some "real world" scenarios shows that
    //   performance can be quite good without relying on `std::simd` - see
    //   https://github.com/image-rs/image-png/discussions/562#discussioncomment-13303307
    match filter {
        NoFilter => {}
        Sub => match tbpp {
            BytesPerPixel::One => {
                current.iter_mut().reduce(|&mut prev, curr| {
                    *curr = curr.wrapping_add(prev);
                    curr
                });
            }
            BytesPerPixel::Two => {
                let mut prev = [0; 2];
                for chunk in current.chunks_exact_mut(2) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                    ];
                    *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Three => {
                let mut prev = [0; 3];
                for chunk in current.chunks_exact_mut(3) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                    ];
                    *TryInto::<&mut [u8; 3]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Four => {
                let mut prev = [0; 4];
                for chunk in current.chunks_exact_mut(4) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                        chunk[3].wrapping_add(prev[3]),
                    ];
                    *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Six => {
                let mut prev = [0; 6];
                for chunk in current.chunks_exact_mut(6) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                        chunk[3].wrapping_add(prev[3]),
                        chunk[4].wrapping_add(prev[4]),
                        chunk[5].wrapping_add(prev[5]),
                    ];
                    *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Eight => {
                let mut prev = [0; 8];
                for chunk in current.chunks_exact_mut(8) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0]),
                        chunk[1].wrapping_add(prev[1]),
                        chunk[2].wrapping_add(prev[2]),
                        chunk[3].wrapping_add(prev[3]),
                        chunk[4].wrapping_add(prev[4]),
                        chunk[5].wrapping_add(prev[5]),
                        chunk[6].wrapping_add(prev[6]),
                        chunk[7].wrapping_add(prev[7]),
                    ];
                    *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
        },
        Up => {
            for (curr, &above) in current.iter_mut().zip(previous) {
                *curr = curr.wrapping_add(above);
            }
        }
        Avg if previous.is_empty() => match tbpp {
            BytesPerPixel::One => {
                current.iter_mut().reduce(|&mut prev, curr| {
                    *curr = curr.wrapping_add(prev / 2);
                    curr
                });
            }
            BytesPerPixel::Two => {
                let mut prev = [0; 2];
                for chunk in current.chunks_exact_mut(2) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0] / 2),
                        chunk[1].wrapping_add(prev[1] / 2),
                    ];
                    *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Three => {
                let mut prev = [0; 3];
                for chunk in current.chunks_exact_mut(3) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0] / 2),
                        chunk[1].wrapping_add(prev[1] / 2),
                        chunk[2].wrapping_add(prev[2] / 2),
                    ];
                    *TryInto::<&mut [u8; 3]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Four => {
                let mut prev = [0; 4];
                for chunk in current.chunks_exact_mut(4) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0] / 2),
                        chunk[1].wrapping_add(prev[1] / 2),
                        chunk[2].wrapping_add(prev[2] / 2),
                        chunk[3].wrapping_add(prev[3] / 2),
                    ];
                    *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Six => {
                let mut prev = [0; 6];
                for chunk in current.chunks_exact_mut(6) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0] / 2),
                        chunk[1].wrapping_add(prev[1] / 2),
                        chunk[2].wrapping_add(prev[2] / 2),
                        chunk[3].wrapping_add(prev[3] / 2),
                        chunk[4].wrapping_add(prev[4] / 2),
                        chunk[5].wrapping_add(prev[5] / 2),
                    ];
                    *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
            BytesPerPixel::Eight => {
                let mut prev = [0; 8];
                for chunk in current.chunks_exact_mut(8) {
                    let new_chunk = [
                        chunk[0].wrapping_add(prev[0] / 2),
                        chunk[1].wrapping_add(prev[1] / 2),
                        chunk[2].wrapping_add(prev[2] / 2),
                        chunk[3].wrapping_add(prev[3] / 2),
                        chunk[4].wrapping_add(prev[4] / 2),
                        chunk[5].wrapping_add(prev[5] / 2),
                        chunk[6].wrapping_add(prev[6] / 2),
                        chunk[7].wrapping_add(prev[7] / 2),
                    ];
                    *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                    prev = new_chunk;
                }
            }
        },
        Avg => match tbpp {
            BytesPerPixel::One => {
                let mut lprev = [0; 1];
                for (chunk, above) in current.chunks_exact_mut(1).zip(previous.chunks_exact(1)) {
                    let new_chunk =
                        [chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8)];
                    *TryInto::<&mut [u8; 1]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Two => {
                let mut lprev = [0; 2];
                for (chunk, above) in current.chunks_exact_mut(2).zip(previous.chunks_exact(2)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Three => {
                let mut lprev = [0; 3];
                for (chunk, above) in current.chunks_exact_mut(3).zip(previous.chunks_exact(3)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 3]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Four => {
                let mut lprev = [0; 4];
                for (chunk, above) in current.chunks_exact_mut(4).zip(previous.chunks_exact(4)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                        chunk[3].wrapping_add(((above[3] as u16 + lprev[3] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 4]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Six => {
                let mut lprev = [0; 6];
                for (chunk, above) in current.chunks_exact_mut(6).zip(previous.chunks_exact(6)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                        chunk[3].wrapping_add(((above[3] as u16 + lprev[3] as u16) / 2) as u8),
                        chunk[4].wrapping_add(((above[4] as u16 + lprev[4] as u16) / 2) as u8),
                        chunk[5].wrapping_add(((above[5] as u16 + lprev[5] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
            BytesPerPixel::Eight => {
                let mut lprev = [0; 8];
                for (chunk, above) in current.chunks_exact_mut(8).zip(previous.chunks_exact(8)) {
                    let new_chunk = [
                        chunk[0].wrapping_add(((above[0] as u16 + lprev[0] as u16) / 2) as u8),
                        chunk[1].wrapping_add(((above[1] as u16 + lprev[1] as u16) / 2) as u8),
                        chunk[2].wrapping_add(((above[2] as u16 + lprev[2] as u16) / 2) as u8),
                        chunk[3].wrapping_add(((above[3] as u16 + lprev[3] as u16) / 2) as u8),
                        chunk[4].wrapping_add(((above[4] as u16 + lprev[4] as u16) / 2) as u8),
                        chunk[5].wrapping_add(((above[5] as u16 + lprev[5] as u16) / 2) as u8),
                        chunk[6].wrapping_add(((above[6] as u16 + lprev[6] as u16) / 2) as u8),
                        chunk[7].wrapping_add(((above[7] as u16 + lprev[7] as u16) / 2) as u8),
                    ];
                    *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                    lprev = new_chunk;
                }
            }
        },
        #[allow(unreachable_code)]
        Paeth => {
            #[cfg(not(target_arch = "x86_64"))]
            {
                // Paeth filter pixels:
                // C B D
                // A X
                match tbpp {
                    BytesPerPixel::One => {
                        let mut a_bpp = [0; 1];
                        let mut c_bpp = [0; 1];
                        for (chunk, b_bpp) in
                            current.chunks_exact_mut(1).zip(previous.chunks_exact(1))
                        {
                            let new_chunk = [
                                chunk[0].wrapping_add(filter_paeth(a_bpp[0], b_bpp[0], c_bpp[0]))
                            ];
                            *TryInto::<&mut [u8; 1]>::try_into(chunk).unwrap() = new_chunk;
                            a_bpp = new_chunk;
                            c_bpp = b_bpp.try_into().unwrap();
                        }
                    }
                    BytesPerPixel::Two => {
                        let mut a_bpp = [0; 2];
                        let mut c_bpp = [0; 2];
                        for (chunk, b_bpp) in
                            current.chunks_exact_mut(2).zip(previous.chunks_exact(2))
                        {
                            let new_chunk = [
                                chunk[0].wrapping_add(filter_paeth(a_bpp[0], b_bpp[0], c_bpp[0])),
                                chunk[1].wrapping_add(filter_paeth(a_bpp[1], b_bpp[1], c_bpp[1])),
                            ];
                            *TryInto::<&mut [u8; 2]>::try_into(chunk).unwrap() = new_chunk;
                            a_bpp = new_chunk;
                            c_bpp = b_bpp.try_into().unwrap();
                        }
                    }
                    BytesPerPixel::Three => {
                        #[cfg(all(feature = "unstable", not(target_vendor = "apple")))]
                        {
                            // Results in PR: https://github.com/image-rs/image-png/pull/63
                            // Approximately 30% better on Arm Cortex A520, 7%
                            // regression on Arm Cortex X4. Switched off on Apple
                            // Silicon due to 10-12% regression.
                            simd::paeth_unfilter_3bpp(current, previous);
                            return;
                        }
                        let mut a_bpp = [0; 3];
                        let mut c_bpp = [0; 3];

                        let mut previous = &previous[..previous.len() / 3 * 3];
                        let current_len = current.len();
                        let mut current = &mut current[..current_len / 3 * 3];

                        while let ([c0, c1, c2, c_rest @ ..], [p0, p1, p2, p_rest @ ..]) =
                            (current, previous)
                        {
                            current = c_rest;
                            previous = p_rest;

                            *c0 = c0.wrapping_add(filter_paeth(a_bpp[0], *p0, c_bpp[0]));
                            *c1 = c1.wrapping_add(filter_paeth(a_bpp[1], *p1, c_bpp[1]));
                            *c2 = c2.wrapping_add(filter_paeth(a_bpp[2], *p2, c_bpp[2]));

                            a_bpp = [*c0, *c1, *c2];
                            c_bpp = [*p0, *p1, *p2];
                        }
                    }
                    BytesPerPixel::Four => {
                        // Using the `simd` module here has no effect on Linux
                        // and appears to regress performance on Windows, so we don't use it here.
                        // See https://github.com/image-rs/image-png/issues/567

                        let mut a_bpp = [0; 4];
                        let mut c_bpp = [0; 4];

                        let mut previous = &previous[..previous.len() & !3];
                        let current_len = current.len();
                        let mut current = &mut current[..current_len & !3];

                        while let ([c0, c1, c2, c3, c_rest @ ..], [p0, p1, p2, p3, p_rest @ ..]) =
                            (current, previous)
                        {
                            current = c_rest;
                            previous = p_rest;

                            *c0 = c0.wrapping_add(filter_paeth(a_bpp[0], *p0, c_bpp[0]));
                            *c1 = c1.wrapping_add(filter_paeth(a_bpp[1], *p1, c_bpp[1]));
                            *c2 = c2.wrapping_add(filter_paeth(a_bpp[2], *p2, c_bpp[2]));
                            *c3 = c3.wrapping_add(filter_paeth(a_bpp[3], *p3, c_bpp[3]));

                            a_bpp = [*c0, *c1, *c2, *c3];
                            c_bpp = [*p0, *p1, *p2, *p3];
                        }
                    }
                    BytesPerPixel::Six => {
                        let mut a_bpp = [0; 6];
                        let mut c_bpp = [0; 6];
                        for (chunk, b_bpp) in
                            current.chunks_exact_mut(6).zip(previous.chunks_exact(6))
                        {
                            let new_chunk = [
                                chunk[0].wrapping_add(filter_paeth(a_bpp[0], b_bpp[0], c_bpp[0])),
                                chunk[1].wrapping_add(filter_paeth(a_bpp[1], b_bpp[1], c_bpp[1])),
                                chunk[2].wrapping_add(filter_paeth(a_bpp[2], b_bpp[2], c_bpp[2])),
                                chunk[3].wrapping_add(filter_paeth(a_bpp[3], b_bpp[3], c_bpp[3])),
                                chunk[4].wrapping_add(filter_paeth(a_bpp[4], b_bpp[4], c_bpp[4])),
                                chunk[5].wrapping_add(filter_paeth(a_bpp[5], b_bpp[5], c_bpp[5])),
                            ];
                            *TryInto::<&mut [u8; 6]>::try_into(chunk).unwrap() = new_chunk;
                            a_bpp = new_chunk;
                            c_bpp = b_bpp.try_into().unwrap();
                        }
                    }
                    BytesPerPixel::Eight => {
                        let mut a_bpp = [0; 8];
                        let mut c_bpp = [0; 8];
                        for (chunk, b_bpp) in
                            current.chunks_exact_mut(8).zip(previous.chunks_exact(8))
                        {
                            let new_chunk = [
                                chunk[0].wrapping_add(filter_paeth(a_bpp[0], b_bpp[0], c_bpp[0])),
                                chunk[1].wrapping_add(filter_paeth(a_bpp[1], b_bpp[1], c_bpp[1])),
                                chunk[2].wrapping_add(filter_paeth(a_bpp[2], b_bpp[2], c_bpp[2])),
                                chunk[3].wrapping_add(filter_paeth(a_bpp[3], b_bpp[3], c_bpp[3])),
                                chunk[4].wrapping_add(filter_paeth(a_bpp[4], b_bpp[4], c_bpp[4])),
                                chunk[5].wrapping_add(filter_paeth(a_bpp[5], b_bpp[5], c_bpp[5])),
                                chunk[6].wrapping_add(filter_paeth(a_bpp[6], b_bpp[6], c_bpp[6])),
                                chunk[7].wrapping_add(filter_paeth(a_bpp[7], b_bpp[7], c_bpp[7])),
                            ];
                            *TryInto::<&mut [u8; 8]>::try_into(chunk).unwrap() = new_chunk;
                            a_bpp = new_chunk;
                            c_bpp = b_bpp.try_into().unwrap();
                        }
                    }
                }
            }

            // The x86_64 functions avoid casting between u8xN and i16xN SIMD
            // representations when possible by maintaining [i16; BPP] arrays
            // between iterations instead of [u8; BPP].
            #[cfg(target_arch = "x86_64")]
            {
                // Paeth filter pixels:
                // C B D
                // A X
                match tbpp {
                    BytesPerPixel::One => {
                        const BPP: usize = 1;
                        let mut a_bpp = [0; BPP];
                        let mut c_bpp = [0; BPP];

                        for (c, p) in current
                            .chunks_exact_mut(BPP)
                            .zip(previous.chunks_exact(BPP))
                        {
                            for i in 0..BPP {
                                c[i] = c[i].wrapping_add(filter_paeth_stbi(
                                    a_bpp[i],
                                    p[i] as i16,
                                    c_bpp[i],
                                ));
                            }

                            a_bpp = [c[0] as i16];
                            c_bpp = [p[0] as i16];
                        }
                    }
                    BytesPerPixel::Two => {
                        const BPP: usize = 2;
                        let mut a_bpp = [0; BPP];
                        let mut c_bpp = [0; BPP];

                        for (c, p) in current
                            .chunks_exact_mut(BPP)
                            .zip(previous.chunks_exact(BPP))
                        {
                            for i in 0..BPP {
                                c[i] = c[i].wrapping_add(filter_paeth_stbi(
                                    a_bpp[i],
                                    p[i] as i16,
                                    c_bpp[i],
                                ));
                            }

                            a_bpp = [c[0] as i16, c[1] as i16];
                            c_bpp = [p[0] as i16, p[1] as i16];
                        }
                    }
                    BytesPerPixel::Three => {
                        #[cfg(feature = "unstable")]
                        {
                            // Results in PR: https://github.com/image-rs/image-png/pull/63
                            // 23% better on an Epyc 7B13, 10% on a Zen 3 part.
                            // ~30% when targeting x86-64-v2.
                            simd::paeth_unfilter_3bpp(current, previous);
                            return;
                        }
                        const BPP: usize = 3;
                        let mut a_bpp = [0; BPP];
                        let mut c_bpp = [0; BPP];

                        for (c, p) in current
                            .chunks_exact_mut(BPP)
                            .zip(previous.chunks_exact(BPP))
                        {
                            for i in 0..BPP {
                                c[i] = c[i].wrapping_add(filter_paeth_stbi(
                                    a_bpp[i],
                                    p[i] as i16,
                                    c_bpp[i],
                                ));
                            }

                            a_bpp = [c[0] as i16, c[1] as i16, c[2] as i16];
                            c_bpp = [p[0] as i16, p[1] as i16, p[2] as i16];
                        }
                    }
                    BytesPerPixel::Four => {
                        // Using the `simd` module here has no effect on Linux
                        // and appears to regress performance on Windows, so we don't use it here.
                        // See https://github.com/image-rs/image-png/issues/567
                        const BPP: usize = 4;
                        let mut a_bpp = [0; BPP];
                        let mut c_bpp = [0; BPP];

                        for (c, p) in current
                            .chunks_exact_mut(BPP)
                            .zip(previous.chunks_exact(BPP))
                        {
                            for i in 0..BPP {
                                c[i] = c[i].wrapping_add(filter_paeth_stbi(
                                    a_bpp[i],
                                    p[i] as i16,
                                    c_bpp[i],
                                ));
                            }

                            a_bpp = [c[0] as i16, c[1] as i16, c[2] as i16, c[3] as i16];
                            c_bpp = [p[0] as i16, p[1] as i16, p[2] as i16, p[3] as i16];
                        }
                    }
                    BytesPerPixel::Six => {
                        const BPP: usize = 6;
                        let mut a_bpp = [0; BPP];
                        let mut c_bpp = [0; BPP];

                        for (c, p) in current
                            .chunks_exact_mut(BPP)
                            .zip(previous.chunks_exact(BPP))
                        {
                            for i in 0..BPP {
                                c[i] = c[i].wrapping_add(filter_paeth_stbi(
                                    a_bpp[i],
                                    p[i] as i16,
                                    c_bpp[i],
                                ));
                            }

                            a_bpp = [
                                c[0] as i16,
                                c[1] as i16,
                                c[2] as i16,
                                c[3] as i16,
                                c[4] as i16,
                                c[5] as i16,
                            ];
                            c_bpp = [
                                p[0] as i16,
                                p[1] as i16,
                                p[2] as i16,
                                p[3] as i16,
                                p[4] as i16,
                                p[5] as i16,
                            ];
                        }
                    }
                    BytesPerPixel::Eight => {
                        const BPP: usize = 8;
                        let mut a_bpp = [0; BPP];
                        let mut c_bpp = [0; BPP];

                        for (c, p) in current
                            .chunks_exact_mut(BPP)
                            .zip(previous.chunks_exact(BPP))
                        {
                            for i in 0..BPP {
                                c[i] = c[i].wrapping_add(filter_paeth_stbi(
                                    a_bpp[i],
                                    p[i] as i16,
                                    c_bpp[i],
                                ));
                            }

                            a_bpp = [
                                c[0] as i16,
                                c[1] as i16,
                                c[2] as i16,
                                c[3] as i16,
                                c[4] as i16,
                                c[5] as i16,
                                c[6] as i16,
                                c[7] as i16,
                            ];
                            c_bpp = [
                                p[0] as i16,
                                p[1] as i16,
                                p[2] as i16,
                                p[3] as i16,
                                p[4] as i16,
                                p[5] as i16,
                                p[6] as i16,
                                p[7] as i16,
                            ];
                        }
                    }
                }
            }
        }
    }
}

fn filter_internal(
    method: RowFilter,
    bpp: usize,
    len: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> RowFilter {
    use self::RowFilter::*;

    // This value was chosen experimentally based on what achieved the best performance. The
    // Rust compiler does auto-vectorization, and 32-bytes per loop iteration seems to enable
    // the fastest code when doing so.
    const CHUNK_SIZE: usize = 32;

    match method {
        NoFilter => {
            output.copy_from_slice(current);
            NoFilter
        }
        Sub => {
            let mut out_chunks = output[bpp..].chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
            let mut prev_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);

            for ((out, cur), prev) in (&mut out_chunks).zip(&mut cur_chunks).zip(&mut prev_chunks) {
                for i in 0..CHUNK_SIZE {
                    out[i] = cur[i].wrapping_sub(prev[i]);
                }
            }

            for ((out, cur), &prev) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub(prev);
            }

            output[..bpp].copy_from_slice(&current[..bpp]);
            Sub
        }
        Up => {
            let mut out_chunks = output.chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current.chunks_exact(CHUNK_SIZE);
            let mut prev_chunks = previous.chunks_exact(CHUNK_SIZE);

            for ((out, cur), prev) in (&mut out_chunks).zip(&mut cur_chunks).zip(&mut prev_chunks) {
                for i in 0..CHUNK_SIZE {
                    out[i] = cur[i].wrapping_sub(prev[i]);
                }
            }

            for ((out, cur), &prev) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub(prev);
            }
            Up
        }
        Avg => {
            let mut out_chunks = output[bpp..].chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
            let mut cur_minus_bpp_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);
            let mut prev_chunks = previous[bpp..].chunks_exact(CHUNK_SIZE);

            for (((out, cur), cur_minus_bpp), prev) in (&mut out_chunks)
                .zip(&mut cur_chunks)
                .zip(&mut cur_minus_bpp_chunks)
                .zip(&mut prev_chunks)
            {
                for i in 0..CHUNK_SIZE {
                    // Bitwise average of two integers without overflow and
                    // without converting to a wider bit-width. See:
                    // http://aggregate.org/MAGIC/#Average%20of%20Integers
                    // If this is unrolled by component, consider reverting to
                    // `((cur_minus_bpp[i] as u16 + prev[i] as u16) / 2) as u8`
                    out[i] = cur[i].wrapping_sub(
                        (cur_minus_bpp[i] & prev[i]) + ((cur_minus_bpp[i] ^ prev[i]) >> 1),
                    );
                }
            }

            for (((out, cur), &cur_minus_bpp), &prev) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(cur_minus_bpp_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub((cur_minus_bpp & prev) + ((cur_minus_bpp ^ prev) >> 1));
            }

            for i in 0..bpp {
                output[i] = current[i].wrapping_sub(previous[i] / 2);
            }
            Avg
        }
        Paeth => {
            let mut out_chunks = output[bpp..].chunks_exact_mut(CHUNK_SIZE);
            let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
            let mut a_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);
            let mut b_chunks = previous[bpp..].chunks_exact(CHUNK_SIZE);
            let mut c_chunks = previous[..len - bpp].chunks_exact(CHUNK_SIZE);

            for ((((out, cur), a), b), c) in (&mut out_chunks)
                .zip(&mut cur_chunks)
                .zip(&mut a_chunks)
                .zip(&mut b_chunks)
                .zip(&mut c_chunks)
            {
                for i in 0..CHUNK_SIZE {
                    out[i] = cur[i].wrapping_sub(filter_paeth_fpnge(a[i], b[i], c[i]));
                }
            }

            for ((((out, cur), &a), &b), &c) in out_chunks
                .into_remainder()
                .iter_mut()
                .zip(cur_chunks.remainder())
                .zip(a_chunks.remainder())
                .zip(b_chunks.remainder())
                .zip(c_chunks.remainder())
            {
                *out = cur.wrapping_sub(filter_paeth_fpnge(a, b, c));
            }

            for i in 0..bpp {
                output[i] = current[i].wrapping_sub(filter_paeth_fpnge(0, previous[i], 0));
            }
            Paeth
        }
    }
}

fn adaptive_filter(
    f: impl Fn(&[u8]) -> u64,
    bpp: usize,
    len: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> RowFilter {
    use RowFilter::*;

    let mut min_cost: u64 = u64::MAX;
    let mut filter_choice = RowFilter::NoFilter;
    for &filter in [Up, Sub, Avg, Paeth].iter() {
        filter_internal(filter, bpp, len, previous, current, output);
        let cost = f(output);
        if cost <= min_cost {
            min_cost = cost;
            filter_choice = filter;

            if cost == 0 {
                return filter_choice;
            }
        }
    }
    if filter_choice != Paeth {
        filter_internal(filter_choice, bpp, len, previous, current, output);
    }
    filter_choice
}

pub(crate) fn filter(
    method: Filter,
    bpp: BytesPerPixel,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> RowFilter {
    let bpp = bpp.into_usize();
    let len = current.len();

    match method {
        Filter::Adaptive => adaptive_filter(sum_buffer, bpp, len, previous, current, output),
        Filter::MinEntropy => adaptive_filter(entropy, bpp, len, previous, current, output),
        _ => {
            let filter = RowFilter::from_method(method).unwrap();
            filter_internal(filter, bpp, len, previous, current, output)
        }
    }
}

/// Estimate the value of i * log2(i) without using floating point operations,
/// implementation originally from oxipng.
fn ilog2i(i: u32) -> u32 {
    let log = 32 - i.leading_zeros() - 1;
    i * log + ((i - (1 << log)) << 1)
}

fn entropy(buf: &[u8]) -> u64 {
    let mut counts = [[0_u32; 256]; 4];
    let mut total = 0;

    // Count the number of occurrences of each byte value.
    let mut chunks = buf.chunks_exact(8);
    for chunk in &mut chunks {
        // Runs of zeros are common and very compressible, so treat them as free.
        if chunk == [0; 8] {
            continue;
        }

        // Scatter the counts into 4 separate arrays to reduce contention.
        for j in 0..2 {
            counts[0][chunk[j * 4] as usize] += 1;
            counts[1][chunk[1 + j * 4] as usize] += 1;
            counts[2][chunk[2 + j * 4] as usize] += 1;
            counts[3][chunk[3 + j * 4] as usize] += 1;
        }
        total += 8;
    }
    for &lit in chunks.remainder() {
        counts[0][lit as usize] += 1;
        total += 1;
    }

    // If the input is entirely zeros, short-circuit the entropy calculation.
    if counts[0][0] == total {
        return 0;
    }

    // Consolidate the counts.
    for i in 0..256 {
        counts[0][i] += counts[1][i] + counts[2][i] + counts[3][i];
    }

    // Compute the entropy.
    let mut entropy = ilog2i(total);
    for &count in &counts[0] {
        if count > 0 {
            entropy = entropy.saturating_sub(ilog2i(count));
        }
    }

    entropy as u64
}

// Helper function for Adaptive filter buffer summation
fn sum_buffer(buf: &[u8]) -> u64 {
    const CHUNK_SIZE: usize = 32;

    let mut buf_chunks = buf.chunks_exact(CHUNK_SIZE);
    let mut sum = 0_u64;

    for chunk in &mut buf_chunks {
        // At most, `acc` can be `32 * (i8::MIN as u8) = 32 * 128 = 4096`.
        let mut acc = 0;
        for &b in chunk {
            acc += u64::from((b as i8).unsigned_abs());
        }
        sum = sum.saturating_add(acc);
    }

    let mut acc = 0;
    for &b in buf_chunks.remainder() {
        acc += u64::from((b as i8).unsigned_abs());
    }

    sum.saturating_add(acc)
}

#[cfg(test)]
mod test {
    use super::*;
    use core::iter;

    #[test]
    fn roundtrip() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = iter::repeat(1).take(LEN.into()).collect();
        let current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();

        let roundtrip = |kind: RowFilter, bpp: BytesPerPixel| {
            let mut output = vec![0; LEN.into()];
            filter(kind.into(), bpp, &previous, &current, &mut output);
            unfilter(kind, bpp, &previous, &mut output);
            assert_eq!(
                output, expected,
                "Filtering {:?} with {:?} does not roundtrip",
                bpp, kind
            );
        };

        let filters = [
            RowFilter::NoFilter,
            RowFilter::Sub,
            RowFilter::Up,
            RowFilter::Avg,
            RowFilter::Paeth,
        ];

        let bpps = [
            BytesPerPixel::One,
            BytesPerPixel::Two,
            BytesPerPixel::Three,
            BytesPerPixel::Four,
            BytesPerPixel::Six,
            BytesPerPixel::Eight,
        ];

        for &filter in filters.iter() {
            for &bpp in bpps.iter() {
                roundtrip(filter, bpp);
            }
        }
    }

    #[test]
    #[ignore] // takes ~20s without optimizations
    fn paeth_impls_are_equivalent() {
        for a in 0..=255 {
            for b in 0..=255 {
                for c in 0..=255 {
                    let baseline = filter_paeth(a, b, c);
                    let fpnge = filter_paeth_fpnge(a, b, c);
                    let stbi = filter_paeth_stbi(a as i16, b as i16, c as i16);

                    assert_eq!(baseline, fpnge);
                    assert_eq!(baseline, stbi);
                }
            }
        }
    }

    #[test]
    fn roundtrip_ascending_previous_line() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = (0..LEN).collect();
        let current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();

        let roundtrip = |kind: RowFilter, bpp: BytesPerPixel| {
            let mut output = vec![0; LEN.into()];
            filter(kind.into(), bpp, &previous, &current, &mut output);
            unfilter(kind, bpp, &previous, &mut output);
            assert_eq!(
                output, expected,
                "Filtering {:?} with {:?} does not roundtrip",
                bpp, kind
            );
        };

        let filters = [
            RowFilter::NoFilter,
            RowFilter::Sub,
            RowFilter::Up,
            RowFilter::Avg,
            RowFilter::Paeth,
        ];

        let bpps = [
            BytesPerPixel::One,
            BytesPerPixel::Two,
            BytesPerPixel::Three,
            BytesPerPixel::Four,
            BytesPerPixel::Six,
            BytesPerPixel::Eight,
        ];

        for &filter in filters.iter() {
            for &bpp in bpps.iter() {
                roundtrip(filter, bpp);
            }
        }
    }

    #[test]
    // This tests that converting u8 to i8 doesn't overflow when taking the
    // absolute value for adaptive filtering: -128_i8.abs() will panic in debug
    // or produce garbage in release mode. The sum of 0..=255u8 should equal the
    // sum of the absolute values of -128_i8..=127, or abs(-128..=0) + 1..=127.
    fn sum_buffer_test() {
        let sum = (0..=128).sum::<u64>() + (1..=127).sum::<u64>();
        let buf: Vec<u8> = (0_u8..=255).collect();

        assert_eq!(sum, crate::filter::sum_buffer(&buf));
    }
}
