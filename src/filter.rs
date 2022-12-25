use crate::common::BytesPerPixel;

/// The byte level filter applied to scanlines to prepare them for compression.
///
/// Compression in general benefits from repetitive data. The filter is a content-aware method of
/// compressing the range of occurring byte values to help the compression algorithm. Note that
/// this does not operate on pixels but on raw bytes of a scanline.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum FilterType {
    NoFilter = 0,
    Sub = 1,
    Up = 2,
    Avg = 3,
    Paeth = 4,
}

impl Default for FilterType {
    fn default() -> Self {
        FilterType::Sub
    }
}

impl FilterType {
    /// u8 -> Self. Temporary solution until Rust provides a canonical one.
    pub fn from_u8(n: u8) -> Option<FilterType> {
        match n {
            0 => Some(FilterType::NoFilter),
            1 => Some(FilterType::Sub),
            2 => Some(FilterType::Up),
            3 => Some(FilterType::Avg),
            4 => Some(FilterType::Paeth),
            _ => None,
        }
    }
}

/// The filtering method for preprocessing scanline data before compression.
///
/// Adaptive filtering performs additional computation in an attempt to maximize
/// the compression of the data. [`NonAdaptive`] filtering is the default.
///
/// [`NonAdaptive`]: enum.AdaptiveFilterType.html#variant.NonAdaptive
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AdaptiveFilterType {
    Adaptive,
    NonAdaptive,
}

impl Default for AdaptiveFilterType {
    fn default() -> Self {
        AdaptiveFilterType::NonAdaptive
    }
}

fn filter_sub(a: u8, _: u8, _: u8) -> u8 {
    a
}
fn filter_up(_: u8, b: u8, _: u8) -> u8 {
    b
}
fn filter_avg(a: u8, b: u8, _: u8) -> u8 {
    ((u16::from(a) + u16::from(b)) / 2) as u8
}
fn filter_paeth(a: u8, b: u8, c: u8) -> u8 {
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
    filter: FilterType,
    tbpp: BytesPerPixel,
    previous: &[u8],
    current: &mut [u8],
) -> std::result::Result<(), &'static str> {
    use self::FilterType::*;
    let bpp = tbpp.into_usize();
    let len = current.len();

    fn require_length(slice: &[u8], length: usize) -> Result<&[u8], &'static str> {
        match slice.get(..length) {
            None => Err("Filtering failed: not enough data in previous row"),
            Some(slice) => Ok(slice),
        }
    }

    match filter {
        NoFilter => Ok(()),
        Sub => {
            let current = &mut current[..len];
            for i in bpp..len {
                current[i] = current[i].wrapping_add(current[i - bpp]);
            }
            Ok(())
        }
        Up => {
            let current = &mut current[..len];
            let previous = require_length(previous, len)?;
            for i in 0..len {
                current[i] = current[i].wrapping_add(previous[i]);
            }
            Ok(())
        }
        Avg => {
            let current = &mut current[..len];
            let previous = require_length(previous, len)?;
            if bpp > len {
                return Err("Filtering failed: bytes per pixel is greater than length of row");
            }

            for i in 0..bpp {
                current[i] = current[i].wrapping_add(previous[i] / 2);
            }

            macro_rules! avg_tail {
                ($name:ident, $bpp:expr) => {
                    fn $name(current: &mut [u8], previous: &[u8]) {
                        let len = current.len();
                        let current = &mut current[..len];
                        let previous = &previous[..len];

                        let mut current = current.chunks_exact_mut($bpp);
                        let mut previous = previous.chunks_exact($bpp);

                        let mut lprevious = current.next().unwrap();
                        let _ = previous.next();

                        while let Some(pprevious) = previous.next() {
                            let pcurrent = current.next().unwrap();

                            for i in 0..$bpp {
                                let lprev = lprevious[i];
                                let pprev = pprevious[i];
                                pcurrent[i] = pcurrent[i].wrapping_add(
                                    ((u16::from(lprev) + u16::from(pprev)) / 2) as u8,
                                );
                            }

                            lprevious = pcurrent;
                        }
                    }
                };
            }

            avg_tail!(avg_tail_8, 8);
            avg_tail!(avg_tail_6, 6);
            avg_tail!(avg_tail_4, 4);
            avg_tail!(avg_tail_3, 3);
            avg_tail!(avg_tail_2, 2);
            avg_tail!(avg_tail_1, 1);

            match tbpp {
                BytesPerPixel::Eight => avg_tail_8(current, previous),
                BytesPerPixel::Six => avg_tail_6(current, previous),
                BytesPerPixel::Four => avg_tail_4(current, previous),
                BytesPerPixel::Three => avg_tail_3(current, previous),
                BytesPerPixel::Two => avg_tail_2(current, previous),
                BytesPerPixel::One => avg_tail_1(current, previous),
            }

            Ok(())
        }
        Paeth => {
            let current = &mut current[..len];
            let previous = require_length(previous, len)?;
            if bpp > len {
                return Err("Filtering failed: bytes per pixel is greater than length of row");
            }

            for i in 0..bpp {
                current[i] = current[i].wrapping_add(filter_paeth(0, previous[i], 0));
            }

            let mut current = current.chunks_exact_mut(bpp);
            let mut previous = previous.chunks_exact(bpp);

            let mut lprevious = current.next().unwrap();
            let mut lpprevious = previous.next().unwrap();

            for pprevious in previous {
                let pcurrent = current.next().unwrap();

                for i in 0..bpp {
                    pcurrent[i] = pcurrent[i].wrapping_add(filter_paeth(
                        lprevious[i],
                        pprevious[i],
                        lpprevious[i],
                    ));
                }

                lprevious = pcurrent;
                lpprevious = pprevious;
            }

            Ok(())
        }
    }
}

#[inline(always)]
fn run_filter(
    bpp: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
    mut f: impl FnMut(&mut u8, u8, u8, u8, u8),
) {
    // This value was chosen experimentally based on what acheived the best performance. The
    // Rust compiler does auto-vectorization, and 32-bytes per loop iteration seems to enable
    // the fastest code when doing so.
    const CHUNK_SIZE: usize = 32;

    let len = current.len();

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
            f(&mut out[i], cur[i], a[i], b[i], c[i]);
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
        f(out, *cur, a, b, c);
    }

    for i in 0..bpp {
        f(&mut output[i], current[i], 0, previous[i], 0);
    }
}

// On x86, this function performs several *times* faster when compiled with AVX2 support, so use
// the multiversion macro to detect support at runtime.
#[multiversion::multiversion(targets("x86_64+avx2"))]
fn select_filter(bpp: usize, previous: &[u8], current: &[u8], output: &mut [u8]) -> FilterType {
    let mut sub_sum = 0u64;
    let mut up_sum = 0u64;
    let mut avg_sum = 0u64;
    let mut paeth_sum = 0u64;

    // Sanity check to make sure weights won't overflow even though we use wrapping adds
    // below. The optimizer seems to handle `wrapping_add` better than `saturating_add` even
    // though the latter is the more natural choice. This condition should always hold because
    // PNG rows are at most 2^32 - 1 pixels.
    assert!((current.len() as u64) < (1 << 56));

    // Compute hueristic weights for each filter. The "none" filter isn't considered because
    // experiments showed that including it tended to make the compression worse.
    let weight = |x| i64::from(x as i8).abs() as u64;
    let update_sums = |_: &mut u8, cur: u8, a, b, c| {
        sub_sum = sub_sum.wrapping_add(weight(cur.wrapping_sub(filter_sub(a, b, c))));
        up_sum = up_sum.wrapping_add(weight(cur.wrapping_sub(filter_up(a, b, c))));
        avg_sum = avg_sum.wrapping_add(weight(cur.wrapping_sub(filter_avg(a, b, c))));
        paeth_sum = paeth_sum.wrapping_add(weight(cur.wrapping_sub(filter_paeth(a, b, c))));
    };
    run_filter(bpp, previous, current, output, update_sums);

    // The filter with the lowest sum is the best filter. If two filters have the same sum, pick
    // the one that is cheaper to decode.
    let min_sum = sub_sum.min(up_sum).min(avg_sum).min(paeth_sum);
    if up_sum == min_sum {
        FilterType::Up
    } else if sub_sum == min_sum {
        FilterType::Sub
    } else if avg_sum == min_sum {
        FilterType::Avg
    } else {
        FilterType::Paeth
    }
}

pub(crate) fn filter(
    mut method: FilterType,
    adaptive: AdaptiveFilterType,
    bpp: BytesPerPixel,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> FilterType {
    let bpp = bpp.into_usize();
    if let AdaptiveFilterType::Adaptive = adaptive {
        method = select_filter(bpp, previous, current, output);
    }

    fn apply_filter(f: impl Fn(u8, u8, u8) -> u8) -> impl FnMut(&mut u8, u8, u8, u8, u8) {
        move |out, cur, a, b, c| *out = cur.wrapping_sub(f(a, b, c))
    }
    match method {
        FilterType::NoFilter => output.copy_from_slice(current),
        FilterType::Sub => run_filter(bpp, previous, current, output, apply_filter(filter_sub)),
        FilterType::Up => run_filter(bpp, previous, current, output, apply_filter(filter_up)),
        FilterType::Avg => run_filter(bpp, previous, current, output, apply_filter(filter_avg)),
        FilterType::Paeth => run_filter(bpp, previous, current, output, apply_filter(filter_paeth)),
    }

    method
}

#[cfg(test)]
mod test {
    use super::{filter, unfilter, AdaptiveFilterType, BytesPerPixel, FilterType};
    use core::iter;

    #[test]
    fn roundtrip() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = iter::repeat(1).take(LEN.into()).collect();
        let current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();
        let adaptive = AdaptiveFilterType::NonAdaptive;

        let roundtrip = |kind, bpp: BytesPerPixel| {
            let mut output = vec![0; LEN.into()];
            filter(kind, adaptive, bpp, &previous, &current, &mut output);
            unfilter(kind, bpp, &previous, &mut output).expect("Unfilter worked");
            assert_eq!(
                output, expected,
                "Filtering {:?} with {:?} does not roundtrip",
                bpp, kind
            );
        };

        let filters = [
            FilterType::NoFilter,
            FilterType::Sub,
            FilterType::Up,
            FilterType::Avg,
            FilterType::Paeth,
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
    fn roundtrip_ascending_previous_line() {
        // A multiple of 8, 6, 4, 3, 2, 1
        const LEN: u8 = 240;
        let previous: Vec<_> = (0..LEN).collect();
        let current: Vec<_> = (0..LEN).collect();
        let expected = current.clone();
        let adaptive = AdaptiveFilterType::NonAdaptive;

        let roundtrip = |kind, bpp: BytesPerPixel| {
            let mut output = vec![0; LEN.into()];
            filter(kind, adaptive, bpp, &previous, &current, &mut output);
            unfilter(kind, bpp, &previous, &mut output).expect("Unfilter worked");
            assert_eq!(
                output, expected,
                "Filtering {:?} with {:?} does not roundtrip",
                bpp, kind
            );
        };

        let filters = [
            FilterType::NoFilter,
            FilterType::Sub,
            FilterType::Up,
            FilterType::Avg,
            FilterType::Paeth,
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
}
