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

pub(crate) fn unfilter_paeth_strip(tbpp: BytesPerPixel, row_bytes: usize, strip: &mut [u8]) {
    const FULL_STRIP_HEIGHT: usize = 9;
    const STRIP_HEIGHT: usize = FULL_STRIP_HEIGHT - 1;

    let bpp = tbpp.into_usize();
    let prefix_bytes = (FULL_STRIP_HEIGHT + 1) * bpp;
    let mut rows: Vec<&mut [u8]> = strip.chunks_exact_mut(row_bytes).collect();

    assert_eq!(rows.len(), FULL_STRIP_HEIGHT);
    assert!(bpp == 3 || bpp == 4);
    assert!(prefix_bytes < row_bytes);

    for i in 1..=STRIP_HEIGHT {
        for k in 0..bpp {
            rows[i][k] = rows[i][k].wrapping_add(filter_paeth(0, rows[i - 1][k], 0));
        }
        for j in bpp..(prefix_bytes - bpp * i) {
            rows[i][j] = rows[i][j].wrapping_add(filter_paeth(
                rows[i][j - bpp],
                rows[i - 1][j],
                rows[i - 1][j - bpp],
            ));
        }
    }

    let inner_bytes = row_bytes + 2 * bpp - prefix_bytes;
    let mut inner_rows: Vec<&mut [u8]> = rows
        .iter_mut()
        .enumerate()
        .map(|(i, row)| &mut row[bpp * (STRIP_HEIGHT - i)..][..inner_bytes])
        .collect();

    if bpp == 3 {
        let mut a = [0u8; STRIP_HEIGHT * 3];
        let mut c = [0u8; STRIP_HEIGHT * 3];
        for j in 0..STRIP_HEIGHT {
            c[j * 3..][..3].copy_from_slice(&inner_rows[j][0..3]);
            a[j * 3..][..3].copy_from_slice(&inner_rows[j + 1][3..6]);
        }

        for i in (6..inner_bytes).step_by(3) {
            let mut b = [0u8; STRIP_HEIGHT * 3];
            let mut cur = [0u8; STRIP_HEIGHT * 3];
            for j in 0..STRIP_HEIGHT {
                b[j * 3..][..3].copy_from_slice(&inner_rows[j][i - 3..][..3]);
                cur[j * 3..][..3].copy_from_slice(&inner_rows[j + 1][i..][..3]);
            }

            let mut out = [0u8; STRIP_HEIGHT * 3];
            for j in 0..STRIP_HEIGHT * 3 {
                out[j] = cur[j].wrapping_add(filter_paeth(a[j], b[j], c[j]));
            }

            for j in 0..STRIP_HEIGHT {
                inner_rows[j + 1][i..][..3].copy_from_slice(&out[j * 3..][..3]);
            }
            a = out;
            c = b;
        }
    } else if bpp == 4 {
        let mut a = [0u8; STRIP_HEIGHT * 4];
        let mut c = [0u8; STRIP_HEIGHT * 4];
        for j in 0..STRIP_HEIGHT {
            c[j * 4..][..4].copy_from_slice(&inner_rows[j][0..4]);
            a[j * 4..][..4].copy_from_slice(&inner_rows[j + 1][4..8]);
        }

        for i in (8..inner_bytes).step_by(4) {
            let mut b = [0u8; STRIP_HEIGHT * 4];
            let mut cur = [0u8; STRIP_HEIGHT * 4];
            for j in 0..STRIP_HEIGHT {
                b[j * 4..][..4].copy_from_slice(&inner_rows[j][i - 4..][..4]);
                cur[j * 4..][..4].copy_from_slice(&inner_rows[j + 1][i..][..4]);
            }

            let mut out = [0u8; STRIP_HEIGHT * 4];
            for j in 0..STRIP_HEIGHT * 4 {
                out[j] = cur[j].wrapping_add(filter_paeth(a[j], b[j], c[j]));
            }

            for j in 0..STRIP_HEIGHT {
                inner_rows[j + 1][i..][..4].copy_from_slice(&out[j * 4..][..4]);
            }
            a = out;
            c = b;
        }
    }

    for i in 1..FULL_STRIP_HEIGHT {
        for j in (prefix_bytes - i * bpp + inner_bytes - 2 * bpp)..row_bytes {
            rows[i][j] = rows[i][j].wrapping_add(filter_paeth(
                rows[i][j - bpp],
                rows[i - 1][j],
                rows[i - 1][j - bpp],
            ));
        }
    }
}

fn filter_internal(
    method: FilterType,
    bpp: usize,
    len: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> FilterType {
    use self::FilterType::*;

    // This value was chosen experimentally based on what acheived the best performance. The
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
                .into_iter()
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
                    out[i] = cur[i].wrapping_sub(
                        ((u16::from(cur_minus_bpp[i]) + u16::from(prev[i])) / 2) as u8,
                    );
                }
            }

            for (((out, cur), &cur_minus_bpp), &prev) in out_chunks
                .into_remainder()
                .into_iter()
                .zip(cur_chunks.remainder())
                .zip(cur_minus_bpp_chunks.remainder())
                .zip(prev_chunks.remainder())
            {
                *out = cur.wrapping_sub(((u16::from(cur_minus_bpp) + u16::from(prev)) / 2) as u8);
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
                    out[i] = cur[i].wrapping_sub(filter_paeth(a[i], b[i], c[i]));
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
                *out = cur.wrapping_sub(filter_paeth(a, b, c));
            }

            for i in 0..bpp {
                output[i] = current[i].wrapping_sub(filter_paeth(0, previous[i], 0));
            }
            Paeth
        }
    }
}

pub(crate) fn filter(
    method: FilterType,
    adaptive: AdaptiveFilterType,
    bpp: BytesPerPixel,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> FilterType {
    use FilterType::*;
    let bpp = bpp.into_usize();
    let len = current.len();

    match adaptive {
        AdaptiveFilterType::NonAdaptive => {
            filter_internal(method, bpp, len, previous, current, output)
        }
        AdaptiveFilterType::Adaptive => {
            let mut min_sum: u64 = u64::MAX;
            let mut filter_choice = FilterType::NoFilter;
            for &filter in [Sub, Up, Avg, Paeth].iter() {
                filter_internal(filter, bpp, len, previous, current, output);
                let sum = sum_buffer(&output);
                if sum <= min_sum {
                    min_sum = sum;
                    filter_choice = filter;
                }
            }

            if filter_choice != Paeth {
                filter_internal(filter_choice, bpp, len, previous, current, output);
            }
            filter_choice
        }
    }
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
