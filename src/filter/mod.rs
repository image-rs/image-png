use crate::{common::BytesPerPixel, Compression};

mod paeth;

#[cfg(feature = "unstable")]
mod simd;

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
        Paeth => paeth::unfilter(tbpp, previous, current),
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
                    out[i] = cur[i].wrapping_sub(paeth::filter_paeth_fpnge(a[i], b[i], c[i]));
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
                *out = cur.wrapping_sub(paeth::filter_paeth_fpnge(a, b, c));
            }

            for i in 0..bpp {
                output[i] = current[i].wrapping_sub(paeth::filter_paeth_fpnge(0, previous[i], 0));
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

// Fused adaptive filter selection for `Filter::Adaptive`.
//
// Avoids writing all 4 candidate filters into `output` and re-reading `output`
// in `sum_buffer`, and avoids recomputing the winning filter a 5th time when
// `Paeth` does not win. Produces byte-for-byte identical filter choices and
// output to `adaptive_filter(sum_buffer, ...)`.
//
// On `aarch64` (32 NEON vector registers + native 1-cycle `ABS` and `UADDLV`),
// scoring all 4 filters in a single read-only 32-byte pass gives the best
// throughput. On `x86_64` baseline SSE2 (16 XMM registers, no `PABSB` until
// SSSE3) and other architectures, a 2-stage stack-chunk pass avoids vector
// register spills and aborts Paeth early once its cost exceeds the best of
// `{Up, Sub, Avg}`.
#[cfg(target_arch = "aarch64")]
fn adaptive_filter_sum(
    bpp: usize,
    len: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> RowFilter {
    use RowFilter::*;
    const CHUNK_SIZE: usize = 32;

    assert_eq!(previous.len(), len);
    assert_eq!(output.len(), len);

    let head = bpp.min(len);
    let mut sum_up: u64 = 0;
    let mut sum_sub: u64 = 0;
    let mut sum_avg: u64 = 0;
    let mut sum_paeth: u64 = 0;

    for i in 0..head {
        let cur = current[i];
        let b = previous[i];
        sum_up += u64::from((cur.wrapping_sub(b) as i8).unsigned_abs());
        sum_sub += u64::from((cur as i8).unsigned_abs());
        sum_avg += u64::from((cur.wrapping_sub(b / 2) as i8).unsigned_abs());
        sum_paeth +=
            u64::from((cur.wrapping_sub(paeth::filter_paeth_fpnge(0, b, 0)) as i8).unsigned_abs());
    }

    if len > bpp {
        let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
        let mut a_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);
        let mut b_chunks = previous[bpp..].chunks_exact(CHUNK_SIZE);
        let mut c_chunks = previous[..len - bpp].chunks_exact(CHUNK_SIZE);

        for (((cur, a), b), c) in (&mut cur_chunks)
            .zip(&mut a_chunks)
            .zip(&mut b_chunks)
            .zip(&mut c_chunks)
        {
            // Max per chunk is 32 * 128 = 4096, which fits in u16 without overflow.
            let mut acc_up: u16 = 0;
            let mut acc_sub: u16 = 0;
            let mut acc_avg: u16 = 0;
            let mut acc_paeth: u16 = 0;
            for i in 0..CHUNK_SIZE {
                let x = cur[i];
                let av = a[i];
                let bv = b[i];
                let cv = c[i];

                let up = x.wrapping_sub(bv);
                let sub = x.wrapping_sub(av);
                let avg = x.wrapping_sub((av & bv) + ((av ^ bv) >> 1));
                let paeth = x.wrapping_sub(paeth::filter_paeth_fpnge(av, bv, cv));

                acc_up += u16::from((up as i8).unsigned_abs());
                acc_sub += u16::from((sub as i8).unsigned_abs());
                acc_avg += u16::from((avg as i8).unsigned_abs());
                acc_paeth += u16::from((paeth as i8).unsigned_abs());
            }
            sum_up += u64::from(acc_up);
            sum_sub += u64::from(acc_sub);
            sum_avg += u64::from(acc_avg);
            sum_paeth += u64::from(acc_paeth);
        }

        for (((&x, &av), &bv), &cv) in cur_chunks
            .remainder()
            .iter()
            .zip(a_chunks.remainder())
            .zip(b_chunks.remainder())
            .zip(c_chunks.remainder())
        {
            let up = x.wrapping_sub(bv);
            let sub = x.wrapping_sub(av);
            let avg = x.wrapping_sub((av & bv) + ((av ^ bv) >> 1));
            let paeth = x.wrapping_sub(paeth::filter_paeth_fpnge(av, bv, cv));

            sum_up += u64::from((up as i8).unsigned_abs());
            sum_sub += u64::from((sub as i8).unsigned_abs());
            sum_avg += u64::from((avg as i8).unsigned_abs());
            sum_paeth += u64::from((paeth as i8).unsigned_abs());
        }
    }

    // Preserve exact zero-cost short-circuit and `<=` tie-breaking order of
    // `[Up, Sub, Avg, Paeth]`.
    let filter_choice = if sum_up == 0 {
        Up
    } else if sum_sub == 0 {
        Sub
    } else if sum_avg == 0 {
        Avg
    } else if sum_paeth == 0 {
        Paeth
    } else {
        let mut min_cost = sum_up;
        let mut choice = Up;
        if sum_sub <= min_cost {
            min_cost = sum_sub;
            choice = Sub;
        }
        if sum_avg <= min_cost {
            min_cost = sum_avg;
            choice = Avg;
        }
        if sum_paeth <= min_cost {
            choice = Paeth;
        }
        choice
    };

    filter_internal(filter_choice, bpp, len, previous, current, output)
}

#[cfg(not(target_arch = "aarch64"))]
#[inline(always)]
fn sum_chunk32(chunk: &[u8; 32]) -> u64 {
    let mut acc: u16 = 0;
    for &b in chunk {
        acc += u16::from((b as i8).unsigned_abs());
    }
    u64::from(acc)
}

#[cfg(not(target_arch = "aarch64"))]
fn adaptive_filter_sum(
    bpp: usize,
    len: usize,
    previous: &[u8],
    current: &[u8],
    output: &mut [u8],
) -> RowFilter {
    use RowFilter::*;
    const CHUNK_SIZE: usize = 32;

    assert_eq!(previous.len(), len);
    assert_eq!(output.len(), len);

    let head = bpp.min(len);
    let mut sum_up: u64 = 0;
    let mut sum_sub: u64 = 0;
    let mut sum_avg: u64 = 0;

    for i in 0..head {
        let cur = current[i];
        let b = previous[i];
        sum_up += u64::from((cur.wrapping_sub(b) as i8).unsigned_abs());
        sum_sub += u64::from((cur as i8).unsigned_abs());
        sum_avg += u64::from((cur.wrapping_sub(b / 2) as i8).unsigned_abs());
    }

    if len > bpp {
        let mut cur_chunks = current[bpp..].chunks_exact(CHUNK_SIZE);
        let mut a_chunks = current[..len - bpp].chunks_exact(CHUNK_SIZE);
        let mut b_chunks = previous[bpp..].chunks_exact(CHUNK_SIZE);

        for ((cur, a), b) in (&mut cur_chunks).zip(&mut a_chunks).zip(&mut b_chunks) {
            let mut up = [0u8; CHUNK_SIZE];
            let mut sub = [0u8; CHUNK_SIZE];
            let mut avg = [0u8; CHUNK_SIZE];
            for i in 0..CHUNK_SIZE {
                up[i] = cur[i].wrapping_sub(b[i]);
            }
            for i in 0..CHUNK_SIZE {
                sub[i] = cur[i].wrapping_sub(a[i]);
            }
            for i in 0..CHUNK_SIZE {
                avg[i] = cur[i].wrapping_sub((a[i] & b[i]) + ((a[i] ^ b[i]) >> 1));
            }
            sum_up += sum_chunk32(&up);
            sum_sub += sum_chunk32(&sub);
            sum_avg += sum_chunk32(&avg);
        }

        for ((&x, &av), &bv) in cur_chunks
            .remainder()
            .iter()
            .zip(a_chunks.remainder())
            .zip(b_chunks.remainder())
        {
            sum_up += u64::from((x.wrapping_sub(bv) as i8).unsigned_abs());
            sum_sub += u64::from((x.wrapping_sub(av) as i8).unsigned_abs());
            sum_avg +=
                u64::from((x.wrapping_sub((av & bv) + ((av ^ bv) >> 1)) as i8).unsigned_abs());
        }
    }

    if sum_up == 0 {
        return filter_internal(Up, bpp, len, previous, current, output);
    }
    if sum_sub == 0 {
        return filter_internal(Sub, bpp, len, previous, current, output);
    }
    if sum_avg == 0 {
        return filter_internal(Avg, bpp, len, previous, current, output);
    }

    let mut min_cost = sum_up;
    let mut filter_choice = Up;
    if sum_sub <= min_cost {
        min_cost = sum_sub;
        filter_choice = Sub;
    }
    if sum_avg <= min_cost {
        min_cost = sum_avg;
        filter_choice = Avg;
    }

    let mut sum_paeth: u64 = 0;
    for i in 0..head {
        let v = current[i].wrapping_sub(paeth::filter_paeth_fpnge(0, previous[i], 0));
        output[i] = v;
        sum_paeth += u64::from((v as i8).unsigned_abs());
    }
    if sum_paeth > min_cost {
        return filter_internal(filter_choice, bpp, len, previous, current, output);
    }

    if len > bpp {
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
            let mut paeth_buf = [0u8; CHUNK_SIZE];
            for i in 0..CHUNK_SIZE {
                paeth_buf[i] = cur[i].wrapping_sub(paeth::filter_paeth_fpnge(a[i], b[i], c[i]));
            }
            out.copy_from_slice(&paeth_buf);
            sum_paeth += sum_chunk32(&paeth_buf);
            if sum_paeth > min_cost {
                return filter_internal(filter_choice, bpp, len, previous, current, output);
            }
        }

        for ((((out, &x), &av), &bv), &cv) in out_chunks
            .into_remainder()
            .iter_mut()
            .zip(cur_chunks.remainder())
            .zip(a_chunks.remainder())
            .zip(b_chunks.remainder())
            .zip(c_chunks.remainder())
        {
            let v = x.wrapping_sub(paeth::filter_paeth_fpnge(av, bv, cv));
            *out = v;
            sum_paeth += u64::from((v as i8).unsigned_abs());
        }
    }

    if sum_paeth <= min_cost {
        Paeth
    } else {
        filter_internal(filter_choice, bpp, len, previous, current, output)
    }
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
        Filter::Adaptive => adaptive_filter_sum(bpp, len, previous, current, output),
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
    //
    // Upstream bug: <https://github.com/rust-lang/rust-clippy/issues/11529>
    #[allow(clippy::needless_range_loop)]
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
#[cfg(test)]
fn sum_buffer(buf: &[u8]) -> u64 {
    const CHUNK_SIZE: usize = 32;

    let mut buf_chunks = buf.chunks_exact(CHUNK_SIZE);
    let mut sum = 0_u64;

    for chunk in &mut buf_chunks {
        // At most, `acc` can be `32 * (i8::MIN as u8) = 32 * 128 = 4096`.
        let mut acc: u16 = 0;
        for &b in chunk {
            acc += u16::from((b as i8).unsigned_abs());
        }
        sum = sum.saturating_add(u64::from(acc));
    }

    let mut acc: u16 = 0;
    for &b in buf_chunks.remainder() {
        acc += u16::from((b as i8).unsigned_abs());
    }

    sum.saturating_add(u64::from(acc))
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

    #[test]
    fn adaptive_filter_sum_matches_reference() {
        let bpps = [
            BytesPerPixel::One,
            BytesPerPixel::Two,
            BytesPerPixel::Three,
            BytesPerPixel::Four,
            BytesPerPixel::Six,
            BytesPerPixel::Eight,
        ];
        let mut rng: u64 = 0x9e37_79b9_7f4a_7c15;
        let mut next_byte = || -> u8 {
            rng ^= rng << 13;
            rng ^= rng >> 7;
            rng ^= rng << 17;
            (rng >> 24) as u8
        };

        for &tbpp in &bpps {
            let bpp = tbpp.into_usize();
            for &len in &[bpp, bpp + 1, 31, 32, 33, 63, 64, 65, 240, 513] {
                if len < bpp {
                    continue;
                }
                let patterns: Vec<(Vec<u8>, Vec<u8>)> = vec![
                    // All zeros (Up == 0 short-circuit)
                    (vec![0; len], vec![0; len]),
                    // Identical non-zero rows (Up == 0)
                    (vec![42; len], vec![42; len]),
                    // Horizontal constant, previous zero (Sub wins / low cost)
                    (vec![0; len], vec![123; len]),
                    // Ascending ramp
                    (
                        (0..len).map(|i| (i & 0xff) as u8).collect(),
                        (0..len).map(|i| ((i + 3) & 0xff) as u8).collect(),
                    ),
                    // High-contrast alternating 0x00 / 0x80 (-128 boundary)
                    (
                        (0..len)
                            .map(|i| if i % 2 == 0 { 0x80 } else { 0x00 })
                            .collect(),
                        (0..len)
                            .map(|i| if i % 2 == 0 { 0x00 } else { 0x80 })
                            .collect(),
                    ),
                    // Pseudo-random
                    (
                        (0..len).map(|_| next_byte()).collect(),
                        (0..len).map(|_| next_byte()).collect(),
                    ),
                ];

                for (previous, current) in &patterns {
                    let mut ref_out = vec![0u8; len];
                    let mut got_out = vec![0u8; len];
                    let want =
                        adaptive_filter(sum_buffer, bpp, len, previous, current, &mut ref_out);
                    let got = adaptive_filter_sum(bpp, len, previous, current, &mut got_out);
                    assert_eq!(got, want, "filter mismatch for bpp={bpp} len={len}");
                    assert_eq!(got_out, ref_out, "output mismatch for bpp={bpp} len={len}");
                }
            }
        }
    }
}
