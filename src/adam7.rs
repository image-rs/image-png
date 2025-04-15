//! Utility functions related to handling of
//! [the Adam7 algorithm](https://en.wikipedia.org/wiki/Adam7_algorithm).

/// Describes which stage of
/// [the Adam7 algorithm](https://en.wikipedia.org/wiki/Adam7_algorithm)
/// applies to a decoded row.
///
/// See also [Reader.next_interlaced_row](crate::decoder::Reader::next_interlaced_row).
use std::cmp::min;

struct PassConstants {
    line_mul: u8,
    line_off: u8,
    samp_mul: u8,
    samp_off: u8,
}

impl PassConstants {
    const fn new(line_mul: u8, line_off: u8, samp_mul: u8, samp_off: u8) -> Self {
        Self {
            line_mul,
            line_off,
            samp_mul,
            samp_off,
        }
    }
    const fn x_repeat(&self) -> u8 {
        self.samp_mul - self.samp_off
    }
    const fn y_repeat(&self) -> u8 {
        self.line_mul - self.line_off
    }
}

const PASS_CONSTANTS: [PassConstants; 7] = [
    PassConstants::new(8, 0, 8, 0),
    PassConstants::new(8, 0, 8, 4),
    PassConstants::new(8, 4, 4, 0),
    PassConstants::new(4, 0, 4, 2),
    PassConstants::new(4, 2, 2, 0),
    PassConstants::new(2, 0, 2, 1),
    PassConstants::new(2, 1, 1, 0),
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Adam7Info {
    pub(crate) pass: u8,
    pub(crate) line: u32,
    pub(crate) width: u32,
}

impl Adam7Info {
    /// Creates a new `Adam7Info`.  May panic if the arguments are out of range (e.g. if `pass` is
    /// 0 or greater than 8).
    ///
    /// * `pass` corresponds to a pass of the
    ///   [the Adam7 algorithm](https://en.wikipedia.org/wiki/Adam7_algorithm)
    /// * `line` is the number of a line within a pass (starting with 0).  For example,
    ///   in an image of height 8, `line` can be beteween `0..4` in the 7th `pass`
    ///   (those 4 interlaced rows correspond to 2nd, 4th, 6th, and 8th row of the full image).
    /// * `width` describes how many pixels are in an interlaced row.  For example,
    ///   in the 7th `pass`, the `width` is be the same as full image width, but in
    ///   in the 1st `pass`, the `width` is be 1/8th of the image width (rounded up as
    ///   necessary).
    ///
    /// Note that in typical usage, `Adam7Info`s are returned by [Reader.next_interlaced_row]
    /// and there is no need to create them by calling `Adam7Info::new`.  `Adam7Info::new` is
    /// nevertheless exposed as a public API, because it helps to provide self-contained example
    /// usage of [expand_interlaced_row](crate::expand_interlaced_row).
    pub fn new(pass: u8, line: u32, width: u32) -> Self {
        assert!(1 <= pass && pass <= 7);
        assert!(width > 0);
        Self { pass, line, width }
    }
}

/// This iterator iterates over the different passes of an image Adam7 encoded
/// PNG image
/// The pattern is:
///     16462646
///     77777777
///     56565656
///     77777777
///     36463646
///     77777777
///     56565656
///     77777777
///
#[derive(Clone)]
pub(crate) struct Adam7Iterator {
    line: u32,
    lines: u32,
    line_width: u32,
    current_pass: u8,
    width: u32,
    height: u32,
}

impl Adam7Iterator {
    pub fn new(width: u32, height: u32) -> Adam7Iterator {
        let mut this = Adam7Iterator {
            line: 0,
            lines: 0,
            line_width: 0,
            current_pass: 1,
            width,
            height,
        };
        this.init_pass();
        this
    }

    /// Calculates the bounds of the current pass
    fn init_pass(&mut self) {
        let w = f64::from(self.width);
        let h = f64::from(self.height);
        let (line_mul, line_off, samp_mul, samp_off) = {
            let PassConstants {
                line_mul,
                line_off,
                samp_mul,
                samp_off,
            } = PASS_CONSTANTS[self.current_pass as usize - 1];
            (
                line_mul as f64,
                line_off as f64,
                samp_mul as f64,
                samp_off as f64,
            )
        };
        let (line_width, lines) = ((w - samp_off) / samp_mul, (h - line_off) / line_mul);
        self.line_width = line_width.ceil() as u32;
        self.lines = lines.ceil() as u32;
        self.line = 0;
    }
}

/// Iterates over `Adam7Info`s.
impl Iterator for Adam7Iterator {
    type Item = Adam7Info;
    fn next(&mut self) -> Option<Self::Item> {
        if self.line < self.lines && self.line_width > 0 {
            let this_line = self.line;
            self.line += 1;
            Some(Adam7Info {
                pass: self.current_pass,
                line: this_line,
                width: self.line_width,
            })
        } else if self.current_pass < 7 {
            self.current_pass += 1;
            self.init_pass();
            self.next()
        } else {
            None
        }
    }
}

fn subbyte_pixels(scanline: &[u8], bits_pp: usize) -> impl Iterator<Item = u8> + '_ {
    (0..scanline.len() * 8)
        .step_by(bits_pp)
        .map(move |bit_idx| {
            let byte_idx = bit_idx / 8;

            // sub-byte samples start in the high-order bits
            let rem = 8 - bit_idx % 8 - bits_pp;

            match bits_pp {
                // evenly divides bytes
                1 => (scanline[byte_idx] >> rem) & 1,
                2 => (scanline[byte_idx] >> rem) & 3,
                4 => (scanline[byte_idx] >> rem) & 15,
                _ => unreachable!(),
            }
        })
}

/// Given `row_stride`, interlace `info`, and bits-per-pixel, produce an iterator of bit positions
/// of pixels to copy from the input scanline to the image buffer.  The positions are expressed as
/// bit offsets from position (0,0) in the frame that is currently being decoded.
fn expand_adam7_bits(
    row_stride_in_bytes: usize,
    info: &Adam7Info,
    bits_pp: usize,
) -> impl Iterator<Item = usize> {
    let line_no = info.line as usize;
    let pass = info.pass;
    let interlaced_width = info.width as usize;

    let (line_mul, line_off, samp_mul, samp_off) = {
        // The pass values range from 1 to 7, need to adjust them to start
        // from 0 to 6.
        let PassConstants {
            line_mul,
            line_off,
            samp_mul,
            samp_off,
        } = PASS_CONSTANTS[pass as usize - 1];
        (
            line_mul as usize,
            line_off as usize,
            samp_mul as usize,
            samp_off as usize,
        )
    };

    // the equivalent line number in progressive scan
    let prog_line = line_mul * line_no + line_off;
    let line_start = prog_line * row_stride_in_bytes * 8;

    (0..interlaced_width)
        .map(move |i| i * samp_mul + samp_off)
        .map(move |i| i * bits_pp)
        .map(move |bits_offset| bits_offset + line_start)
}

/// Copies pixels from `interlaced_row` into the right location in `img`.
///
/// First bytes of `img` should belong to the top-left corner of the currently decoded frame.
///
/// `img_row_stride` specifies an offset in bytes between subsequent rows of `img`.
/// This can be the width of the current frame being decoded, but this is not required - a bigger
/// stride may be useful if the frame being decoded is a sub-region of `img`.
///
/// `interlaced_row` and `interlace_info` typically come from
/// [crate::decoder::Reader::next_interlaced_row], but this is not required.  In particular, before
/// calling `expand_interlaced_row` one may need to expand the decoded row, so that its format and
/// `bits_per_pixel` matches that of `img`.  Note that in initial Adam7 passes the `interlaced_row`
/// may contain less pixels that the width of the frame being decoded (e.g. it contains only 1/8th
/// of pixels in the initial pass).
///
/// Example:
///
/// ```
/// use png::{expand_interlaced_row, Adam7Info};
/// let info = Adam7Info::new(5, 0, 4);  // 1st line of 5th pass has 4 pixels.
/// let mut img = vec![0; 8 * 8];
/// let row = vec![1, 2, 3, 4];
/// expand_interlaced_row(&mut img, 8, &row, &info, 8);
/// assert_eq!(&img, &[
///     0, 0, 0, 0, 0, 0, 0, 0,
///     0, 0, 0, 0, 0, 0, 0, 0,
///     1, 0, 2, 0, 3, 0, 4, 0,  // <= this is where the 1st line of 5s appears
///     0, 0, 0, 0, 0, 0, 0, 0,  //    in the schematic drawing of the passes at
///     0, 0, 0, 0, 0, 0, 0, 0,  //    https://en.wikipedia.org/wiki/Adam7_algorithm
///     0, 0, 0, 0, 0, 0, 0, 0,
///     0, 0, 0, 0, 0, 0, 0, 0,
///     0, 0, 0, 0, 0, 0, 0, 0,
/// ]);
/// ```
pub fn expand_pass(
    img: &mut [u8],
    img_row_stride: usize,
    interlaced_row: &[u8],
    interlace_info: &Adam7Info,
    bits_per_pixel: u8,
) {
    let bits_pp = bits_per_pixel as usize;

    let bit_indices = expand_adam7_bits(img_row_stride, interlace_info, bits_pp);

    if bits_pp < 8 {
        for (pos, px) in bit_indices.zip(subbyte_pixels(interlaced_row, bits_pp)) {
            let rem = 8 - pos % 8 - bits_pp;
            img[pos / 8] |= px << rem as u8;
        }
    } else {
        let bytes_pp = bits_pp / 8;

        for (bitpos, px) in bit_indices.zip(interlaced_row.chunks(bytes_pp)) {
            for (offset, val) in px.iter().enumerate() {
                img[bitpos / 8 + offset] = *val;
            }
        }
    }
}

// TODO: Export this function in lib.rs.
// Should we have a separate function, or can we use an enum to determine whether
// to use default or splat?
//
// |width_in_pixels| is only needed for bit splat expand, but currently it's
// required as a parameter for both splat expand types. Would it be possible to
// pass the width only when it's a bit splat expand? Or is there an alternative
// way to retrieve the width without passing it explicitly?
pub fn splat_expand_pass(
    img: &mut [u8],
    img_row_stride: usize,
    interlaced_row: &[u8],
    interlace_info: &Adam7Info,
    bits_per_pixel: u8,
    width_in_pixels: usize,
) {
    let bits_pp = bits_per_pixel as usize;

    let bit_indices = expand_adam7_bits(img_row_stride, interlace_info, bits_pp);

    if bits_pp < 8 {
        for (pos, px) in bit_indices.zip(subbyte_pixels(interlaced_row, bits_pp)) {
            bit_splat_expand(
                img,
                img_row_stride * 8,
                px,
                interlace_info,
                bits_pp,
                pos,
                width_in_pixels * bits_pp,
            );
        }
    } else {
        let bytes_pp = bits_pp / 8;

        for (bitpos, px) in bit_indices.zip(interlaced_row.chunks(bytes_pp)) {
            let start_byte = bitpos / 8;
            byte_splat_expand(
                img,
                img_row_stride,
                px,
                interlace_info,
                bytes_pp,
                start_byte,
            );
        }
    }
}

// TODO: Can we eliminate the duplicate code in |bit_splat_expand| and |byte_splat_expand|?
// |stride| is bit-count here, unlike in |byte_splat_expand|, where it is byte-count.
fn bit_splat_expand(
    img: &mut [u8],
    stride: usize,
    px: u8,
    info: &Adam7Info,
    bits_pp: usize,
    bit_pos: usize,
    img_width_in_bits: usize,
) {
    assert!(
        img_width_in_bits <= stride,
        "The image width should be less than or equal to the row length."
    );
    let height = ((img.len() * 8 - bit_pos) as f32 / stride as f32).ceil() as usize;
    let pass_const = &PASS_CONSTANTS[info.pass as usize - 1];
    let (x_repeat, y_repeat) = (
        pass_const.x_repeat() as usize,
        pass_const.y_repeat() as usize,
    );
    for i in 0..min(y_repeat, height) {
        let offset = bit_pos + i * stride;
        let max_fill = min((img_width_in_bits - bit_pos % stride) / bits_pp, x_repeat);
        copy_nbits_mtimes(img, offset, bits_pp, px, max_fill);
    }
}

fn byte_splat_expand(
    img: &mut [u8],
    stride: usize,
    px: &[u8],
    info: &Adam7Info,
    bytes_pp: usize,
    start_byte: usize,
) {
    let height = ((img.len() - start_byte) as f32 / stride as f32).ceil() as usize;
    let pass_const = &PASS_CONSTANTS[info.pass as usize - 1];
    let (x_repeat, y_repeat) = (
        pass_const.x_repeat() as usize,
        pass_const.y_repeat() as usize,
    );
    for i in 0..min(y_repeat, height) {
        let offset = start_byte + i * stride;
        let max_fill = min((stride - start_byte % stride) / bytes_pp, x_repeat);
        copy_nbytes_mtimes(&mut img[offset..], px, bytes_pp, max_fill);
    }
}

fn copy_nbits_mtimes(img: &mut [u8], bit_pos: usize, bits_pp: usize, px: u8, m: usize) {
    for i in 0..m {
        let pos = bit_pos + i * bits_pp;
        let rem = 8 - pos % 8 - bits_pp;
        let byte_pos = pos / 8;
        let mask = ((1 << bits_pp) - 1) << rem;
        img[byte_pos] = img[byte_pos] & !mask;
        img[byte_pos] |= px << rem as u8;
    }
}

fn copy_nbytes_mtimes(dst: &mut [u8], src: &[u8], n: usize, m: usize) {
    for dst in dst[..n * m].chunks_exact_mut(n) {
        dst.copy_from_slice(src);
    }
}

#[test]
fn test_adam7() {
    /*
        1646
        7777
        5656
        7777
    */
    let it = Adam7Iterator::new(4, 4);
    let passes: Vec<_> = it.collect();
    assert_eq!(
        &*passes,
        &[
            Adam7Info {
                pass: 1,
                line: 0,
                width: 1
            },
            Adam7Info {
                pass: 4,
                line: 0,
                width: 1
            },
            Adam7Info {
                pass: 5,
                line: 0,
                width: 2
            },
            Adam7Info {
                pass: 6,
                line: 0,
                width: 2
            },
            Adam7Info {
                pass: 6,
                line: 1,
                width: 2
            },
            Adam7Info {
                pass: 7,
                line: 0,
                width: 4
            },
            Adam7Info {
                pass: 7,
                line: 1,
                width: 4
            }
        ]
    );
}

#[test]
fn test_splat_expand_pass_subbyte() {
    /*
      [
        10, 00, 00, 01, 11, 10, 01, 11,
        00, 01, 10, 11, 00, 01, 10, 11,
        00, 11, 01, 10, 10, 01, 11, 00,
        11, 10, 01, 00, 11, 10, 01, 00,
        01, 00, 10, 01, 10, 10, 11, 11,
        00, 01, 10, 11, 00, 01, 10, 11,
        11, 11, 10, 10, 01, 01, 00, 00,
        11, 10, 01, 00, 11, 10, 01, 00,
      ]
    */

    let img = &mut [0u8; 16];
    let width = 8;
    let bits_pp = 2;
    let stride = (width * bits_pp as usize) / 8;
    let mut it = Adam7Iterator::new(width as u32, 8);

    let expected_img = &mut [0u8; 16];

    // First pass
    splat_expand_pass(
        img,
        stride,
        &[0b10000000u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    copy_nbytes_mtimes(expected_img, &[0b10101010], 1, 16);

    assert_eq!(img, expected_img);

    // Second pass
    splat_expand_pass(
        img,
        stride,
        &[0b11000000u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    for i in (1..16).step_by(2) {
        expected_img[i] = 0b11111111u8;
    }

    assert_eq!(img, expected_img);

    // Third pass
    splat_expand_pass(
        img,
        stride,
        &[0b01100000u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    copy_nbytes_mtimes(&mut expected_img[8..], &[0b01010101, 0b10101010], 2, 4);

    assert_eq!(img, expected_img);

    // Fourth pass
    splat_expand_pass(
        img,
        stride,
        &[0b00010000u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b10110000u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    copy_nbytes_mtimes(expected_img, &[0b10100000, 0b11110101], 2, 4);
    copy_nbytes_mtimes(&mut expected_img[8..], &[0b01011010, 0b10101111], 2, 4);

    assert_eq!(img, expected_img);

    // Fifth pass
    splat_expand_pass(
        img,
        stride,
        &[0b00011011u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b11100100u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    copy_nbytes_mtimes(&mut expected_img[4..], &[0b00000101, 0b10101111], 2, 2);
    copy_nbytes_mtimes(&mut expected_img[12..], &[0b11111010, 0b01010000], 2, 2);

    assert_eq!(img, expected_img);

    // Sixth pass
    splat_expand_pass(
        img,
        stride,
        &[0b00011011u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b11100100u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b00011011u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b11100100u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    copy_nbytes_mtimes(expected_img, &[0b10000001, 0b11100111], 2, 2);
    copy_nbytes_mtimes(&mut expected_img[4..], &[0b00110110, 0b10011100], 2, 2);
    copy_nbytes_mtimes(&mut expected_img[8..], &[0b01001001, 0b10101111], 2, 2);
    copy_nbytes_mtimes(&mut expected_img[12..], &[0b11111010, 0b01010000], 2, 2);

    assert_eq!(img, expected_img);

    // Seventh pass
    splat_expand_pass(
        img,
        stride,
        &[0b00011011u8, 0b00011011u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b11100100u8, 0b11100100u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b00011011u8, 0b00011011u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    splat_expand_pass(
        img,
        stride,
        &[0b11100100u8, 0b11100100u8],
        &it.next().unwrap(),
        bits_pp,
        width,
    );
    copy_nbytes_mtimes(&mut expected_img[2..], &[0b00011011u8, 0b00011011u8], 2, 1);
    copy_nbytes_mtimes(&mut expected_img[6..], &[0b11100100u8, 0b11100100u8], 2, 1);
    copy_nbytes_mtimes(&mut expected_img[10..], &[0b00011011u8, 0b00011011u8], 2, 1);
    copy_nbytes_mtimes(&mut expected_img[14..], &[0b11100100u8, 0b11100100u8], 2, 1);

    assert_eq!(img, expected_img);
}

#[test]
fn test_splat_expand_pass_within_8x8() {
    /*
       [ 11, 12, 13, 14,
         21, 22, 23, 24,
         31, 32, 33, 34,
         41, 42, 43, 44,
         51, 52, 53, 54,
         61, 62, 63, 64
       ]
    */

    let actual_img = &mut vec![0u8; 24];
    let expected_img = &mut vec![0u8; 24];
    let bp_pixel = 8;
    let width = 4;
    let img_row_stride = width;

    let mut it = Adam7Iterator::new(width as u32, 6);

    // First pass
    expected_img[0..4].copy_from_slice(&vec![11, 11, 11, 11]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 0, 5);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[11],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // Third pass.  (Not "second pass" because for a 4-pixel-wide image `Adam7Iterator` will skip the no-op 2nd pass.)
    expected_img[16..20].copy_from_slice(&vec![51, 51, 51, 51]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 4, 1);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[51],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // Fourth pass
    expected_img[0..4].copy_from_slice(&vec![11, 11, 13, 13]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 0, 3);
    expected_img[16..20].copy_from_slice(&vec![51, 51, 53, 53]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 4, 1);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[13],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[53],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // Fifth pass
    expected_img[8..12].copy_from_slice(&vec![31, 31, 33, 33]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 2, 1);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[31, 33],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // Sixth pass
    expected_img[0..4].copy_from_slice(&vec![11, 12, 13, 14]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 0, 1);
    expected_img[8..12].copy_from_slice(&vec![31, 32, 33, 34]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 2, 1);
    expected_img[16..20].copy_from_slice(&vec![51, 52, 53, 54]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 4, 1);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[12, 14],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[32, 34],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[52, 54],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // Seventh pass
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[21, 22, 23, 24],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[41, 42, 43, 44],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[61, 62, 63, 64],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );

    let test_img = &mut create_test_img(4, 6);
    assert_eq!(actual_img, test_img);
}

// TODO: Write at least three test cases for multi-byte/bit pixel scenarios in splat_expand_pass,
// ensuring that the expected image is obtained after the seventh pass without verifying all
// intermediate passes.

#[test]
fn test_splat_expand_pass() {
    /*
        [
            11, 12, 13, 14, 15, 16, 17, 18, 19,
            21, 22, 23, 24, 25, 26, 27, 28, 29,
            31, 32, 33, 34, 35, 36, 37, 38, 39,
            41, 42, 43, 44, 45, 46, 47, 48, 49,
            51, 52, 53, 54, 55, 56, 57, 58, 59,
            61, 62, 63, 64, 65, 66, 67, 68, 69,
            71, 72, 73, 74, 75, 76, 77, 78, 79,
            81, 82, 83, 84, 85, 86, 87, 88, 89
        ]
    */

    let actual_img = &mut vec![0u8; 72];
    let expected_img = &mut vec![0u8; 72];
    let bp_pixel = 8;
    let width = 9;
    let img_row_stride = width;
    let mut it = Adam7Iterator::new(width as u32, 8);

    // After first pass
    expected_img[0..9].copy_from_slice(&vec![11, 11, 11, 11, 11, 11, 11, 11, 19]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 0, 7);
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[11, 19],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // After second pass
    expected_img[0..9].copy_from_slice(&vec![11, 11, 11, 11, 15, 15, 15, 15, 19]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 0, 7);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[15],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // After third pass
    expected_img[36..45].copy_from_slice(&vec![51, 51, 51, 51, 55, 55, 55, 55, 59]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 4, 3);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[51, 55, 59],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    assert_eq!(actual_img, expected_img);

    // After fourth pass
    expected_img[0..9].copy_from_slice(&vec![11, 11, 13, 13, 15, 15, 17, 17, 19]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 0, 3);
    expected_img[36..45].copy_from_slice(&vec![51, 51, 53, 53, 55, 55, 57, 57, 59]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 4, 3);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[13, 17],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[53, 57],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );

    assert_eq!(actual_img, expected_img);

    // Fifth pass
    expected_img[18..27].copy_from_slice(&vec![31, 31, 33, 33, 35, 35, 37, 37, 39]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 2, 1);
    expected_img[54..63].copy_from_slice(&vec![71, 71, 73, 73, 75, 75, 77, 77, 79]);
    repeat_nth_row_mtimes(expected_img, img_row_stride, 6, 1);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[31, 33, 35, 37, 39],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[71, 73, 75, 77, 79],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );

    assert_eq!(actual_img, expected_img);

    // Sixth pass
    let test_img = &mut create_test_img(9, 8);
    repeat_nth_row_mtimes(test_img, img_row_stride, 0, 1);
    repeat_nth_row_mtimes(test_img, img_row_stride, 2, 1);
    repeat_nth_row_mtimes(test_img, img_row_stride, 4, 1);
    repeat_nth_row_mtimes(test_img, img_row_stride, 6, 1);

    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[12, 14, 16, 18],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[32, 34, 36, 38],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[52, 54, 56, 58],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[72, 74, 76, 78],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );

    assert_eq!(actual_img, test_img);

    // Seventh pass
    let test_img = &mut create_test_img(9, 8);
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[21, 22, 23, 24, 25, 26, 27, 28, 29],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[41, 42, 43, 44, 45, 46, 47, 48, 49],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[61, 62, 63, 64, 65, 66, 67, 68, 69],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );
    splat_expand_pass(
        actual_img,
        img_row_stride,
        &[81, 82, 83, 84, 85, 86, 87, 88, 89],
        &it.next().unwrap(),
        bp_pixel,
        width,
    );

    assert_eq!(actual_img, test_img);
}

#[test]
fn test_multibyte_expand_pass() {
    // width - odd, height - odd
    multibyte_expand_pass_test_helper(3, 11, 16);
    // width - odd, height - even
    multibyte_expand_pass_test_helper(7, 14, 24);
    // width - even, height - odd
    multibyte_expand_pass_test_helper(4, 9, 8);
    // width - even, height - even
    multibyte_expand_pass_test_helper(6, 12, 48);
}

#[test]
fn test_multibit_expand_pass() {
    // width - odd, height - odd
    multibit_expand_pass_test_helper(3, 11, 1);
    // width - odd, height - even
    multibit_expand_pass_test_helper(7, 14, 2);
    // width - even, height - odd
    multibit_expand_pass_test_helper(4, 9, 4);
    // width - even, height - even
    multibit_expand_pass_test_helper(6, 12, 2);
}

#[test]
fn test_subbyte_pixels() {
    let scanline = &[0b10101010, 0b10101010];

    let pixels = subbyte_pixels(scanline, 1).collect::<Vec<_>>();
    assert_eq!(pixels.len(), 16);
    assert_eq!(pixels, [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]);
}

#[test]
fn test_expand_adam7_bits() {
    let width = 32;
    let bits_pp = 1;
    let stride = width / 8;
    let info = |pass, line, img_width| create_adam7_info_for_tests(pass, line as u32, img_width);

    let expected = |offset: usize, step: usize, count: usize| {
        (0..count)
            .map(move |i| step * i + offset)
            .collect::<Vec<_>>()
    };

    for line_no in 0..8 {
        let start = 8 * line_no * width;

        assert_eq!(
            expand_adam7_bits(stride, &info(1, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 8, 4)
        );

        let start = start + 4;

        assert_eq!(
            expand_adam7_bits(stride, &info(2, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 8, 4)
        );

        let start = (8 * line_no + 4) * width;

        assert_eq!(
            expand_adam7_bits(stride, &info(3, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 4, 8)
        );
    }

    for line_no in 0..16 {
        let start = 4 * line_no * width + 2;

        assert_eq!(
            expand_adam7_bits(stride, &info(4, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 4, 8)
        );

        let start = (4 * line_no + 2) * width;

        assert_eq!(
            expand_adam7_bits(stride, &info(5, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 2, 16)
        )
    }

    for line_no in 0..32 {
        let start = 2 * line_no * width + 1;

        assert_eq!(
            expand_adam7_bits(stride, &info(6, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 2, 16),
            "line_no: {}",
            line_no
        );

        let start = (2 * line_no + 1) * width;

        assert_eq!(
            expand_adam7_bits(stride, &info(7, line_no, width), bits_pp).collect::<Vec<_>>(),
            expected(start, 1, 32)
        );
    }
}

#[test]
fn test_expand_adam7_bits_independent_row_stride() {
    let pass = 1;
    let line_no = 1;
    let width = 32;
    let bits_pp = 8;
    let info = create_adam7_info_for_tests;

    {
        let stride = width;
        assert_eq!(
            expand_adam7_bits(stride, &info(pass, line_no, width), bits_pp).collect::<Vec<_>>(),
            vec![2048, 2112, 2176, 2240],
        );
    }

    {
        let stride = 10000;
        assert_eq!(
            expand_adam7_bits(stride, &info(pass, line_no, width), bits_pp).collect::<Vec<_>>(),
            vec![640000, 640064, 640128, 640192],
        );
    }
}

#[test]
fn test_expand_pass_subbyte() {
    let mut img = [0u8; 8];
    let width = 8;
    let stride = width / 8;
    let bits_pp = 1;
    let info = create_adam7_info_for_tests;

    expand_pass(&mut img, stride, &[0b10000000], &info(1, 0, width), bits_pp);
    assert_eq!(img, [0b10000000u8, 0, 0, 0, 0, 0, 0, 0]);

    expand_pass(&mut img, stride, &[0b10000000], &info(2, 0, width), bits_pp);
    assert_eq!(img, [0b10001000u8, 0, 0, 0, 0, 0, 0, 0]);

    expand_pass(&mut img, stride, &[0b11000000], &info(3, 0, width), bits_pp);
    assert_eq!(img, [0b10001000u8, 0, 0, 0, 0b10001000, 0, 0, 0]);

    expand_pass(&mut img, stride, &[0b11000000], &info(4, 0, width), bits_pp);
    assert_eq!(img, [0b10101010u8, 0, 0, 0, 0b10001000, 0, 0, 0]);

    expand_pass(&mut img, stride, &[0b11000000], &info(4, 1, width), bits_pp);
    assert_eq!(img, [0b10101010u8, 0, 0, 0, 0b10101010, 0, 0, 0]);

    expand_pass(&mut img, stride, &[0b11110000], &info(5, 0, width), bits_pp);
    assert_eq!(img, [0b10101010u8, 0, 0b10101010, 0, 0b10101010, 0, 0, 0]);

    expand_pass(&mut img, stride, &[0b11110000], &info(5, 1, width), bits_pp);
    assert_eq!(
        img,
        [0b10101010u8, 0, 0b10101010, 0, 0b10101010, 0, 0b10101010, 0]
    );

    expand_pass(&mut img, stride, &[0b11110000], &info(6, 0, width), bits_pp);
    assert_eq!(
        img,
        [0b11111111u8, 0, 0b10101010, 0, 0b10101010, 0, 0b10101010, 0]
    );

    expand_pass(&mut img, stride, &[0b11110000], &info(6, 1, width), bits_pp);
    assert_eq!(
        img,
        [0b11111111u8, 0, 0b11111111, 0, 0b10101010, 0, 0b10101010, 0]
    );

    expand_pass(&mut img, stride, &[0b11110000], &info(6, 2, width), bits_pp);
    assert_eq!(
        img,
        [0b11111111u8, 0, 0b11111111, 0, 0b11111111, 0, 0b10101010, 0]
    );

    expand_pass(&mut img, stride, &[0b11110000], &info(6, 3, width), bits_pp);
    assert_eq!(
        [0b11111111u8, 0, 0b11111111, 0, 0b11111111, 0, 0b11111111, 0],
        img
    );

    expand_pass(&mut img, stride, &[0b11111111], &info(7, 0, width), bits_pp);
    assert_eq!(
        [
            0b11111111u8,
            0b11111111,
            0b11111111,
            0,
            0b11111111,
            0,
            0b11111111,
            0
        ],
        img
    );

    expand_pass(&mut img, stride, &[0b11111111], &info(7, 1, width), bits_pp);
    assert_eq!(
        [
            0b11111111u8,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0,
            0b11111111,
            0
        ],
        img
    );

    expand_pass(&mut img, stride, &[0b11111111], &info(7, 2, width), bits_pp);
    assert_eq!(
        [
            0b11111111u8,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0
        ],
        img
    );

    expand_pass(&mut img, stride, &[0b11111111], &info(7, 3, width), bits_pp);
    assert_eq!(
        [
            0b11111111u8,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111,
            0b11111111
        ],
        img
    );
}

#[cfg(test)]
fn create_adam7_info_for_tests(pass: u8, line: u32, img_width: usize) -> Adam7Info {
    let width = {
        let img_height = 8;
        Adam7Iterator::new(img_width as u32, img_height)
            .filter(|info| info.pass == pass)
            .map(|info| info.width)
            .next()
            .unwrap()
    };

    Adam7Info { pass, line, width }
}

#[cfg(test)]
fn create_test_img(width: usize, height: usize) -> Vec<u8> {
    let mut img = vec![0u8; width * height];
    for i in 0..height {
        for j in 0..width {
            img[i * width + j] = ((i + 1) * 10 + (j + 1)) as u8;
        }
    }

    img
}

#[cfg(test)]
fn repeat_nth_row_mtimes(img: &mut Vec<u8>, width: usize, n: usize, m: usize) {
    let src_start = n * width;
    let src_end = (n + 1) * width;

    for i in 0..m {
        let copy_start = src_start + (i + 1) * width;
        img.copy_within(src_start..src_end, copy_start);
    }
}

#[cfg(test)]
fn multibyte_expand_pass_test_helper(width: usize, height: usize, bits_pp: u8) {
    use rand::Rng;

    let bytes_pp = bits_pp / 8;
    let size = width * height * bytes_pp as usize;
    let splat_img = &mut vec![0u8; size];
    let non_splat_img = &mut vec![0u8; size];
    let img_row_stride = width * bytes_pp as usize;
    let mut rng = rand::thread_rng();

    for it in Adam7Iterator::new(width as u32, height as u32).into_iter() {
        let interlace_size = it.width * (bytes_pp as u32);
        let interlaced_row: Vec<_> = (0..interlace_size).map(|_| rng.gen::<u8>()).collect();
        splat_expand_pass(
            splat_img,
            img_row_stride,
            &interlaced_row,
            &it,
            bits_pp,
            width,
        );

        expand_pass(non_splat_img, img_row_stride, &interlaced_row, &it, bits_pp);
    }

    assert_eq!(splat_img, non_splat_img);
}

#[cfg(test)]
fn multibit_expand_pass_test_helper(width: usize, height: usize, bits_pp: u8) {
    use rand::Rng;

    let stride_bits = width * bits_pp as usize;
    let stride = (stride_bits + 7) / 8;
    let size = stride * height;
    let splat_img = &mut vec![0u8; size];
    let non_splat_img = &mut vec![0u8; size];
    let mut rng = rand::thread_rng();

    for it in Adam7Iterator::new(width as u32, height as u32).into_iter() {
        let interlace_bits = it.width * bits_pp as u32;
        let interlace_size = (interlace_bits + 7) / 8;
        let interlaced_row: Vec<_> = (0..interlace_size).map(|_| rng.gen::<u8>()).collect();
        splat_expand_pass(splat_img, stride, &interlaced_row, &it, bits_pp, width);

        expand_pass(non_splat_img, stride, &interlaced_row, &it, bits_pp);
    }

    assert_eq!(splat_img, non_splat_img);
}
