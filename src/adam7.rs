//! Utility functions related to handling of
//! [the Adam7 algorithm](https://en.wikipedia.org/wiki/Adam7_algorithm).

/// Describes which stage of
/// [the Adam7 algorithm](https://en.wikipedia.org/wiki/Adam7_algorithm)
/// applies to a decoded row.
///
/// See also [Reader.next_interlaced_row](crate::decoder::Reader::next_interlaced_row).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Adam7Info {
    pub(crate) pass: u8,
    pub(crate) line: u32,
    pub(crate) width: u32,
}

/// The index of a bit in the image buffer.
///
/// We do not use a pure `usize` to avoid overflows on 32-bit targets.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct BitPostion {
    byte: usize,
    /// [0..8)
    bit: u8,
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
        let (line_width, lines) = match self.current_pass {
            1 => (w / 8.0, h / 8.0),
            2 => ((w - 4.0) / 8.0, h / 8.0),
            3 => (w / 4.0, (h - 4.0) / 8.0),
            4 => ((w - 2.0) / 4.0, h / 4.0),
            5 => (w / 2.0, (h - 2.0) / 4.0),
            6 => ((w - 1.0) / 2.0, h / 2.0),
            7 => (w, (h - 1.0) / 2.0),
            _ => unreachable!(),
        };
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

fn subbyte_values<const N: usize>(
    scanline: &[u8],
    bit_pos: [u8; N],
    mask: u8,
) -> impl Iterator<Item = u8> + '_ {
    (scanline.iter().copied()).flat_map(move |value| bit_pos.map(|n| (value >> n) & mask))
}

/// Given `row_stride`, interlace `info`, and bits-per-pixel, produce an iterator of bit positions
/// of pixels to copy from the input scanline to the image buffer.  The positions are expressed as
/// bit offsets from position (0,0) in the frame that is currently being decoded.
///
/// This should only be used with `bits_pp < 8`.
fn expand_adam7_bits(
    row_stride_in_bytes: usize,
    info: &Adam7Info,
    bits_pp: u8,
) -> impl Iterator<Item = BitPostion> {
    debug_assert!(bits_pp == 1 || bits_pp == 2 || bits_pp == 4);
    let (line_mul, line_off, samp_mul, samp_off) = match info.pass {
        1 => (8, 0, 8, 0),
        2 => (8, 0, 8, 4),
        3 => (8, 4, 4, 0),
        4 => (4, 0, 4, 2),
        5 => (4, 2, 2, 0),
        6 => (2, 0, 2, 1),
        7 => (2, 1, 1, 0),
        _ => {
            // `Adam7Info.pass` is a non-`pub`lic field.  `InterlaceInfo` is expected
            // to maintain an invariant that `pass` is valid.
            panic!("Invalid `Adam7Info.pass`");
        }
    };

    // the equivalent line number in progressive scan
    let prog_line = line_mul * info.line as usize + line_off;
    let byte_start = prog_line * row_stride_in_bytes;

    // In contrast to `subbyte_values` we *must* be precise with our length here.
    (0..u64::from(info.width))
        // Bounded by u32::MAX * 8 * 4 + 16 so does not overflow `u64`.
        .map(move |i| (i * samp_mul + samp_off) * u64::from(bits_pp))
        .map(move |i| BitPostion {
            // Bounded by the buffer size which already exists.
            byte: byte_start + (i / 8) as usize,
            bit: i as u8 % 8,
        })
}

fn expand_adam7_bytes(
    row_stride_in_bytes: usize,
    info: &Adam7Info,
    bytes_pp: u8,
) -> impl Iterator<Item = usize> {
    let (line_mul, line_off, samp_mul, samp_off) = match info.pass {
        1 => (8, 0, 8, 0),
        2 => (8, 0, 8, 4),
        3 => (8, 4, 4, 0),
        4 => (4, 0, 4, 2),
        5 => (4, 2, 2, 0),
        6 => (2, 0, 2, 1),
        7 => (2, 1, 1, 0),
        _ => {
            // `Adam7Info.pass` is a non-`pub`lic field.  `InterlaceInfo` is expected
            // to maintain an invariant that `pass` is valid.
            panic!("Invalid `Adam7Info.pass`");
        }
    };

    // the equivalent line number in progressive scan
    let prog_line = line_mul * info.line as usize + line_off;
    let byte_start = prog_line * row_stride_in_bytes;

    (0..u64::from(info.width))
        .map(move |i| (i * samp_mul + samp_off) * u64::from(bytes_pp))
        // Bounded by the allocated buffer size so must fit in `usize`
        .map(move |i| i as usize + byte_start)
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
    match bits_per_pixel {
        // Note: for 1, 2, 4 multiple runs through the iteration will access the same byte in `img`
        // so we can not iterate over `&mut u8` values. A better strategy would write multiple bit
        // groups in one go. This would then also not be as bounds-check heavy?
        1 => {
            const BIT_POS_1: [u8; 8] = [7, 6, 5, 4, 3, 2, 1, 0];
            let bit_indices = expand_adam7_bits(img_row_stride, interlace_info, 1);
            for (pos, px) in bit_indices.zip(subbyte_values(interlaced_row, BIT_POS_1, 0b1)) {
                let shift = 8 - bits_per_pixel - pos.bit;
                img[pos.byte] |= px << shift;
            }
        }
        2 => {
            const BIT_POS_2: [u8; 4] = [6, 4, 2, 0];
            let bit_indices = expand_adam7_bits(img_row_stride, interlace_info, 2);

            for (pos, px) in bit_indices.zip(subbyte_values(interlaced_row, BIT_POS_2, 0b11)) {
                let shift = 8 - bits_per_pixel - pos.bit;
                img[pos.byte] |= px << shift;
            }
        }
        4 => {
            const BIT_POS_4: [u8; 2] = [4, 0];
            let bit_indices = expand_adam7_bits(img_row_stride, interlace_info, 4);

            for (pos, px) in bit_indices.zip(subbyte_values(interlaced_row, BIT_POS_4, 0b1111)) {
                let shift = 8 - bits_per_pixel - pos.bit;
                img[pos.byte] |= px << shift;
            }
        }
        // While caught by the below loop, we special case this for codegen. The effects are
        // massive when the compiler uses the constant chunk size in particular for this case where
        // no more copy_from_slice is being issued by everything happens in the register alone.
        8 => {
            let byte_indices = expand_adam7_bytes(img_row_stride, interlace_info, 1);

            for (bytepos, &px) in byte_indices.zip(interlaced_row) {
                img[bytepos] = px;
            }
        }
        16 => {
            let byte_indices = expand_adam7_bytes(img_row_stride, interlace_info, 2);

            for (bytepos, px) in byte_indices.zip(interlaced_row.chunks(2)) {
                img[bytepos..][..2].copy_from_slice(px);
            }
        }
        _ => {
            debug_assert!(bits_per_pixel % 8 == 0);
            let bytes_pp = bits_per_pixel / 8;
            let byte_indices = expand_adam7_bytes(img_row_stride, interlace_info, bytes_pp);

            for (bytepos, px) in byte_indices.zip(interlaced_row.chunks(bytes_pp.into())) {
                img[bytepos..][..px.len()].copy_from_slice(px);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_subbyte_pixels() {
        const BIT_POS_1: [u8; 8] = [7, 6, 5, 4, 3, 2, 1, 0];

        let scanline = &[0b10101010, 0b10101010];
        let pixels = subbyte_values(scanline, BIT_POS_1, 1).collect::<Vec<_>>();

        assert_eq!(pixels.len(), 16);
        assert_eq!(pixels, [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]);
    }

    #[test]
    fn test_expand_adam7_bits() {
        let width = 32;
        let bits_pp = 1;
        let stride = width / 8;
        let info =
            |pass, line, img_width| create_adam7_info_for_tests(pass, line as u32, img_width);

        let expected = |offset: usize, step: usize, count: usize| {
            (0..count)
                .map(move |i| step * i + offset)
                .map(|i| BitPostion {
                    byte: i / 8,
                    bit: (i % 8) as u8,
                })
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
        let info = create_adam7_info_for_tests;

        {
            let stride = width;
            assert_eq!(
                expand_adam7_bytes(stride, &info(pass, line_no, width), 1).collect::<Vec<_>>(),
                [2048, 2112, 2176, 2240].map(|n| n / 8),
            );
        }

        {
            let stride = 10000;
            assert_eq!(
                expand_adam7_bytes(stride, &info(pass, line_no, width), 1).collect::<Vec<_>>(),
                [640000, 640064, 640128, 640192].map(|n| n / 8),
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

    /// This test ensures that `expand_pass` works correctly on 32-bit machines, even when the indices
    /// of individual bits in the target buffer can not be expressed within a `usize`. We ensure that
    /// the output buffer size is between `usize::MAX / 8` and `isize::MAX` to trigger that condition.
    #[cfg(target_pointer_width = "32")]
    #[test]
    fn regression_overflow_adam7_bitfill() {
        fn multibyte_expand_pass_test_helper(width: usize, height: usize, bits_pp: u8) -> Vec<u8> {
            let bytes_pp = bits_pp / 8;
            let size = width * height * bytes_pp as usize;
            let mut img = vec![0u8; size];
            let img_row_stride = width * bytes_pp as usize;

            for it in Adam7Iterator::new(width as u32, height as u32).into_iter() {
                if it.pass != 7 {
                    continue;
                }

                if it.line != (width / 2) as u32 - 1 {
                    continue;
                }

                let interlace_size = it.width * (bytes_pp as u32);
                // Ensure that expanded pixels are never empty bits. This differentiates the written bits
                // from the initial bits that are all zeroed.
                let interlaced_row: Vec<_> = (0..interlace_size).map(|_| 0xff).collect();

                expand_pass(&mut img, img_row_stride, &interlaced_row, &it, bits_pp);
            }

            img
        }

        let expanded = multibyte_expand_pass_test_helper(1 << 14, 1 << 14, 32);
        assert_eq!(*expanded.last().unwrap(), 0xff);
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
}
