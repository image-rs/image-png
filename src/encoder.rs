mod animation;
mod image;

pub use animation::{AnimationEncoder, FrameEncoder, FrameWriter};

use io::Write;
use std::{error, fmt, io, result};

use deflate::write::ZlibEncoder;

use crate::common::{BitDepth, BytesPerPixel, ColorType, Compression};
use crate::filter::{filter, FilterType};

pub type Result<T> = result::Result<T, EncodingError>;

#[derive(Debug)]
pub enum EncodingError {
    IoError(io::Error),
    ZeroWidth,
    ZeroHeight,
    ZeroFrames,
    Invalid(BitDepth, ColorType),
    WrongDataSize(usize, usize),
    AlreadyFinished,
    MissingPalette,
    EmptyPalette,
    WrongPaletteLength,
    MissingFrames(u32),
    FrameOutOfBounds(u32, u32),
}

impl error::Error for EncodingError {
    fn cause(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            EncodingError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for EncodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        match self {
            Self::IoError(err) => write!(fmt, "{}", err),
            Self::ZeroWidth => write!(fmt, "Image width must be greater than zero"),
            Self::ZeroHeight => write!(fmt, "Image height must be greater than zero"),
            Self::ZeroFrames => write!(fmt, "An animation must have at least one frame"),
            Self::Invalid(b, c) => write!(
                fmt,
                "Invalid combination of bit-depth '{:?}' and color type '{:?}'",
                b, c
            ),
            Self::WrongDataSize(d, e) => write!(fmt, "Expected {} bytes, found {} bytes", e, d),
            Self::AlreadyFinished => write!(fmt, "All the image data has been written"),
            Self::MissingPalette => write!(fmt, "Missing palette for indexed image color data"),
            Self::EmptyPalette => write!(fmt, "The provided palette contains no data"),
            Self::WrongPaletteLength => write!(
                fmt,
                "The length of the provided palette isn't a multiple of three"
            ),
            Self::MissingFrames(f) => {
                write!(fmt, "There are {} frames yet to be written", f)
            }
            Self::FrameOutOfBounds(w, h) => write!(
                fmt,
                "The frame size is restricted to an area of {}x{} pixels",
                w, h
            ),
        }
    }
}

impl From<io::Error> for EncodingError {
    fn from(err: io::Error) -> Self {
        Self::IoError(err)
    }
}

impl From<EncodingError> for io::Error {
    fn from(err: EncodingError) -> Self {
        Self::new(io::ErrorKind::Other, err)
    }
}

// pub trait PNGEncoder: Sized {
//     fn set<R, P: Parameter<Self, Result = R>>(&mut self, param: P) -> R {
//         param.set(self)
//     }
// }

// pub trait Parameter<E: PNGEncoder> {
//     type Result;

//     fn set(self, enc: &mut E) -> Self::Result;
// }

struct ZlibWriter<W: Write> {
    curr_buf: Vec<u8>,
    prev_buf: Vec<u8>,
    index: usize,
    zenc: ZlibEncoder<W>,
    bpp: BytesPerPixel,
    pub filter: FilterType,
}

impl<W: Write> ZlibWriter<W> {
    pub fn new(
        w: W,
        buf_len: usize,
        compression: Compression,
        bpp: BytesPerPixel,
        filter: FilterType,
    ) -> Self {
        Self {
            curr_buf: vec![0; buf_len],
            prev_buf: vec![0; buf_len],
            index: 0,
            zenc: ZlibEncoder::new(w, compression),
            bpp,
            filter,
        }
    }

    pub fn compress_data(&mut self, data: &[u8]) -> io::Result<()> {
        let mut start = 0;
        if self.index > 0 {
            if self.index + data.len() < self.curr_buf.len() {
                start = data.len();
                self.curr_buf[self.index..self.index + data.len()].copy_from_slice(data);
                self.index += data.len();
            } else {
                start = self.curr_buf.len() - self.index;

                self.curr_buf[self.index..].copy_from_slice(&data[..start]);
                self.index = 0;
                self.filter()?;
            }
        }

        let mut iter = data[start..].chunks_exact(self.curr_buf.len());
        for line in &mut iter {
            // assert_eq!(self.index, 0);
            self.curr_buf.copy_from_slice(line);
            self.filter()?;
        }

        let rem = iter.remainder();
        self.curr_buf[..rem.len()].copy_from_slice(rem);
        self.index += rem.len();

        Ok(())
    }

    /// Filters the data before compressing
    fn filter(&mut self) -> io::Result<()> {
        let prev = self.curr_buf.clone();
        filter(self.filter, self.bpp, &self.prev_buf, &mut self.curr_buf);
        self.prev_buf = prev;

        self.zenc.write_all(&[self.filter as u8])?;
        self.zenc.write_all(&self.curr_buf)
    }

    /// Returns the number of bytes that haven't been compressed and encoded yet
    pub fn buffered(&self) -> usize {
        self.index
    }

    /// Finishes the compression by writing the chunksum at the end, consumes the zlib writer
    /// and returns the inner one
    pub fn finish(self) -> io::Result<W> {
        self.zenc.finish()
    }
}

// --- For backwards compatibility ---

pub use image::ImageEncoder as Encoder;
pub use image::ImageWriter as Writer;

impl<W: Write> Writer<W> {
    /// Create an stream writer.
    ///
    /// This allows you create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chuck
    /// size.
    ///
    /// This borrows the writer. This preserves it which allows manually
    /// appending additional chunks after the image data has been written
    pub fn stream_writer(&mut self) -> StreamWriter<W> {
        StreamWriter {
            w: Exclusive::MutRef(self),
        }
    }

    /// Create a stream writer with custom buffer size.
    ///
    /// See [`stream_writer`].
    ///
    /// [`stream_writer`]: #fn.stream_writer
    pub fn stream_writer_with_size(&mut self, _: usize) -> StreamWriter<W> {
        StreamWriter {
            w: Exclusive::MutRef(self),
        }
    }

    /// Turn this into a stream writer for image data.
    ///
    /// This allows you create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chuck
    /// size.
    pub fn into_stream_writer(self) -> StreamWriter<'static, W> {
        StreamWriter {
            w: Exclusive::Owned(self),
        }
    }

    /// Turn this into a stream writer with custom buffer size.
    ///
    /// See [`into_stream_writer`].
    ///
    /// [`into_stream_writer`]: #fn.into_stream_writer
    pub fn into_stream_writer_with_size(self, _: usize) -> StreamWriter<'static, W> {
        StreamWriter {
            w: Exclusive::Owned(self),
        }
    }
}

enum Exclusive<'a, T> {
    MutRef(&'a mut T),
    Owned(T),
}

impl<'a, T> std::ops::Deref for Exclusive<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match *self {
            Self::MutRef(&mut ref v) | Self::Owned(ref v) => v,
        }
    }
}

impl<'a, T> std::ops::DerefMut for Exclusive<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::MutRef(v) => v,
            Self::Owned(v) => v,
        }
    }
}

pub struct StreamWriter<'a, W: Write> {
    w: Exclusive<'a, Writer<W>>,
}

impl<'a, W: Write> Write for StreamWriter<'a, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write_image_data(buf)?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(self.w.finish()?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ScaledFloat;

    extern crate glob;

    use rand::{thread_rng, Rng};
    use std::fs::File;
    use std::io::Write;
    use std::{cmp, io};

    #[test]
    fn roundtrip() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png")
                .unwrap()
                .map(|r| r.unwrap())
            {
                if path.file_name().unwrap().to_str().unwrap().starts_with('x') {
                    // x* files are expected to fail to decode
                    continue;
                }
                eprintln!("{}", path.display());
                // Decode image
                let decoder = crate::Decoder::new(File::open(path).unwrap());
                let (info, mut reader) = decoder.read_info().unwrap();
                if info.line_size != 32 {
                    // TODO encoding only works with line size 32?
                    continue;
                }
                let mut buf = vec![0; info.buffer_size()];
                eprintln!("{:?}", info);
                reader.next_frame(&mut buf).unwrap();
                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut out,
                    };

                    let mut encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();
                    encoder.write_image_data(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = crate::Decoder::new(&*out);
                let (info, mut reader) = decoder.read_info().unwrap();
                let mut buf2 = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn roundtrip_stream() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png")
                .unwrap()
                .map(|r| r.unwrap())
            {
                if path.file_name().unwrap().to_str().unwrap().starts_with('x') {
                    // x* files are expected to fail to decode
                    continue;
                }
                // Decode image
                let decoder = crate::Decoder::new(File::open(path).unwrap());
                let (info, mut reader) = decoder.read_info().unwrap();
                if info.line_size != 32 {
                    // TODO encoding only works with line size 32?
                    continue;
                }
                let mut buf = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf).unwrap();
                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut out,
                    };

                    let mut encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();
                    let mut stream_writer = encoder.stream_writer();

                    let mut outer_wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut stream_writer,
                    };

                    outer_wrapper.write_all(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = crate::Decoder::new(&*out);
                let (info, mut reader) = decoder.read_info().unwrap();
                let mut buf2 = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn image_palette() -> Result<()> {
        let samples = 3;
        for &bit_depth in &[1u8, 2, 4, 8] {
            // Do a reference decoding, choose a fitting palette image from pngsuite
            let path = format!("tests/pngsuite/basn3p0{}.png", bit_depth);
            let decoder = crate::Decoder::new(File::open(&path).unwrap());
            let (info, mut reader) = decoder.read_info().unwrap();

            let palette: Vec<u8> = reader.info().palette.clone().unwrap();
            let mut decoded_pixels = vec![0; info.buffer_size()];
            assert_eq!(
                info.width as usize * info.height as usize * samples,
                decoded_pixels.len()
            );
            reader.next_frame(&mut decoded_pixels).unwrap();

            let pixels_per_byte = 8 / usize::from(bit_depth);
            let mut indexed_data = vec![0; decoded_pixels.len() / samples];
            {
                // Retransform the image into palette bits.
                let mut indexes = vec![];
                for color in decoded_pixels.chunks(samples) {
                    let j = palette
                        .chunks(samples)
                        .position(|pcolor| color == pcolor)
                        .unwrap();
                    indexes.push(j as u8);
                }

                let idx_per_byte = indexes.chunks(pixels_per_byte);
                indexed_data.truncate(idx_per_byte.len());
                for (pixels, byte) in idx_per_byte.zip(&mut indexed_data) {
                    let mut shift = 8;
                    for idx in pixels {
                        shift -= bit_depth;
                        *byte |= idx << shift;
                    }
                }
            };

            let mut out = Vec::new();
            {
                let mut encoder = Encoder::new(&mut out, info.width, info.height);
                encoder.set_depth(BitDepth::from_u8(bit_depth).unwrap());
                encoder.set_color(ColorType::Indexed);
                encoder.set_palette(palette.clone()).ok();

                let mut writer = encoder.write_header().unwrap();
                writer.write_image_data(&indexed_data).unwrap();
            }

            // Decode re-encoded image
            let decoder = crate::Decoder::new(&*out);
            let (info, mut reader) = decoder.read_info().unwrap();
            let mut redecoded = vec![0; info.buffer_size()];
            reader.next_frame(&mut redecoded).unwrap();
            // check if the encoded image is ok:
            assert_eq!(decoded_pixels, redecoded);
        }
        Ok(())
    }

    #[test]
    fn expect_error_on_wrong_image_len() -> Result<()> {
        use std::io::Cursor;

        let width = 10;
        let height = 10;

        let output = vec![0u8; 1024];
        let writer = Cursor::new(output);
        let mut encoder = Encoder::new(writer, width as u32, height as u32);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::RGB);
        let mut png_writer = encoder.write_header()?;

        let correct_image_size = width * height * 3;
        let image = vec![0u8; correct_image_size + 1];
        let result = png_writer.write_image_data(image.as_ref());

        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn expect_error_on_empty_image() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let encoder = Encoder::new(&mut writer, 0, 0);
        assert!(encoder.write_header().is_err());

        let encoder = Encoder::new(&mut writer, 100, 0);
        assert!(encoder.write_header().is_err());

        let encoder = Encoder::new(&mut writer, 0, 100);
        assert!(encoder.write_header().is_err());

        Ok(())
    }

    #[test]
    fn expect_error_on_invalid_bit_depth_color_type_combination() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_err());

        Ok(())
    }

    #[test]
    fn can_write_header_with_valid_bit_depth_color_type_combination() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Indexed);
        encoder.set_palette(vec![0, 0, 0]).unwrap();
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Indexed);
        encoder.set_palette(vec![0, 0, 0]).unwrap();
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Indexed);
        encoder.set_palette(vec![0, 0, 0]).unwrap();
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Indexed);
        encoder.set_palette(vec![0, 0, 0]).unwrap();
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::RGB);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::RGBA);
        assert!(encoder.write_header().is_ok());

        Ok(())
    }

    #[test]
    fn all_filters_roundtrip() -> io::Result<()> {
        let pixel: Vec<_> = (0..48).collect();

        let roundtrip = |filter: FilterType| -> io::Result<()> {
            let mut buffer = vec![];
            let mut encoder = Encoder::new(&mut buffer, 4, 4);
            encoder.set_depth(BitDepth::Eight);
            encoder.set_color(ColorType::RGB);
            encoder.set_default_filter(filter);
            encoder.write_header()?.write_image_data(&pixel)?;

            let decoder = crate::Decoder::new(io::Cursor::new(buffer));
            let (info, mut reader) = decoder.read_info()?;
            assert_eq!(info.width, 4);
            assert_eq!(info.height, 4);
            let mut dest = vec![0; pixel.len()];
            reader.next_frame(&mut dest)?;
            assert_eq!(dest, pixel, "Deviation with filter type {:?}", filter);

            Ok(())
        };

        roundtrip(FilterType::NoFilter)?;
        roundtrip(FilterType::Sub)?;
        roundtrip(FilterType::Up)?;
        roundtrip(FilterType::Avg)?;
        roundtrip(FilterType::Paeth)?;

        Ok(())
    }

    #[test]
    fn some_gamma_roundtrip() -> io::Result<()> {
        let pixel: Vec<_> = (0..48).collect();

        let roundtrip = |gamma: Option<ScaledFloat>| -> io::Result<()> {
            let mut buffer = vec![];
            let mut encoder = Encoder::new(&mut buffer, 4, 4);
            encoder.set_depth(BitDepth::Eight);
            encoder.set_color(ColorType::RGB);
            encoder.set_default_filter(FilterType::Avg);
            if let Some(gamma) = gamma {
                encoder.set_source_gamma(gamma);
            }
            encoder.write_header()?.write_image_data(&pixel)?;

            let decoder = crate::Decoder::new(io::Cursor::new(buffer));
            let (info, mut reader) = decoder.read_info()?;
            assert_eq!(
                reader.info().source_gamma,
                gamma,
                "Deviation with gamma {:?}",
                gamma
            );
            assert_eq!(info.width, 4);
            assert_eq!(info.height, 4);
            let mut dest = vec![0; pixel.len()];
            reader.next_frame(&mut dest)?;

            Ok(())
        };

        roundtrip(None)?;
        roundtrip(Some(ScaledFloat::new(0.35)))?;
        roundtrip(Some(ScaledFloat::new(0.45)))?;
        roundtrip(Some(ScaledFloat::new(0.55)))?;
        roundtrip(Some(ScaledFloat::new(0.7)))?;
        roundtrip(Some(ScaledFloat::new(1.0)))?;
        roundtrip(Some(ScaledFloat::new(2.5)))?;

        Ok(())
    }

    /// A Writer that only writes a few bytes at a time
    struct RandomChunkWriter<'a, R: Rng, W: Write + 'a> {
        rng: R,
        w: &'a mut W,
    }

    impl<'a, R: Rng, W: Write + 'a> Write for RandomChunkWriter<'a, R, W> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            // choose a random length to write
            let len = cmp::min(self.rng.gen_range(1, 50), buf.len());

            self.w.write(&buf[0..len])
        }

        fn flush(&mut self) -> io::Result<()> {
            self.w.flush()
        }
    }
}
