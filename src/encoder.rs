extern crate deflate;

use std::io::Write;
use std::mem;

use image_core::{ColorType, ImageError, ImageResult};

use chunk;
use crc::Crc32;
use common::{Info, PngColorType, BitDepth};
use filter::{FilterType, filter};
use traits::WriteBytesExt;
use utils;

/// PNG Encoder
pub struct PngEncoder<W: Write> {
    w: W,
    info: Info,
}

impl<W: Write> PngEncoder<W> {
    pub fn new(w: W, width: u32, height: u32, color_type: ColorType) -> ImageResult<Self> {
        let (color_type, bits) = match color_type {
            ColorType::L8 => (PngColorType::Grayscale, BitDepth::Eight),
            ColorType::LA8 => (PngColorType::GrayscaleAlpha, BitDepth::Eight),
            ColorType::RGB8 => (PngColorType::RGB, BitDepth::Eight),
            ColorType::RGBA8 => (PngColorType::RGBA, BitDepth::Eight),
            ColorType::L16 => (PngColorType::Grayscale, BitDepth::Sixteen),
            ColorType::LA16 => (PngColorType::GrayscaleAlpha, BitDepth::Sixteen),
            ColorType::RGB16 => (PngColorType::RGB, BitDepth::Sixteen),
            ColorType::RGBA16 => (PngColorType::RGBA, BitDepth::Sixteen),
            _ => return Err(ImageError::UnsupportedColor(color_type)),
        };

        let mut info = Info::default();
        info.width = width;
        info.height = height;
        info.bit_depth = bits;
        info.color_type = color_type;
        Self::write_header(w, info)
    }

    /// Create a `PngEncoder` that writes data with a specified output color type.
    pub fn with_output_colortype(
        w: W,
        width: u32,
        height: u32,
        input_colortype: ColorType,
        output_colortype: PngColorType,
        output_bit_depth: BitDepth) -> ImageResult<Self>
    {
        if (input_colortype.bits_per_pixel() == 16) != (output_bit_depth == BitDepth::Sixteen) {
            return Err(ImageError::UnsupportedFeature(
                "Cannot convert between 16 bpp formats and non-16 bpp formats".to_owned()));
        }

        let input_png_colortype = match input_colortype {
            ColorType::L8 => PngColorType::Grayscale,
            ColorType::LA8 => PngColorType::GrayscaleAlpha,
            ColorType::RGB8 => PngColorType::RGB,
            ColorType::RGBA8 => PngColorType::RGBA,
            ColorType::L16 => PngColorType::Grayscale,
            ColorType::LA16 => PngColorType::GrayscaleAlpha,
            ColorType::RGB16 => PngColorType::RGB,
            ColorType::RGBA16 => PngColorType::RGBA,
            _ => return Err(ImageError::UnsupportedColor(input_colortype)),
        };

        if input_png_colortype != output_colortype {
            return Err(ImageError::UnsupportedFeature(
                format!("Cannot convert between {:?} and {:?}", input_colortype, output_colortype)));
        }

        let mut info = Info::default();
        info.width = width;
        info.height = height;
        info.bit_depth = output_bit_depth;
        info.color_type = output_colortype;
        Self::write_header(w, info)
    }

    /// *CURRENTLY UNIMPLEMENTED* function to create a `PngEncoder` with the specified palette.
    pub fn with_palette(_w: W, _width: u32, _height: u32, _palette: &[u8]) {
        unimplemented!();
    }

    fn write_header(mut w: W, info: Info) -> ImageResult<Self> {
        try!(w.write_all(&[137, 80, 78, 71, 13, 10, 26, 10]));
        let mut data = [0; 13];
        try!((&mut data[..]).write_be(info.width));
        try!((&mut data[4..]).write_be(info.height));
        data[8] = info.bit_depth as u8;
        data[9] = info.color_type as u8;
        data[12] = if info.interlaced { 1 } else { 0 };

        let mut encoder = Self { w, info };
        try!(encoder.write_chunk(chunk::IHDR, &data));
        Ok(encoder)
    }

    pub fn write_chunk(&mut self, name: [u8; 4], data: &[u8]) -> ImageResult<()> {
        try!(self.w.write_be(data.len() as u32));
        try!(self.w.write_all(&name));
        try!(self.w.write_all(data));
        let mut crc = Crc32::new();
        crc.update(&name);
        crc.update(data);
        try!(self.w.write_be(crc.checksum()));
        Ok(())
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> ImageResult<()> {
        let bpp = self.info.bytes_per_pixel();

        // The constructors guarantee that the input color type will always be 8 or 16 bits per
        // sample and further that the output bit depth (info.bit_depth) will only be <8 if the
        // input bit depth is equal to 8.
        let in_len = self.info.width as usize
            * self.info.color_type.samples()
            * if self.info.bit_depth == BitDepth::Sixteen { 2 } else { 1 };

        let data_size = in_len * self.info.height as usize;
        if data_size != data.len() {
            let message = format!("wrong data size, expected {} got {}", data_size, data.len());
            return Err(ImageError::InvalidData(message));
        }

        let raw_len = self.info.raw_row_length() - 1;
        let mut prev = vec![0; raw_len];
        let mut current = vec![0; raw_len];

        let mut zlib = deflate::write::ZlibEncoder::new(Vec::new(), deflate::Compression::Fast);
        let filter_method = FilterType::Sub;
        for line in data.chunks(in_len) {
            if (self.info.bit_depth as u8) < 8 {
                utils::pack_bits(line, &mut current, self.info.bit_depth as u8);
           } else {
                current.copy_from_slice(&line);
            }
            try!(zlib.write_all(&[filter_method as u8]));
            filter(filter_method, bpp, &prev, &mut current);
            try!(zlib.write_all(&current));
            mem::swap(&mut prev, &mut current);
        }
        self.write_chunk(chunk::IDAT, &try!(zlib.finish()))
    }
}

impl<W: Write> Drop for PngEncoder<W> {
    fn drop(&mut self) {
        let _ = self.write_chunk(chunk::IEND, &[]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use image_core::ImageDecoder;

    use crate::PngDecoder;

    extern crate rand;
    extern crate glob;

    use self::rand::Rng;
    use std::{io, cmp};
    use std::io::Write;
    use std::fs::File;

    #[test]
    fn roundtrip() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png").unwrap().map(|r| r.unwrap()) {
                if path.file_name().unwrap().to_str().unwrap().starts_with("x") {
                    // x* files are expected to fail to decode
                    continue;
                }
                // Decode image
                let decoder = PngDecoder::new(File::open(path).unwrap()).unwrap();
                let dimensions = decoder.dimensions();
                let color_type = decoder.colortype();
                let mut buf = vec![0; decoder.total_bytes() as usize];
                decoder.read_image(&mut buf).unwrap();

                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: self::rand::thread_rng(),
                        w: &mut out
                    };

                    let mut encoder = PngEncoder::new(&mut wrapper,
                                                      dimensions.0 as u32,
                                                      dimensions.1 as u32,
                                                      color_type).unwrap();
                    encoder.write_image_data(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = PngDecoder::new(&*out).unwrap();
                let mut buf2 = vec![0; decoder.total_bytes() as usize];
                decoder.read_image(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn expect_error_on_wrong_image_len() -> ImageResult<()> {
        use std::io::Cursor;

        let width = 10;
        let height = 10;

        let output = vec![0u8; 1024];
        let writer = Cursor::new(output);
        let mut encoder = PngEncoder::new(writer, width as u32, height as u32, ColorType::RGB8).unwrap();

        let correct_image_size = width * height * 3;
        let image = vec![0u8; correct_image_size + 1];
        let result = encoder.write_image_data(image.as_ref());
        assert!(result.is_err());

        Ok(())
    }

    /// A Writer that only writes a few bytes at a time
    struct RandomChunkWriter<'a, R: Rng, W: Write + 'a> {
        rng: R,
        w: &'a mut W
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
