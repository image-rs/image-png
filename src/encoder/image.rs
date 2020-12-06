use super::{EncodingError, Result, ZlibWriter};
use crate::{
    chunk, BitDepth, ColorType, Compression, FilterType, Info, ScaledFloat, SourceChromaticities,
    Time,
};

use io::{BufWriter, Write};
use std::io;

const MAX_CHUNK_LEN: u32 = u32::MAX >> 1;

/// PNG Image Encoder
pub struct ImageEncoder<W: io::Write> {
    w: W,
    info: Info,
    buf_size: usize,
}

impl<W: Write> ImageEncoder<W> {
    pub fn new(w: W, width: u32, height: u32) -> Self {
        let mut info = Info::default();
        info.width = width;
        info.height = height;
        Self {
            w,
            info,
            buf_size: MAX_CHUNK_LEN as usize,
        }
    }

    pub fn set_time(&mut self, time: Time) {
        self.info.time = Some(time);
    }

    pub fn set_palette(&mut self, palette: Vec<u8>) -> Result<()> {
        if palette.is_empty() {
            Err(EncodingError::EmptyPalette)
        } else if palette.len() % 3 != 0 {
            Err(EncodingError::WrongPaletteLength)
        } else {
            self.info.palette = Some(palette);
            Ok(())
        }
    }

    pub fn set_trns(&mut self, trns: Vec<u8>) {
        self.info.trns = Some(trns);
    }

    /// Set the display gamma of the source system on which the image was generated or last edited.
    pub fn set_source_gamma(&mut self, source_gamma: ScaledFloat) {
        self.info.source_gamma = Some(source_gamma);
    }

    /// Set the chromaticities for the source system's display channels (red, green, blue) and the whitepoint
    /// of the source system on which the image was generated or last edited.
    pub fn set_source_chromaticities(&mut self, source_chromaticities: SourceChromaticities) {
        self.info.source_chromaticities = Some(source_chromaticities);
    }

    /// Set the color of the encoded image.
    ///
    /// These correspond to the color types in the png IHDR data that will be written. The length
    /// of the image data that is later supplied must match the color type, otherwise an error will
    /// be emitted.
    pub fn set_color(&mut self, color: ColorType) {
        self.info.color_type = color;
    }

    /// Set the indicated depth of the image data.
    pub fn set_depth(&mut self, depth: BitDepth) {
        self.info.bit_depth = depth;
    }

    /// Set compression parameters.
    ///
    /// Accepts a `Compression` or any type that can transform into a `Compression`. Notably `deflate::Compression` and
    /// `deflate::CompressionOptions` which "just work".
    pub fn set_compression<C: Into<Compression>>(&mut self, compression: C) {
        self.info.compression = compression.into();
    }

    /// Set the used filter type.
    ///
    /// The default filter is [`FilterType::Sub`] which provides a basic prediction algorithm for
    /// sample values based on the previous. For a potentially better compression ratio, at the
    /// cost of more complex processing, try out [`FilterType::Paeth`].
    ///
    /// [`FilterType::Sub`]: enum.FilterType.html#variant.Sub
    /// [`FilterType::Paeth`]: enum.FilterType.html#variant.Paeth
    pub fn set_default_filter(&mut self, filter: FilterType) {
        self.info.filter = filter;
    }

    pub fn set_buffer_size(&mut self, size: usize) {
        self.buf_size = size;
    }

    pub fn write_header(mut self) -> Result<ImageWriter<W>> {
        let info = self.info;

        if info.width == 0 {
            return Err(EncodingError::ZeroWidth);
        }

        if info.height == 0 {
            return Err(EncodingError::ZeroHeight);
        }

        if info.color_type.is_combination_invalid(info.bit_depth) {
            return Err(EncodingError::Invalid(info.bit_depth, info.color_type));
        }

        if info.color_type == ColorType::Indexed && info.palette.is_none() {
            return Err(EncodingError::MissingPalette);
        }

        self.w.write_all(&[137, 80, 78, 71, 13, 10, 26, 10])?;

        info.encode(&mut self.w)?;

        // This doesn't need everything from `info` but the compiler probably knows that
        Ok(ImageWriter::new(self.w, info, self.buf_size))
    }
}

pub(super) struct IDATChunkWriter<W: Write> {
    pub w: W,
}

impl<W: Write> Write for IDATChunkWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        chunk::encode_chunk(&mut self.w, chunk::IDAT, buf)?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// PNG writer
pub struct ImageWriter<W: Write> {
    w: Option<ZlibWriter<io::BufWriter<IDATChunkWriter<W>>>>,
    // info: Info,
    total: usize,
}

impl<W: Write> ImageWriter<W> {
    pub(super) fn new(w: W, info: Info, buf_size: usize) -> Self {
        let in_len = info.raw_row_length() - 1;
        let total = in_len * info.height as usize;

        let chunk_writer = IDATChunkWriter { w };
        let buf_writer = BufWriter::with_capacity(buf_size, chunk_writer);
        let zlib_writer = ZlibWriter::new(
            buf_writer,
            in_len,
            info.compression,
            info.bpp_in_prediction(),
            info.filter,
        );

        Self {
            w: Some(zlib_writer),
            total,
        }
    }

    // TODO: I don't think this would cause any damage
    pub fn set_filter(&mut self, filter: FilterType) -> Result<()> {
        match self.w {
            Some(ref mut w) => Ok(w.filter = filter),
            None => Err(EncodingError::AlreadyFinished),
        }
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        let w = self.w.as_mut().ok_or(EncodingError::AlreadyFinished)?;

        if self.total < data.len() {
            return Err(EncodingError::WrongDataSize(data.len(), self.total));
        }
        self.total -= data.len();
        Ok(w.compress_data(data)?)
    }

    pub fn finish(&mut self) -> Result<()> {
        if let Some(w) = self.w.take() {
            if self.total > 0 {
                // Replace the writer has it should be possible to add the missing data
                self.w = Some(w);
                Err(EncodingError::WrongDataSize(0, self.total))
            } else if w.buffered() != 0 {
                Err(EncodingError::WrongDataSize(w.buffered(), 0))
            } else {
                // into_inner calls flush
                let mut w = w.finish()?.into_inner().map_err(io::Error::from)?.w;
                Ok(chunk::encode_chunk(&mut w, chunk::IEND, &[])?)
            }
        } else {
            Ok(())
        }
    }
}

impl<W: Write> Drop for ImageWriter<W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}
