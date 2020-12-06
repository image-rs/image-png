#![allow(dead_code)]
#![allow(non_camel_case_types)]

use super::{EncodingError, Result, ZlibWriter};
use crate::{
    chunk, AnimationControl, BitDepth, BlendOp, ColorType, Compression, DisposeOp, FilterType,
    FrameControl, Info, ScaledFloat, SourceChromaticities, Time,
};

use io::{BufWriter, Write};
use std::io;

// TODO: adjust
const MAX_CHUNK_LEN: u32 = u32::MAX >> 2;

/// PNG Animation Encoder
pub struct AnimationEncoder<W: io::Write> {
    w: W,
    info: Info,
    buf_size: usize,
}

impl<W: Write> AnimationEncoder<W> {
    pub fn new(w: W, width: u32, height: u32, frames: u32, repeat: u32) -> Result<Self> {
        if frames == 0 {
            return Err(EncodingError::ZeroFrames);
        }

        let mut info = Info::default();
        info.width = width;
        info.height = height;
        info.animation_control = Some(AnimationControl {
            num_frames: frames,
            num_plays: repeat,
        });
        info.frame_control = Some(FrameControl {
            sequence_number: 0,
            width,
            height,
            x_offset: 0,
            y_offset: 0,
            ..Default::default()
        });
        Ok(Self {
            w,
            info,
            buf_size: MAX_CHUNK_LEN as usize,
        })
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

    pub fn set_default_frame_delay(&mut self, delay_num: u16, delay_den: u16) {
        let fctl = self.info.frame_control.as_mut().unwrap();
        fctl.delay_num = delay_num;
        fctl.delay_den = delay_den;
    }

    pub fn set_default_dispose_op(&mut self, dispose: DisposeOp) {
        let fctl = self.info.frame_control.as_mut().unwrap();
        fctl.dispose_op = dispose;
    }

    pub fn set_default_blend_op(&mut self, blend: BlendOp) {
        let fctl = self.info.frame_control.as_mut().unwrap();
        fctl.blend_op = blend;
    }

    pub fn write_header(mut self, separate_default_image: bool) -> Result<FrameEncoder<W>> {
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

        Ok(FrameEncoder::new(
            self.w,
            info,
            self.buf_size,
            separate_default_image,
        ))
    }
}

struct ChunkWriter<W: Write> {
    w: W,
    fdAT: bool,
    seq_num: u32,
}

impl<W: Write> ChunkWriter<W> {
    pub fn seq_num(&self) -> u32 {
        self.seq_num
    }

    pub fn set_fdAT(&mut self, set: bool) {
        self.fdAT = set;
    }

    pub fn get_mut(&mut self) -> &mut W {
        &mut self.w
    }
}

impl<W: Write> Write for ChunkWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.fdAT {
            chunk::fdAT_encode(&mut self.w, self.seq_num, buf)?;
            self.seq_num += 1;
        } else {
            chunk::encode_chunk(&mut self.w, chunk::IDAT, buf)?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub struct FrameEncoder<W: Write> {
    w: Option<io::BufWriter<ChunkWriter<W>>>,
    // here the FrameControl is used for costomized data
    info: Info,
    fctl: FrameControl,
    total: u32,
    written: u32,
    sep_def: bool,
}

impl<W: Write> FrameEncoder<W> {
    fn get_fctl_mut(&mut self) -> &mut FrameControl {
        self.info.frame_control.get_or_insert(self.fctl)
    }

    fn inner_mut(w: &mut io::BufWriter<ChunkWriter<W>>) -> &mut W {
        w.get_mut().get_mut()
    }

    fn new(w: W, mut info: Info, buf_size: usize, sep_def: bool) -> Self {
        let chunk_writer = ChunkWriter {
            w,
            seq_num: 0,
            fdAT: false,
        };
        let buf_writer = BufWriter::with_capacity(buf_size, chunk_writer);

        Self {
            w: Some(buf_writer),
            total: info.animation_control.unwrap().num_frames,
            fctl: info.frame_control.take().unwrap(),
            info,
            sep_def,
            written: 0,
        }
    }

    pub fn set_dispose_op(&mut self, op: DisposeOp) {
        self.get_fctl_mut().dispose_op = op
    }

    pub fn set_blend_op(&mut self, op: BlendOp) {
        self.get_fctl_mut().blend_op = op
    }

    pub fn set_frame_delay(&mut self, delay_den: u16, delay_num: u16) {
        let fctl = self.get_fctl_mut();
        fctl.delay_den = delay_den;
        fctl.delay_num = delay_num;
    }

    pub fn set_frame_position(&mut self, x: u32, y: u32, width: u32, height: u32) -> Result<()> {
        if x + width > self.info.width || y + height > self.info.height {
            Err(EncodingError::FrameOutOfBounds(
                self.info.width,
                self.info.height,
            ))
        } else {
            let fctl = self.get_fctl_mut();
            fctl.x_offset = x;
            fctl.y_offset = y;
            fctl.width = width;
            fctl.height = height;
            Ok(())
        }
    }

    pub fn remaining(&self) -> u32 {
        self.total - self.written
    }

    pub fn write_frame_header<'a>(&'a mut self) -> Result<FrameWriter<'a, W>> {
        let fctl = self.info.frame_control.get_or_insert(self.fctl);
        if let Some(ref mut w) = self.w {
            if self.written == self.total {
                Err(EncodingError::AlreadyFinished)
            } else if self.sep_def {
                self.sep_def = false;
                Ok(FrameWriter::new(w, self.info.clone()))
            } else {
                fctl.sequence_number = w.get_mut().seq_num;
                w.get_mut().seq_num += 1;
                chunk::fcTL_encode(Self::inner_mut(w), *fctl)?;
                self.written += 1;
                Ok(FrameWriter::new(w, self.info.clone()))
            }
        } else {
            Err(EncodingError::AlreadyFinished)
        }
    }

    pub fn finish(&mut self) -> Result<()> {
        if let Some(mut w) = self.w.take() {
            if self.remaining() > 0 {
                self.w = Some(w);
                Err(EncodingError::MissingFrames(self.remaining()))
            } else {
                Ok(chunk::encode_chunk(
                    Self::inner_mut(&mut w),
                    chunk::IEND,
                    &[],
                )?)
            }
        } else {
            Err(EncodingError::AlreadyFinished)
        }
    }
}

impl<W: Write> Drop for FrameEncoder<W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}

/// PNG writer
pub struct FrameWriter<'a, W: Write> {
    w: Option<ZlibWriter<&'a mut io::BufWriter<ChunkWriter<W>>>>,
    info: Info,
    total: usize,
}

impl<'a, W: Write> FrameWriter<'a, W> {
    fn new(w: &'a mut io::BufWriter<ChunkWriter<W>>, info: Info) -> Self {
        let fctl = info.frame_control.unwrap();
        let in_len = info.raw_row_length_from_width(fctl.width) - 1;
        let total = in_len * fctl.height as usize;
        let zlib_writer = ZlibWriter::new(
            w,
            in_len,
            info.compression,
            info.bpp_in_prediction(),
            info.filter,
        );

        Self {
            w: Some(zlib_writer),
            info,
            total,
        }
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        let w = self.w.as_mut().ok_or(EncodingError::AlreadyFinished)?;

        if self.total < data.len() {
            return Err(EncodingError::WrongDataSize(data.len(), self.total));
        }
        self.total -= data.len();

        w.compress_data(data)?;
        Ok(())
    }

    pub fn set_filter(&mut self, filter: FilterType) -> Result<()> {
        match self.w {
            Some(ref mut w) => Ok(w.filter = filter),
            None => Err(EncodingError::AlreadyFinished),
        }
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
                let w = w.finish()?;
                w.flush()?;
                Ok(w.get_mut().set_fdAT(true))
            }
        } else {
            Ok(())
        }
    }
}

impl<'a, W: Write> Drop for FrameWriter<'a, W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}
