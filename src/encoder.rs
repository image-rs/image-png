#![allow(non_snake_case)]

use crate::{
    chunk, filter::filter, AnimationControl, BitDepth, BlendOp, BytesPerPixel, ColorType,
    Compression, DisposeOp, FilterType, FrameControl, Info, ScaledFloat, SourceChromaticities,
    Time,
};
// use chunk::ChunkType;

use io::Write;
use std::{error, fmt, io, result};

use deflate::write::ZlibEncoder;

pub type Result<T = ()> = result::Result<T, EncodingError>;

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
    NotAnAnimation,
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
            Self::NotAnAnimation => write!(fmt, "The `animated` method hasn't been called"),
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

// pub struct ChunkWriter<W: Write> {
//     w: W,
//     // chunk: ChunkType,
//     fdAT: bool,
//     written: usize,
//     to_write: usize,
//     crc: Crc32,
//     /// chunk on next
//     con: bool,
//     pub seq_num: u32,
// }

// impl<W: Write> ChunkWriter<W> {
//     // Find a way to make it relative to the chunktype
//     pub const MAX_BYTES: usize = u32::MAX as usize >> 3;

//     // pub fn new(w: W, chunk: ChunkType, to_write: usize) -> Self {
//     pub fn new(w: W, fdAT: bool, to_write: usize) -> Self {
//         Self {
//             w,
//             // chunk,
//             fdAT,
//             written: 0,
//             to_write,
//             crc: Crc32::new(),
//             con: true,
//             seq_num: 0,
//         }
//     }

//     // pub fn current_chunk(&self) -> &ChunkType {
//     //     &self.chunk
//     // }

//     // pub fn reset(&mut self, chunk: ChunkType, to_write: usize) -> Result<()> {
//     pub fn set_fdAT(&mut self, fdAT: bool) -> Result<()> {
//         // another chunk could be added:
//         // if no data was written then it's not an error
//         if self.to_write == 0 || self.written == 0 {
//             // self.chunk = chunk;
//             self.fdAT = fdAT;
//             Ok(())
//         } else {
//             Err(EncodingError::WrongDataSize(self.to_write, 0))
//         }
//     }

//     pub fn set_to_write(&mut self, to_write: usize) -> Result<()> {
//         // another chunk could be added:
//         // if no data was written then it's not an error
//         if self.to_write == 0 || self.written == 0 {
//             // self.chunk = chunk;
//             self.to_write = to_write;
//             Ok(())
//         } else {
//             Err(EncodingError::WrongDataSize(self.to_write, 0))
//         }
//     }

//     pub fn into_inner(self) -> W {
//         // Maybe this should return a Result
//         self.w
//     }
// }

// impl<W: Write> Write for ChunkWriter<W> {
//     fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
//         if self.to_write == 0 {
//             Ok(0) // should this be err?
//         } else {
//             if self.con {
//                 self.con = false;
//                 let bytes = Self::MAX_BYTES.min(self.to_write).to_be_bytes();
//                 self.w.write_all(&bytes)?;
//                 self.crc.update(&bytes);

//                 // self.w.write_all(&self.chunk.0)?;
//                 if self.fdAT {
//                     self.w.write_all(&chunk::fdAT.0)?;
//                     self.w.write_all(&self.seq_num.to_be_bytes())?;
//                     self.seq_num += 1;
//                     self.crc.update(&chunk::fdAT.0);
//                     self.crc.update(&self.seq_num.to_be_bytes());
//                 } else {
//                     self.w.write_all(&chunk::IDAT.0)?;
//                     self.crc.update(&chunk::IDAT.0);
//                 }
//             }
//             let write = buf
//                 .len()
//                 .min(Self::MAX_BYTES - self.written)
//                 .min(self.to_write);

//             let written = self.w.write(&buf[..write])?;
//             self.written += written;
//             self.to_write -= written;

//             self.crc.update(&buf[..written]);

//             if self.written % Self::MAX_BYTES == 0 || self.to_write == 0 {
//                 let mut crc = Crc32::new();
//                 std::mem::swap(&mut crc, &mut self.crc);

//                 self.w.write_all(&crc.finalize().to_be_bytes())?;
//                 self.con = true;
//             }
//             Ok(written)
//         }
//     }

//     fn flush(&mut self) -> io::Result<()> {
//         // should flush write the chunk ???
//         Ok(())
//     }
// }

struct ChunkWriter<W: Write> {
    w: W,
    fdAT: bool,
    pub seq_num: u32,
}

impl<W: Write> ChunkWriter<W> {
    pub fn new(w: W) -> Self {
        Self {
            w,
            fdAT: false,
            seq_num: 0,
        }
    }

    pub fn get_mut(&mut self) -> &mut W {
        &mut self.w
    }

    pub fn set_fdAT(&mut self, set: bool) {
        self.fdAT = set;
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

/// PNG Animation Encoder
pub struct Encoder<W: io::Write> {
    w: W,
    info: Info,
    sep_def_img: bool,
}

impl<W: Write> Encoder<W> {
    pub fn new(w: W, width: u32, height: u32) -> Self {
        let mut info = Info::default();
        info.width = width;
        info.height = height;
        Self {
            w,
            info,
            sep_def_img: false,
        }
    }

    pub fn animated(&mut self, frames: u32, repeat: u32) -> Result {
        if frames == 0 {
            Err(EncodingError::ZeroFrames)
        } else {
            self.info.animation_control = Some(AnimationControl {
                num_frames: frames,
                num_plays: repeat,
            });
            self.info.frame_control = Some(FrameControl {
                width: self.info.width,
                height: self.info.height,
                ..Default::default()
            });
            Ok(())
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

    pub fn set_default_frame_delay(&mut self, delay_num: u16, delay_den: u16) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::NotAnAnimation)
        } else {
            let fctl = self.info.frame_control.as_mut().unwrap();
            fctl.delay_num = delay_num;
            fctl.delay_den = delay_den;
            Ok(())
        }
    }

    pub fn set_default_dispose_op(&mut self, dispose: DisposeOp) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::NotAnAnimation)
        } else {
            let fctl = self.info.frame_control.as_mut().unwrap();
            fctl.dispose_op = dispose;
            Ok(())
        }
    }

    pub fn set_default_blend_op(&mut self, blend: BlendOp) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::NotAnAnimation)
        } else {
            let fctl = self.info.frame_control.as_mut().unwrap();
            fctl.blend_op = blend;
            Ok(())
        }
    }

    // This could be a parameter of .animated(..., sep_def_image: bool)
    pub fn with_separate_default_image(&mut self) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::NotAnAnimation)
        } else {
            self.sep_def_img = true;
            Ok(())
        }
    }

    pub fn write_header(mut self) -> Result<FrameEncoder<W>> {
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

        Ok(FrameEncoder::new(self.w, info, self.sep_def_img))
    }
}

struct AnimationData {
    fctl: FrameControl,
    sep_def: bool,
}

pub struct FrameEncoder<W: Write> {
    w: Option<io::BufWriter<ChunkWriter<W>>>,
    // here the FrameControl is used for costomized data
    info: Info,
    anim_data: Option<AnimationData>,
    total: u32,
    written: u32,
}

impl<W: Write> FrameEncoder<W> {
    fn new(w: W, info: Info, sep_def: bool) -> Self {
        let frames;
        let anim_data = if let Some(actl) = info.animation_control {
            let fctl = info.frame_control.unwrap();
            frames = actl.num_frames;
            Some(AnimationData { fctl, sep_def })
        } else {
            frames = 1;
            None
        };

        Self {
            w: Some(io::BufWriter::with_capacity(
                u32::MAX as usize >> 3,
                ChunkWriter::new(w),
            )),
            info,
            anim_data,
            total: frames,
            written: 0,
        }
    }

    pub fn set_dispose_op(&mut self, op: DisposeOp) -> Result {
        if let Some(ref mut anim_data) = self.anim_data {
            anim_data.fctl.dispose_op = op;
            Ok(())
        } else {
            Err(EncodingError::NotAnAnimation)
        }
    }

    pub fn set_blend_op(&mut self, op: BlendOp) -> Result {
        if let Some(ref mut anim_data) = self.anim_data {
            anim_data.fctl.blend_op = op;
            Ok(())
        } else {
            Err(EncodingError::NotAnAnimation)
        }
    }

    pub fn set_frame_delay(&mut self, delay_den: u16, delay_num: u16) -> Result {
        if let Some(ref mut anim_data) = self.anim_data {
            anim_data.fctl.delay_den = delay_den;
            anim_data.fctl.delay_num = delay_num;
            Ok(())
        } else {
            Err(EncodingError::NotAnAnimation)
        }
    }

    pub fn set_frame_position(&mut self, x: u32, y: u32, width: u32, height: u32) -> Result<()> {
        if let Some(ref mut anim_data) = self.anim_data {
            if x + width > self.info.width || y + height > self.info.height {
                Err(EncodingError::FrameOutOfBounds(
                    self.info.width,
                    self.info.height,
                ))
            } else {
                anim_data.fctl.x_offset = x;
                anim_data.fctl.y_offset = y;
                anim_data.fctl.width = width;
                anim_data.fctl.height = height;
                Ok(())
            }
        } else {
            Err(EncodingError::NotAnAnimation)
        }
    }

    pub fn remaining(&self) -> u32 {
        self.total - self.written
    }

    pub fn write_frame_header<'a>(&'a mut self) -> Result<Writer<'a, W>> {
        if let Some(ref mut w) = self.w {
            if self.written == self.total {
                Err(EncodingError::AlreadyFinished)
            } else if let Some(ref mut anim_data) = self.anim_data {
                if anim_data.sep_def {
                    anim_data.sep_def = false;
                    Ok(Writer::new(w, self.info.clone()))
                } else {
                    if self.written == 1 {
                        w.get_mut().set_fdAT(true);
                    }
                    // Reset the frame metadata
                    anim_data.fctl.sequence_number = w.get_mut().seq_num;
                    w.get_mut().seq_num += 1;
                    anim_data.fctl.encode(w.get_mut().get_mut())?;
                    anim_data.fctl = self.info.frame_control.unwrap();

                    self.written += 1;
                    Ok(Writer::new(w, self.info.clone()))
                }
            } else {
                self.written = 1;
                Ok(Writer::new(w, self.info.clone()))
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
                    w.get_mut().get_mut(),
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
pub struct Writer<'a, W: Write> {
    w: Option<ZlibWriter<&'a mut io::BufWriter<ChunkWriter<W>>>>,
    // info: Info,
    total: usize,
}

impl<'a, W: Write> Writer<'a, W> {
    fn new(w: &'a mut io::BufWriter<ChunkWriter<W>>, info: Info) -> Self {
        let (width, height) = info
            .frame_control
            .map(|fctl| (fctl.width, fctl.height))
            .unwrap_or((info.width, info.height));
        let in_len = info.raw_row_length_from_width(width) - 1;
        let total = in_len * height as usize;
        // w.set_to_write(total).unwrap();

        let zlib_writer = ZlibWriter::new(
            w,
            in_len,
            info.compression,
            info.bpp_in_prediction(),
            info.filter,
        );

        Self {
            w: Some(zlib_writer),
            // info,
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
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

impl<'a, W: Write> Drop for Writer<'a, W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}

// --- For backwards compatibility ---

impl<'a, W: Write> Writer<'a, W> {
    /// Create an stream writer.
    ///
    /// This allows you create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chuck
    /// size.
    ///
    /// This borrows the writer. This preserves it which allows manually
    /// appending additional chunks after the image data has been written
    pub fn stream_writer<'b>(&'b mut self) -> StreamWriter<'a, 'b, W> {
        StreamWriter {
            w: Exclusive::MutRef(self),
        }
    }

    /// Create a stream writer with custom buffer size.
    ///
    /// See [`stream_writer`].
    ///
    /// [`stream_writer`]: #fn.stream_writer
    pub fn stream_writer_with_size<'b>(&'b mut self, _: usize) -> StreamWriter<'a, 'b, W> {
        StreamWriter {
            w: Exclusive::MutRef(self),
        }
    }

    /// Turn this into a stream writer for image data.
    ///
    /// This allows you create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chuck
    /// size.
    pub fn into_stream_writer(self) -> StreamWriter<'a, 'static, W> {
        StreamWriter {
            w: Exclusive::Owned(self),
        }
    }

    /// Turn this into a stream writer with custom buffer size.
    ///
    /// See [`into_stream_writer`].
    ///
    /// [`into_stream_writer`]: #fn.into_stream_writer
    pub fn into_stream_writer_with_size(self, _: usize) -> StreamWriter<'a, 'static, W> {
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

pub struct StreamWriter<'b: 'a, 'a, W: Write> {
    w: Exclusive<'a, Writer<'b, W>>,
}

impl<'b: 'a, 'a, W: Write> Write for StreamWriter<'b, 'a, W> {
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

    // #[test]
    // fn test() {
    //     let file = File::create("a.png").unwrap();
    //     let mut enc = Encoder::new(file, 11, 11);
    //     enc.set_color(ColorType::RGB);
    //     enc.set_depth(BitDepth::Eight);
    //     let mut ctrl = enc.write_header().unwrap();
    //     {
    //         let mut wrt = ctrl.write_frame_header().unwrap();
    //         wrt.write_image_data(
    //             &[0, 255, 127, 255, 0]
    //                 .iter()
    //                 .copied()
    //                 .cycle()
    //                 .take(11 * 11 * 3)
    //                 .collect::<Vec<u8>>(),
    //         )
    //         .unwrap();
    //         wrt.finish().unwrap();
    //     }
    //     ctrl.finish().unwrap();
    // }

    // #[test]
    // fn test_2() {
    //     let file = File::create("b.png").unwrap();
    //     let mut enc = Encoder::new(file, 11, 11);
    //     enc.set_color(ColorType::RGB);
    //     enc.set_depth(BitDepth::Eight);
    //     enc.animated(10, 0).unwrap();
    //     let mut ctrl = enc.write_header().unwrap();
    //     let data = &[0, 255, 127, 255, 0]
    //         .iter()
    //         .copied()
    //         .cycle()
    //         .take(11 * 11 * 3 * 10)
    //         .collect::<Vec<u8>>();
    //     let mut iter = data.chunks_exact(11 * 11 * 3);
    //     while let Ok(mut wrt) = ctrl.write_frame_header() {
    //         wrt.write_image_data(iter.next().unwrap()).unwrap();
    //         wrt.finish().unwrap();
    //     }
    //     ctrl.finish().unwrap();
    // }

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
                    encoder
                        .write_frame_header()
                        .unwrap()
                        .write_image_data(&buf)
                        .unwrap();
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

                    let mut img_wrt = encoder.write_frame_header().unwrap();
                    let mut stream_writer = img_wrt.stream_writer();

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
                writer
                    .write_frame_header()
                    .unwrap()
                    .write_image_data(&indexed_data)
                    .unwrap();
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
        let result = png_writer
            .write_frame_header()
            .unwrap()
            .write_image_data(image.as_ref());

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
            encoder
                .write_header()?
                .write_frame_header()
                .unwrap()
                .write_image_data(&pixel)?;

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
            encoder
                .write_header()?
                .write_frame_header()
                .unwrap()
                .write_image_data(&pixel)?;

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
