#![allow(non_snake_case)]

use crate::chunk;
use crate::common::{
    AnimationControl, BitDepth, BlendOp, BytesPerPixel, ColorType, Compression, DisposeOp,
    FrameControl, Info, ParameterError, ParameterErrorKind, ScaledFloat, SourceChromaticities,
    SrgbRenderingIntent,
};
use crate::filter::{filter, AdaptiveFilterType, FilterType};

// use chunk::ChunkType;
use io::Write;
use std::{error, fmt, io, result};

use deflate::write::ZlibEncoder;

pub type Result<T = ()> = result::Result<T, EncodingError>;

#[derive(Debug)]
pub enum EncodingError {
    IoError(io::Error),
    Format(FormatError),
    Parameter(ParameterError),
    LimitsExceeded,
}

#[derive(Debug)]
pub struct FormatError {
    inner: FormatErrorKind,
}

#[derive(Debug)]
enum FormatErrorKind {
    ZeroWidth,
    ZeroHeight,
    ZeroFrames,
    ZeroColors,
    InvalidColorCombination(BitDepth, ColorType),
    NoPalette,
    WrongPaletteSize,
    // TODO: wait, what?
    WrittenTooMuch(usize),
    MissingData(usize),
    NotAnimated,
    FrameOutOfBounds, // add more info
    MissingFrames(u32),
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
        use EncodingError::*;
        match self {
            IoError(err) => write!(fmt, "{}", err),
            Format(desc) => write!(fmt, "{}", desc),
            Parameter(desc) => write!(fmt, "{}", desc),
            LimitsExceeded => write!(fmt, "Limits are exceeded."),
        }
    }
}

impl fmt::Display for FormatError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        use FormatErrorKind::*;
        match self.inner {
            ZeroWidth => write!(fmt, "Zero width not allowed"),
            ZeroHeight => write!(fmt, "Zero height not allowed"),
            ZeroFrames => write!(fmt, "Zero frames not allowed"),
            ZeroColors => write!(fmt, "The color palette must contain at least one color"),
            InvalidColorCombination(depth, color) => write!(
                fmt,
                "Invalid combination of bit-depth '{:?}' and color-type '{:?}'",
                depth, color
            ),
            NoPalette => write!(fmt, "Can't write indexed image without palette"),
            WrongPaletteSize => write!(fmt, "The palette must contain RGB colors"),
            WrittenTooMuch(index) => write!(fmt, "Wrong data size, got {} bytes too many", index),
            MissingData(index) => write!(fmt, "Wrong data size, needs {} more bytes", index),
            NotAnimated => write!(fmt, "Not an animation"),
            FrameOutOfBounds => write!(fmt, "The frame doesn't fit in the image"),
            MissingFrames(frames) => write!(fmt, "There are still {} frames to write", frames),
        }
    }
}

impl From<io::Error> for EncodingError {
    fn from(err: io::Error) -> Self {
        EncodingError::IoError(err)
    }
}

impl From<EncodingError> for io::Error {
    fn from(err: EncodingError) -> Self {
        io::Error::new(io::ErrorKind::Other, err)
    }
}

impl FrameControl {
    pub fn encode<W: Write>(self, w: &mut W) -> io::Result<()> {
        let mut data = [0u8; 26];
        data[..4].copy_from_slice(&self.sequence_number.to_be_bytes());
        data[4..8].copy_from_slice(&self.width.to_be_bytes());
        data[8..12].copy_from_slice(&self.height.to_be_bytes());
        data[12..16].copy_from_slice(&self.x_offset.to_be_bytes());
        data[16..20].copy_from_slice(&self.y_offset.to_be_bytes());
        data[20..22].copy_from_slice(&self.delay_num.to_be_bytes());
        data[22..24].copy_from_slice(&self.delay_den.to_be_bytes());
        data[24] = self.dispose_op as u8;
        data[25] = self.blend_op as u8;

        chunk::encode_chunk(w, chunk::fcTL, &data)
    }
}

impl AnimationControl {
    pub fn encode<W: Write>(self, w: &mut W) -> io::Result<()> {
        let mut data = [0; 8];
        data[..4].copy_from_slice(&self.num_frames.to_be_bytes());
        data[4..].copy_from_slice(&self.num_plays.to_be_bytes());
        chunk::encode_chunk(w, chunk::acTL, &data)
    }
}

impl ScaledFloat {
    pub fn encode<W: Write>(self, w: &mut W) -> io::Result<()> {
        chunk::encode_chunk(w, chunk::gAMA, &self.into_scaled().to_be_bytes())
    }
}

impl SourceChromaticities {
    #[rustfmt::skip]
    pub fn to_be_bytes(self) -> [u8; 32] {
        let white_x = self.white.0.into_scaled().to_be_bytes();
        let white_y = self.white.1.into_scaled().to_be_bytes();
        let red_x   = self.red.0.into_scaled().to_be_bytes();
        let red_y   = self.red.1.into_scaled().to_be_bytes();
        let green_x = self.green.0.into_scaled().to_be_bytes();
        let green_y = self.green.1.into_scaled().to_be_bytes();
        let blue_x  = self.blue.0.into_scaled().to_be_bytes();
        let blue_y  = self.blue.1.into_scaled().to_be_bytes();
        [
            white_x[0], white_x[1], white_x[2], white_x[3],
            white_y[0], white_y[1], white_y[2], white_y[3],
            red_x[0],   red_x[1],   red_x[2],   red_x[3],
            red_y[0],   red_y[1],   red_y[2],   red_y[3],
            green_x[0], green_x[1], green_x[2], green_x[3],
            green_y[0], green_y[1], green_y[2], green_y[3],
            blue_x[0],  blue_x[1],  blue_x[2],  blue_x[3],
            blue_y[0],  blue_y[1],  blue_y[2],  blue_y[3],
        ]
    }

    pub fn encode<W: Write>(self, w: &mut W) -> io::Result<()> {
        chunk::encode_chunk(w, chunk::cHRM, &self.to_be_bytes())
    }
}

impl SrgbRenderingIntent {
    pub fn encode<W: Write>(self, w: &mut W) -> io::Result<()> {
        chunk::encode_chunk(w, chunk::sRGB, &[self.into_raw()])
    }
}

impl Info {
    pub fn encode<W: Write>(&self, w: &mut W) -> io::Result<()> {
        chunk::IHDR_encode(
            w,
            self.width,
            self.height,
            self.bit_depth,
            self.color_type,
            self.interlaced,
        )?;

        if let Some(p) = &self.palette {
            chunk::encode_chunk(w, chunk::PLTE, p)?;
        };

        if let Some(t) = &self.trns {
            chunk::encode_chunk(w, chunk::tRNS, t)?;
        }

        // If specified, the sRGB information overrides the source gamma and chromaticities.
        if let Some(srgb) = &self.srgb {
            let gamma = crate::srgb::substitute_gamma();
            let chromaticities = crate::srgb::substitute_chromaticities();
            srgb.encode(w)?;
            gamma.encode(w)?;
            chromaticities.encode(w)?;
        } else {
            self.source_gamma.map_or(Ok(()), |v| v.encode(w))?;
            self.source_chromaticities.map_or(Ok(()), |v| v.encode(w))?;
        }
        self.animation_control.map_or(Ok(()), |v| v.encode(w))?;

        Ok(())
    }
}

struct ZlibWriter<W: Write> {
    curr_buf: Vec<u8>,
    prev_buf: Vec<u8>,
    index: usize,
    zenc: ZlibEncoder<W>,
    bpp: BytesPerPixel,
    pub filter: FilterType,
    pub adaptive: AdaptiveFilterType,
}

impl<W: Write> ZlibWriter<W> {
    pub fn new(
        w: W,
        buf_len: usize,
        compression: Compression,
        bpp: BytesPerPixel,
        filter: FilterType,
        adaptive: AdaptiveFilterType,
    ) -> Self {
        Self {
            curr_buf: vec![0; buf_len],
            prev_buf: vec![0; buf_len],
            index: 0,
            zenc: ZlibEncoder::new(w, compression),
            bpp,
            filter,
            adaptive,
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
        let method = filter(
            self.filter,
            self.adaptive,
            self.bpp,
            &self.prev_buf,
            &mut self.curr_buf,
        );
        self.prev_buf = prev;

        self.zenc.write_all(&[method as u8])?;
        self.zenc.write_all(&self.curr_buf)
    }

    /// Finishes the compression by writing the chunksum at the end, consumes the zlib writer
    /// and returns the inner one
    pub fn finish(self) -> io::Result<W> {
        self.zenc.finish()
    }
}

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
    adaptive: AdaptiveFilterType,
    filter: FilterType,
}

// Private impl.
impl From<FormatErrorKind> for FormatError {
    fn from(kind: FormatErrorKind) -> Self {
        FormatError { inner: kind }
    }
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
            filter: <_>::default(),
            adaptive: <_>::default(),
        }
    }

    pub fn animated(&mut self, frames: u32, repeat: u32) -> Result {
        if frames == 0 {
            Err(EncodingError::Format(FormatErrorKind::ZeroFrames.into()))
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

    pub fn set_palette(&mut self, palette: Vec<u8>) -> Result<()> {
        if palette.is_empty() {
            Err(EncodingError::Format(FormatErrorKind::ZeroColors.into()))
        } else if palette.len() % 3 != 0 {
            Err(EncodingError::Format(
                FormatErrorKind::WrongPaletteSize.into(),
            ))
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

    /// Mark the image data as conforming to the SRGB color space with the specified rendering intent.
    ///
    /// Matching source gamma and chromaticities chunks are added automatically.
    /// Any manually specified source gamma or chromaticities will be ignored.
    pub fn set_srgb(&mut self, rendering_intent: super::SrgbRenderingIntent) {
        self.info.srgb = Some(rendering_intent);
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
        self.filter = filter;
    }

    /// Set the adaptive filter type.
    ///
    /// Adaptive filtering attempts to select the best filter for each line
    /// based on heuristics which minimize the file size for compression rather
    /// than use a single filter for the entire image. The default method is
    /// [`AdaptiveFilterType::NonAdaptive`].
    ///
    /// [`AdaptiveFilterType::NonAdaptive`]: enum.AdaptiveFilterType.html
    pub fn set_default_adaptive_filter(&mut self, adaptive_filter: AdaptiveFilterType) {
        self.adaptive = adaptive_filter;
    }

    pub fn set_default_frame_delay(&mut self, delay_num: u16, delay_den: u16) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        } else {
            let fctl = self.info.frame_control.as_mut().unwrap();
            fctl.delay_num = delay_num;
            fctl.delay_den = delay_den;
            Ok(())
        }
    }

    pub fn set_default_dispose_op(&mut self, dispose: DisposeOp) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        } else {
            let fctl = self.info.frame_control.as_mut().unwrap();
            fctl.dispose_op = dispose;
            Ok(())
        }
    }

    pub fn set_default_blend_op(&mut self, blend: BlendOp) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        } else {
            let fctl = self.info.frame_control.as_mut().unwrap();
            fctl.blend_op = blend;
            Ok(())
        }
    }

    // This could be a parameter of .animated(..., sep_def_image: bool)
    pub fn with_separate_default_image(&mut self) -> Result {
        if self.info.animation_control.is_none() {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        } else {
            self.sep_def_img = true;
            Ok(())
        }
    }

    pub fn write_header(mut self) -> Result<Controller<W>> {
        let info = self.info;

        if info.width == 0 {
            return Err(EncodingError::Format(FormatErrorKind::ZeroWidth.into()));
        }

        if info.height == 0 {
            return Err(EncodingError::Format(FormatErrorKind::ZeroHeight.into()));
        }

        if info.color_type.is_combination_invalid(info.bit_depth) {
            return Err(EncodingError::Format(
                FormatErrorKind::InvalidColorCombination(info.bit_depth, info.color_type).into(),
            ));
        }

        if info.color_type == ColorType::Indexed && info.palette.is_none() {
            return Err(EncodingError::Format(FormatErrorKind::NoPalette.into()));
        }

        self.w.write_all(&[137, 80, 78, 71, 13, 10, 26, 10])?;

        info.encode(&mut self.w)?;

        Ok(Controller::new(
            self.w,
            info,
            self.sep_def_img,
            self.filter,
            self.adaptive,
        ))
    }
}

struct AnimationData {
    fctl: FrameControl,
    sep_def: bool,
}

// TODO: find a better name
pub struct Controller<W: Write> {
    w: Option<io::BufWriter<ChunkWriter<W>>>,
    // here the FrameControl is used for costomized data
    info: Info,
    anim_data: Option<AnimationData>,
    total: u32,
    written: u32,
    filter: FilterType,
    adaptive: AdaptiveFilterType,
}

impl<W: Write> Controller<W> {
    fn new(
        w: W,
        info: Info,
        sep_def: bool,
        filter: FilterType,
        adaptive: AdaptiveFilterType,
    ) -> Self {
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
                std::u32::MAX as usize >> 3,
                ChunkWriter::new(w),
            )),
            info,
            anim_data,
            total: frames,
            written: 0,
            filter,
            adaptive,
        }
    }

    pub fn set_dispose_op(&mut self, op: DisposeOp) -> Result {
        if let Some(ref mut anim_data) = self.anim_data {
            anim_data.fctl.dispose_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_blend_op(&mut self, op: BlendOp) -> Result {
        if let Some(ref mut anim_data) = self.anim_data {
            anim_data.fctl.blend_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_frame_delay(&mut self, delay_den: u16, delay_num: u16) -> Result {
        if let Some(ref mut anim_data) = self.anim_data {
            anim_data.fctl.delay_den = delay_den;
            anim_data.fctl.delay_num = delay_num;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_frame_position(&mut self, x: u32, y: u32, width: u32, height: u32) -> Result<()> {
        if let Some(ref mut anim_data) = self.anim_data {
            if x + width > self.info.width || y + height > self.info.height {
                // Err(EncodingError::FrameOutOfBounds(
                //     self.info.width,
                //     self.info.height,
                // ))
                Err(EncodingError::Format(
                    FormatErrorKind::FrameOutOfBounds.into(),
                ))
            } else {
                anim_data.fctl.x_offset = x;
                anim_data.fctl.y_offset = y;
                anim_data.fctl.width = width;
                anim_data.fctl.height = height;
                Ok(())
            }
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn remaining(&self) -> u32 {
        self.total - self.written
    }

    pub fn write_header<'a>(&'a mut self) -> Result<Writer<'a, W>> {
        if let Some(ref mut w) = self.w {
            if self.written == self.total {
                Err(EncodingError::Parameter(
                    ParameterErrorKind::PolledAfterEndOfImage.into(),
                ))
            } else if let Some(ref mut anim_data) = self.anim_data {
                if anim_data.sep_def {
                    anim_data.sep_def = false;
                    Ok(Writer::new(
                        w,
                        self.info.clone(),
                        self.filter,
                        self.adaptive,
                    ))
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
                    Ok(Writer::new(
                        w,
                        self.info.clone(),
                        self.filter,
                        self.adaptive,
                    ))
                }
            } else {
                self.written = 1;
                Ok(Writer::new(
                    w,
                    self.info.clone(),
                    self.filter,
                    self.adaptive,
                ))
            }
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }

    pub fn finish(&mut self) -> Result<()> {
        if let Some(mut w) = self.w.take() {
            if self.remaining() > 0 {
                self.w = Some(w);
                Err(EncodingError::Format(
                    FormatErrorKind::MissingFrames(self.remaining()).into(),
                ))
            } else {
                Ok(chunk::encode_chunk(
                    w.get_mut().get_mut(),
                    chunk::IEND,
                    &[],
                )?)
            }
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }
}

impl<W: Write> Drop for Controller<W> {
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
    fn new(
        w: &'a mut io::BufWriter<ChunkWriter<W>>,
        info: Info,
        filter: FilterType,
        adaptive: AdaptiveFilterType,
    ) -> Self {
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
            filter,
            adaptive,
        );

        Self {
            w: Some(zlib_writer),
            // info,
            total,
        }
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        let w = self.w.as_mut().ok_or_else(|| {
            EncodingError::Parameter(ParameterErrorKind::PolledAfterEndOfImage.into())
        })?;

        if self.total < data.len() {
            return Err(EncodingError::Format(
                FormatErrorKind::WrittenTooMuch(data.len() - self.total).into(),
            ));
        }
        self.total -= data.len();

        w.compress_data(data)?;
        Ok(())
    }

    pub fn set_filter(&mut self, filter: FilterType) -> Result<()> {
        match self.w {
            Some(ref mut w) => Ok(w.filter = filter),
            None => Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            )),
        }
    }

    pub fn set_adaptive_filter(&mut self, adaptive: AdaptiveFilterType) -> Result<()> {
        match self.w {
            Some(ref mut w) => Ok(w.adaptive = adaptive),
            None => Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            )),
        }
    }

    pub fn finish(&mut self) -> Result<()> {
        if let Some(w) = self.w.take() {
            if self.total > 0 {
                // Replace the writer has it should be possible to add the missing data
                self.w = Some(w);
                Err(EncodingError::Format(
                    FormatErrorKind::MissingData(self.total).into(),
                ))
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
            Exclusive::MutRef(&mut ref v) | Exclusive::Owned(ref v) => v,
        }
    }
}

impl<'a, T> std::ops::DerefMut for Exclusive<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Exclusive::MutRef(v) => v,
            Exclusive::Owned(v) => v,
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
                        .write_header()
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

                    let mut img_wrt = encoder.write_header().unwrap();
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
                    .write_header()
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
        encoder.set_color(ColorType::Rgb);
        let mut png_writer = encoder.write_header()?;

        let correct_image_size = width * height * 3;
        let image = vec![0u8; correct_image_size + 1];
        let result = png_writer
            .write_header()
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
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Rgba);
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
        encoder.set_color(ColorType::Rgb);
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
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Rgba);
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
            encoder.set_color(ColorType::Rgb);
            encoder.set_default_filter(filter);
            encoder
                .write_header()?
                .write_header()
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
            encoder.set_color(ColorType::Rgb);
            encoder.set_default_filter(FilterType::Avg);
            if let Some(gamma) = gamma {
                encoder.set_source_gamma(gamma);
            }
            encoder
                .write_header()?
                .write_header()
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
