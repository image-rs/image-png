#![allow(non_snake_case)]

use crate::chunk;
use crate::common::{
    AnimationControl, BitDepth, BlendOp, BytesPerPixel, ColorType, Compression, DisposeOp,
    FrameControl, Info, ParameterError, ParameterErrorKind, ScaledFloat, SourceChromaticities,
    SrgbRenderingIntent,
};
use crate::filter::{filter, AdaptiveFilterType, FilterType};

use io::{Read, Write};
use std::{error, fmt, io, result};

use deflate::write::ZlibEncoder;

pub type Result<T = ()> = result::Result<T, EncodingError>;

const DEFAULT_BUFFER_LENGTH: usize = 4 * 1024;

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

// Private impl.
impl From<FormatErrorKind> for FormatError {
    fn from(kind: FormatErrorKind) -> Self {
        FormatError { inner: kind }
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

/// PNG Animation Encoder
pub struct Encoder<W: io::Write> {
    w: W,
    info: Info,
    sep_def_img: bool,
    adaptive: AdaptiveFilterType,
    filter: FilterType,
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

// This enum is used to generalize what to write on each image/frame
#[derive(Clone, Copy)]
enum ChunkData {
    IDAT,
    fdAT {
        fctl: FrameControl,
        sep_def: bool,
        first: bool,
    },
}

impl ChunkData {
    pub fn max_chunk_data(&self) -> usize {
        use ChunkData::*;
        (match self {
            IDAT | fdAT { sep_def: true, .. } | fdAT { first: true, .. } => std::u32::MAX,
            fdAT { .. } => std::u32::MAX - 4,
        }) as usize
    }

    pub fn write_header<W: Write>(&mut self, w: &mut W) -> io::Result<()> {
        match self {
            ChunkData::fdAT {
                ref mut fctl,
                sep_def: false,
                ..
            } => {
                fctl.encode(w)?;
                fctl.sequence_number += 1;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn encode<W: Write>(&mut self, w: &mut W, data: &[u8]) -> io::Result<()> {
        use ChunkData::*;
        match self {
            IDAT | fdAT { sep_def: true, .. } | fdAT { first: true, .. } => {
                chunk::encode_chunk(w, chunk::IDAT, data)
            }
            fdAT { ref mut fctl, .. } => {
                chunk::fdAT_encode(w, fctl.sequence_number, data)?;
                fctl.sequence_number += 1;
                Ok(())
            }
        }
    }

    pub fn finish(&mut self) {
        if let ChunkData::fdAT {
            ref mut sep_def,
            ref mut first,
            ..
        } = self
        {
            *sep_def = false;
            *first = false;
        }
    }
}

// TODO: find a better name
pub struct Controller<W: Write> {
    w: Option<W>,
    data: ChunkData,
    // here the FrameControl is used for costomized data
    info: Info,
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
        let frames = info
            .animation_control
            .map(|actl| actl.num_frames)
            .unwrap_or(1);

        let data = info
            .frame_control
            .map(|fctl| ChunkData::fdAT {
                fctl,
                sep_def,
                first: true,
            })
            .unwrap_or(ChunkData::IDAT);

        Self {
            w: Some(w),
            info,
            data,
            total: frames,
            written: 0,
            filter,
            adaptive,
        }
    }

    pub fn set_dispose_op(&mut self, op: DisposeOp) -> Result {
        if let ChunkData::fdAT { ref mut fctl, .. } = self.data {
            fctl.dispose_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_blend_op(&mut self, op: BlendOp) -> Result {
        if let ChunkData::fdAT { ref mut fctl, .. } = self.data {
            fctl.blend_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_frame_delay(&mut self, delay_den: u16, delay_num: u16) -> Result {
        if let ChunkData::fdAT { ref mut fctl, .. } = self.data {
            fctl.delay_den = delay_den;
            fctl.delay_num = delay_num;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_frame_position(&mut self, x: u32, y: u32, width: u32, height: u32) -> Result<()> {
        if let ChunkData::fdAT { ref mut fctl, .. } = self.data {
            if x + width > self.info.width || y + height > self.info.height {
                // Err(EncodingError::FrameOutOfBounds(
                //     self.info.width,
                //     self.info.height,
                // ))
                Err(EncodingError::Format(
                    FormatErrorKind::FrameOutOfBounds.into(),
                ))
            } else {
                fctl.x_offset = x;
                fctl.y_offset = y;
                fctl.width = width;
                fctl.height = height;
                Ok(())
            }
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_filter(&mut self, filter: FilterType) {
        self.filter = filter;
    }

    pub fn set_adaptive_filter(&mut self, filter: AdaptiveFilterType) {
        self.adaptive = filter;
    }

    pub fn remaining(&self) -> u32 {
        self.total - self.written
    }

    pub fn next_image(mut self) -> Result<Writer<W>> {
        if self.written == self.total || self.w.is_none() {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        } else {
            self.written += 1;
            self.data.write_header(self.w.as_mut().unwrap())?;
            Ok(Writer::new(self))
        }
    }

    pub fn finish(&mut self) -> Result<W> {
        if let Some(mut w) = self.w.take() {
            if self.remaining() > 0 {
                self.w = Some(w);
                Err(EncodingError::Format(
                    FormatErrorKind::MissingFrames(self.remaining()).into(),
                ))
            } else {
                chunk::encode_chunk(&mut w, chunk::IEND, &[])?;
                Ok(w)
            }
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }

    /// Returns the width and height of the current
    pub fn current_size(&self) -> (u32, u32) {
        match &self.info {
            Info {
                frame_control: Some(fctl),
                ..
            } => (fctl.width, fctl.height),
            info => (info.width, info.height),
        }
    }

    /// Returns the size in bytes of both the current image and the current image scalines
    pub fn current_bytes(&self) -> (usize, usize) {
        let (width, height) = self.current_size();
        let in_len = self.info.raw_row_length_from_width(width) - 1;
        (in_len * height as usize, in_len)
    }
}

impl<W: Write> Drop for Controller<W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}

/// PNG writer
pub struct Writer<W: Write> {
    ctrl: Option<Controller<W>>,
}

impl<W: Write> Writer<W> {
    fn new(ctrl: Controller<W>) -> Self {
        Self { ctrl: Some(ctrl) }
    }

    pub fn write_chunk(&mut self, chunk: chunk::ChunkType, data: &[u8]) -> Result<()> {
        let ctrl = self.ctrl.as_mut().ok_or_else(|| {
            EncodingError::Parameter(ParameterErrorKind::PolledAfterEndOfImage.into())
        })?;

        let w = ctrl.w.as_mut().unwrap();
        Ok(chunk::encode_chunk(w, chunk, data)?)
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        let ctrl = self.ctrl.as_mut().ok_or_else(|| {
            EncodingError::Parameter(ParameterErrorKind::PolledAfterEndOfImage.into())
        })?;

        let (data_size, in_len) = ctrl.current_bytes();
        let w = ctrl.w.as_mut().unwrap();

        if data_size != data.len() {
            Err(EncodingError::Parameter(
                ParameterErrorKind::ImageBufferSize {
                    expected: data_size,
                    actual: data.len(),
                }
                .into(),
            ))
        } else {
            let bpp = ctrl.info.bpp_in_prediction();
            let mut prev = vec![0; in_len];
            let mut current = vec![0; in_len];
            let mut zlib = ZlibEncoder::new(Vec::new(), ctrl.info.compression);
            let filter_method = ctrl.filter;
            let adaptive_method = ctrl.adaptive;
            for line in data.chunks(in_len) {
                current.copy_from_slice(&line);
                let filter_type = filter(filter_method, adaptive_method, bpp, &prev, &mut current);
                zlib.write_all(&[filter_type as u8])?;
                zlib.write_all(&current)?;
                prev.copy_from_slice(line);
            }
            let zlib_encoded = zlib.finish()?;
            for data in zlib_encoded.chunks(ctrl.data.max_chunk_data()) {
                ctrl.data.encode(w, data)?;
            }
            Ok(())
        }
    }

    pub fn finish(&mut self) -> Result<Controller<W>> {
        if let Some(mut ctrl) = self.ctrl.take() {
            if let Some(w) = ctrl.w.as_mut() {
                w.flush()?;
            }
            ctrl.data.finish();
            Ok(ctrl)
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }

    /// Create a stream writer.
    ///
    /// This allows you to create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chunk
    /// size.
    ///
    /// This borrows the writer which allows for manually appending additional
    /// chunks after the image data has been written.
    pub fn stream_writer(&mut self) -> Result<StreamWriter<W>> {
        self.stream_writer_with_size(DEFAULT_BUFFER_LENGTH)
    }

    /// Create a stream writer with custom buffer size.
    ///
    /// See [`stream_writer`].
    ///
    /// [`stream_writer`]: #fn.stream_writer
    pub fn stream_writer_with_size(&mut self, size: usize) -> Result<StreamWriter<W>> {
        if let Some(ref mut ctrl) = self.ctrl {
            Ok(StreamWriter::new(ChunkOutput::Borrowed(ctrl), size))
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }

    /// Turn this into a stream writer for image data.
    ///
    /// This allows you to create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chunk
    /// size.
    pub fn into_stream_writer(self) -> Result<StreamWriter<'static, W>> {
        self.into_stream_writer_with_size(DEFAULT_BUFFER_LENGTH)
    }

    /// Turn this into a stream writer with custom buffer size.
    ///
    /// See [`into_stream_writer`].
    ///
    /// [`into_stream_writer`]: #fn.into_stream_writer
    pub fn into_stream_writer_with_size(mut self, size: usize) -> Result<StreamWriter<'static, W>> {
        if let Some(ctrl) = self.ctrl.take() {
            Ok(StreamWriter::new(ChunkOutput::Owned(ctrl), size))
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }
}

impl<'a, W: Write> Drop for Writer<W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}

pub enum ChunkOutput<'a, W: Write> {
    Borrowed(&'a mut Controller<W>),
    Owned(Controller<W>),
}

impl<'a, W: Write> std::ops::Deref for ChunkOutput<'a, W> {
    type Target = Controller<W>;
    fn deref(&self) -> &Controller<W> {
        match self {
            ChunkOutput::Borrowed(writer) => writer,
            ChunkOutput::Owned(writer) => writer,
        }
    }
}

impl<'a, W: Write> std::ops::DerefMut for ChunkOutput<'a, W> {
    fn deref_mut(&mut self) -> &mut Controller<W> {
        match self {
            ChunkOutput::Borrowed(writer) => writer,
            ChunkOutput::Owned(writer) => writer,
        }
    }
}

struct ChunkWriter<'a, W: Write> {
    writer: ChunkOutput<'a, W>,
    buffer: Vec<u8>,
    index: usize,
}

impl<'a, W: Write> ChunkWriter<'a, W> {
    fn new(writer: ChunkOutput<'a, W>, buf_len: usize) -> ChunkWriter<'a, W> {
        ChunkWriter {
            writer,
            buffer: vec![0; buf_len],
            index: 0,
        }
    }

    fn flush_inner(&mut self) -> Result<()> {
        match *self.writer {
            Controller {
                w: Some(ref mut w),
                ref mut data,
                ..
            } => {
                data.encode(w, &self.buffer[..self.index])?;
                self.index = 0;
                Ok(())
            }
            _ => Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            )),
        }
    }
}

impl<'a, W: Write> Write for ChunkWriter<'a, W> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let written = buf.read(&mut self.buffer[self.index..])?;
        self.index += written;
        if self.index == self.buffer.len() {
            self.flush_inner()?;
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.index > 0 {
            self.flush_inner()?;
        }
        Ok(())
    }
}

/// Streaming PNG writer
///
/// This may silently fail in the destructor, so it is a good idea to call
/// [`finish`](#method.finish) or [`flush`](https://doc.rust-lang.org/stable/std/io/trait.Write.html#tymethod.flush) before dropping.
pub struct StreamWriter<'a, W: Write> {
    writer: Option<ZlibEncoder<ChunkWriter<'a, W>>>,
    prev_buf: Vec<u8>,
    curr_buf: Vec<u8>,
    index: usize,
    bpp: BytesPerPixel,
    filter: FilterType,
    adaptive_filter: AdaptiveFilterType,
}

impl<'a, W: Write> StreamWriter<'a, W> {
    fn new(writer: ChunkOutput<'a, W>, buf_len: usize) -> StreamWriter<'a, W> {
        let bpp = writer.info.bpp_in_prediction();
        let in_len = writer.info.raw_row_length() - 1;
        let filter = writer.filter;
        let adaptive_filter = writer.adaptive;
        let prev_buf = vec![0; in_len];
        let curr_buf = vec![0; in_len];

        let compression = writer.info.compression;
        let chunk_writer = ChunkWriter::new(writer, buf_len);
        let zlib = deflate::write::ZlibEncoder::new(chunk_writer, compression);

        StreamWriter {
            writer: Some(zlib),
            index: 0,
            prev_buf,
            curr_buf,
            bpp,
            filter,
            adaptive_filter,
        }
    }

    pub fn finish(&mut self) -> Result<ChunkOutput<'a, W>> {
        self.flush()?;
        if let Some(w) = self.writer.take() {
            let mut chunk_w = w.finish()?;
            chunk_w.flush()?;
            chunk_w.writer.data.finish();
            // chunk_w.writer.written += 1;
            Ok(chunk_w.writer)
        } else {
            Err(EncodingError::Parameter(
                ParameterErrorKind::PolledAfterEndOfImage.into(),
            ))
        }
    }
}

impl<'a, W: Write> Write for StreamWriter<'a, W> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let w = self.writer.as_mut().ok_or_else(|| {
            EncodingError::Parameter(ParameterErrorKind::PolledAfterEndOfImage.into())
        })?;

        let written = buf.read(&mut self.curr_buf[self.index..])?;
        self.index += written;

        if self.index == self.curr_buf.len() {
            let filter_type = filter(
                self.filter,
                self.adaptive_filter,
                self.bpp,
                &self.prev_buf,
                &mut self.curr_buf,
            );
            w.write_all(&[filter_type as u8])?;
            w.write_all(&self.curr_buf)?;
            std::mem::swap(&mut self.prev_buf, &mut self.curr_buf);
            self.index = 0;
        }

        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.index > 0 {
            let missing = self.curr_buf.len() - self.index;
            Err(EncodingError::Format(FormatErrorKind::MissingData(missing).into()).into())
        } else {
            let w = self.writer.as_mut().ok_or_else(|| {
                EncodingError::Parameter(ParameterErrorKind::PolledAfterEndOfImage.into())
            })?;
            w.flush()
        }
    }
}

impl<'a, W: Write> Drop for StreamWriter<'a, W> {
    fn drop(&mut self) {
        self.finish().ok();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Decoder;

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
                let decoder = Decoder::new(File::open(path).unwrap());
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

                    let _ = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap()
                        .next_image()
                        .unwrap()
                        .write_image_data(&buf)
                        .unwrap();
                }
                // Decode encoded decoded image
                let decoder = Decoder::new(&*out);
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
                let decoder = Decoder::new(File::open(path).unwrap());
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

                    let encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();

                    let mut img_wrt = encoder.next_image().unwrap();
                    let mut stream_writer = img_wrt.stream_writer().unwrap();

                    let mut outer_wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut stream_writer,
                    };

                    outer_wrapper.write_all(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = Decoder::new(&*out);
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
        for bit_depth in vec![1u8, 2, 4, 8] {
            // Do a reference decoding, choose a fitting palette image from pngsuite
            let path = format!("tests/pngsuite/basn3p0{}.png", bit_depth);
            let decoder = Decoder::new(File::open(&path).unwrap());
            let (info, mut reader) = decoder.read_info().unwrap();

            let palette: Vec<u8> = reader.info().palette.clone().unwrap();
            let mut decoded_pixels = vec![0; info.buffer_size()];
            assert_eq!(
                info.width as usize * info.height as usize * usize::from(bit_depth),
                decoded_pixels.len() * 8
            );
            reader.next_frame(&mut decoded_pixels).unwrap();
            let indexed_data = decoded_pixels;

            let mut out = Vec::new();
            {
                let mut encoder = Encoder::new(&mut out, info.width, info.height);
                encoder.set_depth(BitDepth::from_u8(bit_depth).unwrap());
                encoder.set_color(ColorType::Indexed);
                encoder.set_palette(palette.clone()).ok();

                let _ = encoder
                    .write_header()
                    .unwrap()
                    .next_image()
                    .unwrap()
                    .write_image_data(&indexed_data)
                    .unwrap();
            }

            // Decode re-encoded image
            let decoder = Decoder::new(&*out);
            let (info, mut reader) = decoder.read_info().unwrap();
            let mut redecoded = vec![0; info.buffer_size()];
            reader.next_frame(&mut redecoded).unwrap();
            // check if the encoded image is ok:
            assert_eq!(indexed_data, redecoded);
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
        let png_writer = encoder.write_header()?;

        let correct_image_size = width * height * 3;
        let image = vec![0u8; correct_image_size + 1];
        let result = png_writer
            .next_image()
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
                .next_image()
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
                .next_image()
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
