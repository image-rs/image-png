extern crate crc32fast;

use std::convert::From;
use std::default::Default;
use std::error;
use std::fmt;
use std::io;
use std::{borrow::Cow, cmp::min};

use crc32fast::Hasher as Crc32;

use super::zlib::ZlibStream;
use crate::chunk::{self, ChunkType, IDAT, IEND, IHDR};
use crate::common::{
    AnimationControl, BitDepth, BlendOp, ColorType, DisposeOp, FrameControl, Info, ParameterError,
    PixelDimensions, ScaledFloat, SourceChromaticities, Unit,
};
use crate::text_metadata::{ITXtChunk, TEXtChunk, TextDecodingError, ZTXtChunk};
use crate::traits::ReadBytesExt;

/// TODO check if these size are reasonable
pub const CHUNCK_BUFFER_SIZE: usize = 32 * 1024;

/// Determines if checksum checks should be disabled globally.
///
/// This is used only in fuzzing. `afl` automatically adds `--cfg fuzzing` to RUSTFLAGS which can
/// be used to detect that build.
const CHECKSUM_DISABLED: bool = cfg!(fuzzing);

#[derive(Debug)]
enum U32Value {
    // CHUNKS
    Length,
    Type(u32),
    Crc(ChunkType),
}

#[derive(Debug)]
enum State {
    Signature(u8, [u8; 7]),
    U32Byte3(U32Value, u32),
    U32Byte2(U32Value, u32),
    U32Byte1(U32Value, u32),
    U32(U32Value),
    ReadChunk(ChunkType),
    PartialChunk(ChunkType),
    DecodeData(ChunkType, usize),
}

#[derive(Debug)]
/// Result of the decoding process
pub enum Decoded {
    /// Nothing decoded yet
    Nothing,
    Header(u32, u32, BitDepth, ColorType, bool),
    ChunkBegin(u32, ChunkType),
    ChunkComplete(u32, ChunkType),
    PixelDimensions(PixelDimensions),
    AnimationControl(AnimationControl),
    FrameControl(FrameControl),
    /// Decoded raw image data.
    ImageData,
    /// The last of a consecutive chunk of IDAT was done.
    /// This is distinct from ChunkComplete which only marks that some IDAT chunk was completed but
    /// not that no additional IDAT chunk follows.
    ImageDataFlushed,
    PartialChunk(ChunkType),
    ImageEnd,
}

/// Any kind of error during PNG decoding.
///
/// This enumeration provides a very rough analysis on the origin of the failure. That is, each
/// variant corresponds to one kind of actor causing the error. It should not be understood as a
/// direct blame but can inform the search for a root cause or if such a search is required.
#[derive(Debug)]
pub enum DecodingError {
    /// An error in IO of the underlying reader.
    IoError(io::Error),
    /// The input image was not a valid PNG.
    ///
    /// There isn't a lot that can be done here, except if the program itself was responsible for
    /// creating this image then investigate the generator. This is internally implemented with a
    /// large Enum. If You are interested in accessing some of the more exact information on the
    /// variant then we can discuss in an issue.
    Format(FormatError),
    /// An interface was used incorrectly.
    ///
    /// This is used in cases where it's expected that the programmer might trip up and stability
    /// could be affected. For example when:
    ///
    /// * The decoder is polled for more animation frames despite being done (or not being animated
    ///   in the first place).
    /// * The output buffer does not have the required size.
    ///
    /// As a rough guideline for introducing new variants parts of the requirements are dynamically
    /// derived from the (untrusted) input data while the other half is from the caller. In the
    /// above cases the number of frames respectively the size is determined by the file while the
    /// number of calls
    ///
    /// If you're an application you might want to signal that a bug report is appreciated.
    Parameter(ParameterError),
    /// The image would have required exceeding the limits configured with the decoder.
    ///
    /// Note that Your allocations, e.g. when reading into a pre-allocated buffer, is __NOT__
    /// considered part of the limits. Nevertheless, required intermediate buffers such as for
    /// singular lines is checked against the limit.
    ///
    /// Note that this is a best-effort basis.
    LimitsExceeded,
}

#[derive(Debug)]
pub struct FormatError {
    inner: FormatErrorInner,
}

#[derive(Debug)]
pub(crate) enum FormatErrorInner {
    /// Bad framing.
    CrcMismatch {
        /// bytes to skip to try to recover from this error
        recover: usize,
        /// Stored CRC32 value
        crc_val: u32,
        /// Calculated CRC32 sum
        crc_sum: u32,
        /// The chunk type that has the CRC mismatch.
        chunk: ChunkType,
    },
    /// Not a PNG, the magic signature is missing.
    InvalidSignature,
    /// End of file, within a chunk event.
    UnexpectedEof,
    /// End of file, while expecting more image data.
    UnexpectedEndOfChunk,
    // Errors of chunk level ordering, missing etc.
    /// Ihdr must occur.
    MissingIhdr,
    /// Fctl must occur if an animated chunk occurs.
    MissingFctl,
    /// Image data that was indicated in IHDR or acTL is missing.
    MissingImageData,
    /// 4.3., Must be first.
    ChunkBeforeIhdr {
        kind: ChunkType,
    },
    /// 4.3., some chunks must be before IDAT.
    AfterIdat {
        kind: ChunkType,
    },
    /// 4.3., some chunks must be before PLTE.
    AfterPlte {
        kind: ChunkType,
    },
    /// 4.3., some chunks must be between PLTE and IDAT.
    OutsidePlteIdat {
        kind: ChunkType,
    },
    /// 4.3., some chunks must be unique.
    DuplicateChunk {
        kind: ChunkType,
    },
    /// Specifically for fdat there is an embedded sequence number for chunks.
    ApngOrder {
        /// The sequence number in the chunk.
        present: u32,
        /// The one that should have been present.
        expected: u32,
    },
    // Errors specific to particular chunk data to be validated.
    /// The palette did not even contain a single pixel data.
    ShortPalette {
        expected: usize,
        len: usize,
    },
    /// A palletized image did not have a palette.
    PaletteRequired,
    /// The color-depth combination is not valid according to Table 11.1.
    InvalidColorBitDepth {
        color_type: ColorType,
        bit_depth: BitDepth,
    },
    ColorWithBadTrns(ColorType),
    InvalidBitDepth(u8),
    InvalidColorType(u8),
    InvalidDisposeOp(u8),
    InvalidBlendOp(u8),
    InvalidUnit(u8),
    /// The rendering intent of the sRGB chunk is invalid.
    InvalidSrgbRenderingIntent(u8),
    UnknownCompressionMethod(u8),
    UnknownFilterMethod(u8),
    UnknownInterlaceMethod(u8),
    /// The subframe is not in bounds of the image.
    /// TODO: fields with relevant data.
    BadSubFrameBounds {},
    // Errors specific to the IDAT/fDAT chunks.
    /// The compression of the data stream was faulty.
    CorruptFlateStream {
        err: miniz_oxide::inflate::TINFLStatus,
    },
    /// The image data chunk was too short for the expected pixel count.
    NoMoreImageData,
    // TODO: strictly type this.
    /// Filtering of a row has failed.
    BadFilter(&'static str),
    /// Bad text encoding
    BadTextEncoding(TextDecodingError),
}

impl error::Error for DecodingError {
    fn cause(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            DecodingError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for DecodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::DecodingError::*;
        match self {
            IoError(err) => write!(fmt, "{}", err),
            Parameter(desc) => write!(fmt, "{}", &desc),
            Format(desc) => write!(fmt, "{}", desc),
            LimitsExceeded => write!(fmt, "limits are exceeded"),
        }
    }
}

impl fmt::Display for FormatError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use FormatErrorInner::*;
        match &self.inner {
            CrcMismatch {
                crc_val,
                crc_sum,
                chunk,
                ..
            } => write!(
                fmt,
                "CRC error: expected 0x{:x} have 0x{:x} while decoding {:?} chunk.",
                crc_val, crc_sum, chunk
            ),
            MissingIhdr => write!(fmt, "IHDR chunk missing"),
            MissingFctl => write!(fmt, "fcTL chunk missing before fdAT chunk."),
            MissingImageData => write!(fmt, "IDAT or fDAT chunk is missing."),
            ChunkBeforeIhdr { kind } => write!(fmt, "{:?} chunk appeared before IHDR chunk", kind),
            AfterIdat { kind } => write!(fmt, "Chunk {:?} is invalid after IDAT chunk.", kind),
            AfterPlte { kind } => write!(fmt, "Chunk {:?} is invalid after PLTE chunk.", kind),
            OutsidePlteIdat { kind } => write!(
                fmt,
                "Chunk {:?} must appear between PLTE and IDAT chunks.",
                kind
            ),
            DuplicateChunk { kind } => write!(fmt, "Chunk {:?} must appear at most once.", kind),
            ApngOrder { present, expected } => write!(
                fmt,
                "Sequence is not in order, expected #{} got #{}.",
                expected, present,
            ),
            ShortPalette { expected, len } => write!(
                fmt,
                "Not enough palette entries, expect {} got {}.",
                expected, len
            ),
            PaletteRequired => write!(fmt, "Missing palette of indexed image."),
            InvalidColorBitDepth {
                color_type,
                bit_depth,
            } => write!(
                fmt,
                "Invalid color/depth combination in header: {:?}/{:?}",
                color_type, bit_depth,
            ),
            ColorWithBadTrns(color_type) => write!(
                fmt,
                "Transparency chunk found for color type {:?}.",
                color_type
            ),
            InvalidBitDepth(nr) => write!(fmt, "Invalid dispose operation {}.", nr),
            InvalidColorType(nr) => write!(fmt, "Invalid color type {}.", nr),
            InvalidDisposeOp(nr) => write!(fmt, "Invalid dispose op {}.", nr),
            InvalidBlendOp(nr) => write!(fmt, "Invalid blend op {}.", nr),
            InvalidUnit(nr) => write!(fmt, "Invalid physical pixel size unit {}.", nr),
            InvalidSrgbRenderingIntent(nr) => write!(fmt, "Invalid sRGB rendering intent {}.", nr),
            UnknownCompressionMethod(nr) => write!(fmt, "Unknown compression method {}.", nr),
            UnknownFilterMethod(nr) => write!(fmt, "Unknown filter method {}.", nr),
            UnknownInterlaceMethod(nr) => write!(fmt, "Unknown interlace method {}.", nr),
            BadSubFrameBounds {} => write!(fmt, "Sub frame is out-of-bounds."),
            InvalidSignature => write!(fmt, "Invalid PNG signature."),
            UnexpectedEof => write!(fmt, "Unexpected end of data before image end."),
            UnexpectedEndOfChunk => write!(fmt, "Unexpected end of data within a chunk."),
            NoMoreImageData => write!(fmt, "IDAT or fDAT chunk is has not enough data for image."),
            CorruptFlateStream { err } => {
                write!(fmt, "Corrupt deflate stream. ")?;
                use miniz_oxide::inflate::TINFLStatus;
                match err {
                    TINFLStatus::Adler32Mismatch => write!(fmt, "Adler32 checksum failed."),
                    TINFLStatus::BadParam => write!(fmt, "Invalid input parameters."),
                    // The Done status should already have been handled.
                    TINFLStatus::Done => write!(fmt, "Unexpected done status."),
                    TINFLStatus::FailedCannotMakeProgress => {
                        write!(fmt, "Unexpected end of data.")
                    }
                    // The HasMoreOutput status should already have been handled.
                    TINFLStatus::HasMoreOutput => write!(fmt, "Has more output."),
                    // The HasMoreInput status should already have been handled.
                    TINFLStatus::NeedsMoreInput => write!(fmt, "Needs more input."),
                    // Write out the error number in case a new has been added.
                    _ => write!(fmt, "Error number {:?}.", err),
                }
            }
            BadFilter(message) => write!(fmt, "{}.", message),
            // TODO: Wrap more info in the enum variant
            BadTextEncoding(tde) => {
                match tde {
                    TextDecodingError::Unrepresentable => {
                        write!(fmt, "Unrepresentable data in tEXt chunk.")
                    }
                    TextDecodingError::InvalidKeywordSize => {
                        write!(fmt, "Keyword empty or longer than 79 bytes.")
                    }
                    TextDecodingError::MissingNullSeparator => {
                        write!(fmt, "No null separator in tEXt chunk.")
                    }
                    TextDecodingError::InflationError => {
                        write!(fmt, "Invalid compressed text data.")
                    }
                    TextDecodingError::OutOfDecompressionSpace => {
                        write!(fmt, "Out of decompression space. Try with a larger limit.")
                    }
                    TextDecodingError::InvalidCompressionMethod => {
                        write!(fmt, "Using an unrecognized byte as compression method.")
                    }
                    TextDecodingError::InvalidCompressionFlag => {
                        write!(fmt, "Using a flag that is not 0 or 255 as a compression flag for iTXt chunk.")
                    }
                    TextDecodingError::MissingCompressionFlag => {
                        write!(fmt, "No compression flag in the iTXt chunk.")
                    }
                }
            }
        }
    }
}

impl From<io::Error> for DecodingError {
    fn from(err: io::Error) -> DecodingError {
        DecodingError::IoError(err)
    }
}

impl From<FormatError> for DecodingError {
    fn from(err: FormatError) -> DecodingError {
        DecodingError::Format(err)
    }
}

impl From<FormatErrorInner> for FormatError {
    fn from(inner: FormatErrorInner) -> Self {
        FormatError { inner }
    }
}

impl From<DecodingError> for io::Error {
    fn from(err: DecodingError) -> io::Error {
        match err {
            DecodingError::IoError(err) => err,
            err => io::Error::new(io::ErrorKind::Other, err.to_string()),
        }
    }
}

impl From<TextDecodingError> for DecodingError {
    fn from(tbe: TextDecodingError) -> Self {
        DecodingError::Format(FormatError {
            inner: FormatErrorInner::BadTextEncoding(tbe),
        })
    }
}

/// Decode config
#[derive(Default)]
pub(crate) struct DecodeConfig {
    ignore_text_chunk: bool,
}

impl DecodeConfig {
    pub fn set_ignore_text_chunk(&mut self, ignore_text_chunk: bool) {
        self.ignore_text_chunk = ignore_text_chunk;
    }
}

/// PNG StreamingDecoder (low-level interface)
pub struct StreamingDecoder {
    state: Option<State>,
    current_chunk: ChunkState,
    /// The inflater state handling consecutive `IDAT` and `fdAT` chunks.
    inflater: ZlibStream,
    /// The complete image info read from all prior chunks.
    pub(crate) info: Option<Info<'static>>,
    /// The animation chunk sequence number.
    current_seq_no: Option<u32>,
    /// Stores where in decoding an `fdAT` chunk we are.
    apng_seq_handled: bool,
    have_idat: bool,
    decode_config: DecodeConfig,
}

struct ChunkState {
    /// The type of the current chunk.
    /// Relevant for `IDAT` and `fdAT` which aggregate consecutive chunks of their own type.
    type_: ChunkType,

    /// Partial crc until now.
    crc: Crc32,

    /// Remaining bytes to be read.
    remaining: u32,

    /// Non-decoded bytes in the chunk.
    raw_bytes: Vec<u8>,
}

impl StreamingDecoder {
    /// Creates a new StreamingDecoder
    ///
    /// Allocates the internal buffers.
    pub fn new() -> StreamingDecoder {
        StreamingDecoder {
            state: Some(State::Signature(0, [0; 7])),
            current_chunk: ChunkState::default(),
            inflater: ZlibStream::new(),
            info: None,
            current_seq_no: None,
            apng_seq_handled: false,
            have_idat: false,
            decode_config: DecodeConfig::default(),
        }
    }

    /// Resets the StreamingDecoder
    pub fn reset(&mut self) {
        self.state = Some(State::Signature(0, [0; 7]));
        self.current_chunk.crc = Crc32::new();
        self.current_chunk.remaining = 0;
        self.current_chunk.raw_bytes.clear();
        self.inflater = ZlibStream::new();
        self.info = None;
        self.current_seq_no = None;
        self.apng_seq_handled = false;
        self.have_idat = false;
    }

    /// Provides access to the inner `info` field
    pub fn info(&self) -> Option<&Info<'static>> {
        self.info.as_ref()
    }

    /// Set decode config
    pub(crate) fn set_decode_config(&mut self, decode_config: DecodeConfig) {
        self.decode_config = decode_config;
    }

    pub fn set_ignore_text_chunk(&mut self, ignore_text_chunk: bool) {
        self.decode_config.set_ignore_text_chunk(ignore_text_chunk);
    }

    /// Low level StreamingDecoder interface.
    ///
    /// Allows to stream partial data to the encoder. Returns a tuple containing the bytes that have
    /// been consumed from the input buffer and the current decoding result. If the decoded chunk
    /// was an image data chunk, it also appends the read data to `image_data`.
    pub fn update(
        &mut self,
        mut buf: &[u8],
        image_data: &mut Vec<u8>,
    ) -> Result<(usize, Decoded), DecodingError> {
        let len = buf.len();
        while !buf.is_empty() && self.state.is_some() {
            match self.next_state(buf, image_data) {
                Ok((bytes, Decoded::Nothing)) => buf = &buf[bytes..],
                Ok((bytes, result)) => {
                    buf = &buf[bytes..];
                    return Ok((len - buf.len(), result));
                }
                Err(err) => return Err(err),
            }
        }
        Ok((len - buf.len(), Decoded::Nothing))
    }

    fn next_state<'a>(
        &'a mut self,
        buf: &[u8],
        image_data: &mut Vec<u8>,
    ) -> Result<(usize, Decoded), DecodingError> {
        use self::State::*;

        let current_byte = buf[0];

        // Driver should ensure that state is never None
        let state = self.state.take().unwrap();

        match state {
            Signature(i, mut signature) if i < 7 => {
                signature[i as usize] = current_byte;
                self.state = Some(Signature(i + 1, signature));
                Ok((1, Decoded::Nothing))
            }
            Signature(_, signature)
                if signature == [137, 80, 78, 71, 13, 10, 26] && current_byte == 10 =>
            {
                self.state = Some(U32(U32Value::Length));
                Ok((1, Decoded::Nothing))
            }
            Signature(..) => Err(DecodingError::Format(
                FormatErrorInner::InvalidSignature.into(),
            )),
            U32Byte3(type_, mut val) => {
                use self::U32Value::*;
                val |= u32::from(current_byte);
                match type_ {
                    Length => {
                        self.state = Some(U32(Type(val)));
                        Ok((1, Decoded::Nothing))
                    }
                    Type(length) => {
                        let type_str = ChunkType([
                            (val >> 24) as u8,
                            (val >> 16) as u8,
                            (val >> 8) as u8,
                            val as u8,
                        ]);
                        if type_str != self.current_chunk.type_
                            && (self.current_chunk.type_ == IDAT
                                || self.current_chunk.type_ == chunk::fdAT)
                        {
                            self.current_chunk.type_ = type_str;
                            self.inflater.finish_compressed_chunks(image_data)?;
                            self.inflater.reset();
                            self.state = Some(U32Byte3(Type(length), val & !0xff));
                            return Ok((0, Decoded::ImageDataFlushed));
                        }
                        self.current_chunk.type_ = type_str;
                        self.current_chunk.crc.reset();
                        self.current_chunk.crc.update(&type_str.0);
                        self.current_chunk.remaining = length;
                        self.apng_seq_handled = false;
                        self.current_chunk.raw_bytes.clear();
                        self.state = Some(ReadChunk(type_str));
                        Ok((1, Decoded::ChunkBegin(length, type_str)))
                    }
                    Crc(type_str) => {
                        let sum = self.current_chunk.crc.clone().finalize();
                        if CHECKSUM_DISABLED || val == sum {
                            self.state = Some(State::U32(U32Value::Length));
                            if type_str == IEND {
                                Ok((1, Decoded::ImageEnd))
                            } else {
                                Ok((1, Decoded::ChunkComplete(val, type_str)))
                            }
                        } else {
                            Err(DecodingError::Format(
                                FormatErrorInner::CrcMismatch {
                                    recover: 1,
                                    crc_val: val,
                                    crc_sum: sum,
                                    chunk: type_str,
                                }
                                .into(),
                            ))
                        }
                    }
                }
            }
            U32Byte2(type_, val) => {
                self.state = Some(U32Byte3(type_, val | u32::from(current_byte) << 8));
                Ok((1, Decoded::Nothing))
            }
            U32Byte1(type_, val) => {
                self.state = Some(U32Byte2(type_, val | u32::from(current_byte) << 16));
                Ok((1, Decoded::Nothing))
            }
            U32(type_) => {
                self.state = Some(U32Byte1(type_, u32::from(current_byte) << 24));
                Ok((1, Decoded::Nothing))
            }
            PartialChunk(type_str) => {
                match type_str {
                    IDAT => {
                        self.have_idat = true;
                        self.state = Some(DecodeData(type_str, 0));
                        Ok((0, Decoded::PartialChunk(type_str)))
                    }
                    chunk::fdAT => {
                        let data_start;
                        if let Some(seq_no) = self.current_seq_no {
                            if !self.apng_seq_handled {
                                data_start = 4;
                                let mut buf = &self.current_chunk.raw_bytes[..];
                                let next_seq_no = buf.read_be()?;
                                if next_seq_no != seq_no + 1 {
                                    return Err(DecodingError::Format(
                                        FormatErrorInner::ApngOrder {
                                            present: next_seq_no,
                                            expected: seq_no + 1,
                                        }
                                        .into(),
                                    ));
                                }
                                self.current_seq_no = Some(next_seq_no);
                                self.apng_seq_handled = true;
                            } else {
                                data_start = 0;
                            }
                        } else {
                            return Err(DecodingError::Format(
                                FormatErrorInner::MissingFctl.into(),
                            ));
                        }
                        self.state = Some(DecodeData(type_str, data_start));
                        Ok((0, Decoded::PartialChunk(type_str)))
                    }
                    // Handle other chunks
                    _ => {
                        if self.current_chunk.remaining == 0 {
                            // complete chunk
                            Ok((0, self.parse_chunk(type_str)?))
                        } else {
                            // Make sure we have room to read more of the chunk.
                            // We need it fully before parsing.
                            self.reserve_current_chunk()?;

                            self.state = Some(ReadChunk(type_str));
                            Ok((0, Decoded::PartialChunk(type_str)))
                        }
                    }
                }
            }
            ReadChunk(type_str) => {
                // The _previous_ event wanted to return the contents of raw_bytes, and let the
                // caller consume it,
                if self.current_chunk.remaining == 0 {
                    self.state = Some(U32(U32Value::Crc(type_str)));
                    Ok((0, Decoded::Nothing))
                } else {
                    let ChunkState {
                        crc,
                        remaining,
                        raw_bytes,
                        type_: _,
                    } = &mut self.current_chunk;

                    let buf_avail = raw_bytes.capacity() - raw_bytes.len();
                    let bytes_avail = min(buf.len(), buf_avail);
                    let n = min(*remaining, bytes_avail as u32);
                    if buf_avail == 0 {
                        self.state = Some(PartialChunk(type_str));
                        Ok((0, Decoded::Nothing))
                    } else {
                        let buf = &buf[..n as usize];
                        crc.update(buf);
                        raw_bytes.extend_from_slice(buf);

                        *remaining -= n;
                        if *remaining == 0 {
                            self.state = Some(PartialChunk(type_str));
                        } else {
                            self.state = Some(ReadChunk(type_str));
                        }
                        Ok((n as usize, Decoded::Nothing))
                    }
                }
            }
            DecodeData(type_str, mut n) => {
                let chunk_len = self.current_chunk.raw_bytes.len();
                let chunk_data = &self.current_chunk.raw_bytes[n..];
                let c = self.inflater.decompress(chunk_data, image_data)?;
                n += c;
                if n == chunk_len && c == 0 {
                    self.current_chunk.raw_bytes.clear();
                    self.state = Some(ReadChunk(type_str));
                    Ok((0, Decoded::ImageData))
                } else {
                    self.state = Some(DecodeData(type_str, n));
                    Ok((0, Decoded::ImageData))
                }
            }
        }
    }

    fn reserve_current_chunk(&mut self) -> Result<(), DecodingError> {
        // FIXME: use limits, also do so in iccp/zlib decompression.
        const MAX: usize = 0x10_0000;
        let ref mut buffer = self.current_chunk.raw_bytes;

        // Double if necessary, but no more than until the limit is reached.
        let reserve_size = MAX.saturating_sub(buffer.capacity()).min(buffer.len());
        buffer.reserve_exact(reserve_size);

        if buffer.capacity() == buffer.len() {
            Err(DecodingError::LimitsExceeded)
        } else {
            Ok(())
        }
    }

    fn parse_chunk(&mut self, type_str: ChunkType) -> Result<Decoded, DecodingError> {
        self.state = Some(State::U32(U32Value::Crc(type_str)));
        if self.info.is_none() && type_str != IHDR {
            return Err(DecodingError::Format(
                FormatErrorInner::ChunkBeforeIhdr { kind: type_str }.into(),
            ));
        }
        match match type_str {
            IHDR => self.parse_ihdr(),
            chunk::PLTE => self.parse_plte(),
            chunk::tRNS => self.parse_trns(),
            chunk::pHYs => self.parse_phys(),
            chunk::gAMA => self.parse_gama(),
            chunk::acTL => self.parse_actl(),
            chunk::fcTL => self.parse_fctl(),
            chunk::cHRM => self.parse_chrm(),
            chunk::sRGB => self.parse_srgb(),
            chunk::iCCP => self.parse_iccp(),
            chunk::tEXt if !self.decode_config.ignore_text_chunk => self.parse_text(),
            chunk::zTXt if !self.decode_config.ignore_text_chunk => self.parse_ztxt(),
            chunk::iTXt if !self.decode_config.ignore_text_chunk => self.parse_itxt(),
            _ => Ok(Decoded::PartialChunk(type_str)),
        } {
            Err(err) => {
                // Borrow of self ends here, because Decoding error does not borrow self.
                self.state = None;
                Err(err)
            }
            ok => ok,
        }
    }

    fn parse_fctl(&mut self) -> Result<Decoded, DecodingError> {
        let mut buf = &self.current_chunk.raw_bytes[..];
        let next_seq_no = buf.read_be()?;

        // Assuming that fcTL is required before *every* fdAT-sequence
        self.current_seq_no = Some(if let Some(seq_no) = self.current_seq_no {
            if next_seq_no != seq_no + 1 {
                return Err(DecodingError::Format(
                    FormatErrorInner::ApngOrder {
                        expected: seq_no + 1,
                        present: next_seq_no,
                    }
                    .into(),
                ));
            }
            next_seq_no
        } else {
            if next_seq_no != 0 {
                return Err(DecodingError::Format(
                    FormatErrorInner::ApngOrder {
                        expected: 0,
                        present: next_seq_no,
                    }
                    .into(),
                ));
            }
            0
        });
        self.inflater = ZlibStream::new();
        let fc = FrameControl {
            sequence_number: next_seq_no,
            width: buf.read_be()?,
            height: buf.read_be()?,
            x_offset: buf.read_be()?,
            y_offset: buf.read_be()?,
            delay_num: buf.read_be()?,
            delay_den: buf.read_be()?,
            dispose_op: {
                let dispose_op = buf.read_be()?;
                match DisposeOp::from_u8(dispose_op) {
                    Some(dispose_op) => dispose_op,
                    None => {
                        return Err(DecodingError::Format(
                            FormatErrorInner::InvalidDisposeOp(dispose_op).into(),
                        ))
                    }
                }
            },
            blend_op: {
                let blend_op = buf.read_be()?;
                match BlendOp::from_u8(blend_op) {
                    Some(blend_op) => blend_op,
                    None => {
                        return Err(DecodingError::Format(
                            FormatErrorInner::InvalidBlendOp(blend_op).into(),
                        ))
                    }
                }
            },
        };
        self.info.as_ref().unwrap().validate(&fc)?;
        self.info.as_mut().unwrap().frame_control = Some(fc);
        Ok(Decoded::FrameControl(fc))
    }

    fn parse_actl(&mut self) -> Result<Decoded, DecodingError> {
        if self.have_idat {
            Err(DecodingError::Format(
                FormatErrorInner::AfterIdat { kind: chunk::acTL }.into(),
            ))
        } else {
            let mut buf = &self.current_chunk.raw_bytes[..];
            let actl = AnimationControl {
                num_frames: buf.read_be()?,
                num_plays: buf.read_be()?,
            };
            self.info.as_mut().unwrap().animation_control = Some(actl);
            Ok(Decoded::AnimationControl(actl))
        }
    }

    fn parse_plte(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if info.palette.is_some() {
            // Only one palette is allowed
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::PLTE }.into(),
            ));
        } else {
            info.palette = Some(Cow::Owned(self.current_chunk.raw_bytes.clone()));
            Ok(Decoded::Nothing)
        }
    }

    fn parse_trns(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if info.trns.is_some() {
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::PLTE }.into(),
            ));
        }
        let (color_type, bit_depth) = { (info.color_type, info.bit_depth as u8) };
        let mut vec = self.current_chunk.raw_bytes.clone();
        let len = vec.len();
        match color_type {
            ColorType::Grayscale => {
                if len < 2 {
                    return Err(DecodingError::Format(
                        FormatErrorInner::ShortPalette { expected: 2, len }.into(),
                    ));
                }
                if bit_depth < 16 {
                    vec[0] = vec[1];
                    vec.truncate(1);
                }
                info.trns = Some(Cow::Owned(vec));
                Ok(Decoded::Nothing)
            }
            ColorType::Rgb => {
                if len < 6 {
                    return Err(DecodingError::Format(
                        FormatErrorInner::ShortPalette { expected: 6, len }.into(),
                    ));
                }
                if bit_depth < 16 {
                    vec[0] = vec[1];
                    vec[1] = vec[3];
                    vec[2] = vec[5];
                    vec.truncate(3);
                }
                info.trns = Some(Cow::Owned(vec));
                Ok(Decoded::Nothing)
            }
            ColorType::Indexed => {
                // The transparency chunk must be after the palette chunk and
                // before the data chunk.
                if info.palette.is_none() {
                    return Err(DecodingError::Format(
                        FormatErrorInner::AfterPlte { kind: chunk::tRNS }.into(),
                    ));
                } else if self.have_idat {
                    return Err(DecodingError::Format(
                        FormatErrorInner::OutsidePlteIdat { kind: chunk::tRNS }.into(),
                    ));
                }

                info.trns = Some(Cow::Owned(vec));
                Ok(Decoded::Nothing)
            }
            c => Err(DecodingError::Format(
                FormatErrorInner::ColorWithBadTrns(c).into(),
            )),
        }
    }

    fn parse_phys(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if self.have_idat {
            Err(DecodingError::Format(
                FormatErrorInner::AfterIdat { kind: chunk::pHYs }.into(),
            ))
        } else if info.pixel_dims.is_some() {
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::pHYs }.into(),
            ));
        } else {
            let mut buf = &self.current_chunk.raw_bytes[..];
            let xppu = buf.read_be()?;
            let yppu = buf.read_be()?;
            let unit = buf.read_be()?;
            let unit = match Unit::from_u8(unit) {
                Some(unit) => unit,
                None => {
                    return Err(DecodingError::Format(
                        FormatErrorInner::InvalidUnit(unit).into(),
                    ))
                }
            };
            let pixel_dims = PixelDimensions { xppu, yppu, unit };
            info.pixel_dims = Some(pixel_dims);
            Ok(Decoded::PixelDimensions(pixel_dims))
        }
    }

    fn parse_chrm(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if self.have_idat {
            Err(DecodingError::Format(
                FormatErrorInner::AfterIdat { kind: chunk::cHRM }.into(),
            ))
        } else if info.chrm_chunk.is_some() {
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::cHRM }.into(),
            ));
        } else {
            let mut buf = &self.current_chunk.raw_bytes[..];
            let white_x: u32 = buf.read_be()?;
            let white_y: u32 = buf.read_be()?;
            let red_x: u32 = buf.read_be()?;
            let red_y: u32 = buf.read_be()?;
            let green_x: u32 = buf.read_be()?;
            let green_y: u32 = buf.read_be()?;
            let blue_x: u32 = buf.read_be()?;
            let blue_y: u32 = buf.read_be()?;

            let source_chromaticities = SourceChromaticities {
                white: (
                    ScaledFloat::from_scaled(white_x),
                    ScaledFloat::from_scaled(white_y),
                ),
                red: (
                    ScaledFloat::from_scaled(red_x),
                    ScaledFloat::from_scaled(red_y),
                ),
                green: (
                    ScaledFloat::from_scaled(green_x),
                    ScaledFloat::from_scaled(green_y),
                ),
                blue: (
                    ScaledFloat::from_scaled(blue_x),
                    ScaledFloat::from_scaled(blue_y),
                ),
            };

            info.chrm_chunk = Some(source_chromaticities);
            // Ignore chromaticities if sRGB profile is used.
            if info.srgb.is_none() {
                info.source_chromaticities = Some(source_chromaticities);
            }

            Ok(Decoded::Nothing)
        }
    }

    fn parse_gama(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if self.have_idat {
            Err(DecodingError::Format(
                FormatErrorInner::AfterIdat { kind: chunk::gAMA }.into(),
            ))
        } else if info.gama_chunk.is_some() {
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::gAMA }.into(),
            ));
        } else {
            let mut buf = &self.current_chunk.raw_bytes[..];
            let source_gamma: u32 = buf.read_be()?;
            let source_gamma = ScaledFloat::from_scaled(source_gamma);

            info.gama_chunk = Some(source_gamma);
            // Ignore chromaticities if sRGB profile is used.
            if info.srgb.is_none() {
                info.source_gamma = Some(source_gamma);
            }

            Ok(Decoded::Nothing)
        }
    }

    fn parse_srgb(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if self.have_idat {
            Err(DecodingError::Format(
                FormatErrorInner::AfterIdat { kind: chunk::acTL }.into(),
            ))
        } else if info.srgb.is_some() {
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::sRGB }.into(),
            ));
        } else {
            let mut buf = &self.current_chunk.raw_bytes[..];
            let raw: u8 = buf.read_be()?; // BE is is nonsense for single bytes, but this way the size is checked.
            let rendering_intent = crate::SrgbRenderingIntent::from_raw(raw).ok_or_else(|| {
                FormatError::from(FormatErrorInner::InvalidSrgbRenderingIntent(raw))
            })?;

            // Set srgb and override source gamma and chromaticities.
            info.srgb = Some(rendering_intent);
            info.source_gamma = Some(crate::srgb::substitute_gamma());
            info.source_chromaticities = Some(crate::srgb::substitute_chromaticities());
            Ok(Decoded::Nothing)
        }
    }

    fn parse_iccp(&mut self) -> Result<Decoded, DecodingError> {
        let info = self.info.as_mut().unwrap();
        if self.have_idat {
            Err(DecodingError::Format(
                FormatErrorInner::AfterIdat { kind: chunk::iCCP }.into(),
            ))
        } else {
            if info.icc_profile.is_some() {
                log::warn!("iCCP chunks must appear at most once");
            }

            let mut buf = &self.current_chunk.raw_bytes[..];

            // read profile name
            let _: u8 = buf.read_be()?;
            for _ in 1..80 {
                let raw: u8 = buf.read_be()?;
                if raw == 0 {
                    break;
                }
            }

            match buf.read_be()? {
                // compression method
                0u8 => (),
                n => {
                    return Err(DecodingError::Format(
                        FormatErrorInner::UnknownCompressionMethod(n).into(),
                    ))
                }
            }

            let mut profile = Vec::new();
            let mut inflater = ZlibStream::new();
            while !buf.is_empty() {
                let consumed_bytes = inflater.decompress(buf, &mut profile)?;
                if profile.len() > 8000000 {
                    // TODO: this should use Limits.bytes
                    return Err(DecodingError::LimitsExceeded);
                }
                buf = &buf[consumed_bytes..];
            }

            info.icc_profile = Some(Cow::Owned(profile));
            Ok(Decoded::Nothing)
        }
    }

    fn parse_ihdr(&mut self) -> Result<Decoded, DecodingError> {
        if self.info.is_some() {
            return Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: IHDR }.into(),
            ));
        }
        let mut buf = &self.current_chunk.raw_bytes[..];
        let width = buf.read_be()?;
        let height = buf.read_be()?;
        let bit_depth = buf.read_be()?;
        let bit_depth = match BitDepth::from_u8(bit_depth) {
            Some(bits) => bits,
            None => {
                return Err(DecodingError::Format(
                    FormatErrorInner::InvalidBitDepth(bit_depth).into(),
                ))
            }
        };
        let color_type = buf.read_be()?;
        let color_type = match ColorType::from_u8(color_type) {
            Some(color_type) => {
                if color_type.is_combination_invalid(bit_depth) {
                    return Err(DecodingError::Format(
                        FormatErrorInner::InvalidColorBitDepth {
                            color_type,
                            bit_depth,
                        }
                        .into(),
                    ));
                } else {
                    color_type
                }
            }
            None => {
                return Err(DecodingError::Format(
                    FormatErrorInner::InvalidColorType(color_type).into(),
                ))
            }
        };
        match buf.read_be()? {
            // compression method
            0u8 => (),
            n => {
                return Err(DecodingError::Format(
                    FormatErrorInner::UnknownCompressionMethod(n).into(),
                ))
            }
        }
        match buf.read_be()? {
            // filter method
            0u8 => (),
            n => {
                return Err(DecodingError::Format(
                    FormatErrorInner::UnknownFilterMethod(n).into(),
                ))
            }
        }
        let interlaced = match buf.read_be()? {
            0u8 => false,
            1 => true,
            n => {
                return Err(DecodingError::Format(
                    FormatErrorInner::UnknownInterlaceMethod(n).into(),
                ))
            }
        };

        self.info = Some(Info {
            width,
            height,
            bit_depth,
            color_type,
            interlaced,
            ..Default::default()
        });

        Ok(Decoded::Header(
            width, height, bit_depth, color_type, interlaced,
        ))
    }

    fn split_keyword(buf: &[u8]) -> Result<(&[u8], &[u8]), DecodingError> {
        let null_byte_index = buf
            .iter()
            .position(|&b| b == 0)
            .ok_or_else(|| DecodingError::from(TextDecodingError::MissingNullSeparator))?;

        if null_byte_index == 0 || null_byte_index > 79 {
            return Err(DecodingError::from(TextDecodingError::InvalidKeywordSize));
        }

        Ok((&buf[..null_byte_index], &buf[null_byte_index + 1..]))
    }

    fn parse_text(&mut self) -> Result<Decoded, DecodingError> {
        let buf = &self.current_chunk.raw_bytes[..];

        let (keyword_slice, value_slice) = Self::split_keyword(buf)?;

        self.info
            .as_mut()
            .unwrap()
            .uncompressed_latin1_text
            .push(TEXtChunk::decode(keyword_slice, value_slice).map_err(DecodingError::from)?);

        Ok(Decoded::Nothing)
    }

    fn parse_ztxt(&mut self) -> Result<Decoded, DecodingError> {
        let buf = &self.current_chunk.raw_bytes[..];

        let (keyword_slice, value_slice) = Self::split_keyword(buf)?;

        let compression_method = *value_slice
            .get(0)
            .ok_or_else(|| DecodingError::from(TextDecodingError::InvalidCompressionMethod))?;

        let text_slice = &value_slice[1..];

        self.info.as_mut().unwrap().compressed_latin1_text.push(
            ZTXtChunk::decode(keyword_slice, compression_method, text_slice)
                .map_err(DecodingError::from)?,
        );

        Ok(Decoded::Nothing)
    }

    fn parse_itxt(&mut self) -> Result<Decoded, DecodingError> {
        let buf = &self.current_chunk.raw_bytes[..];

        let (keyword_slice, value_slice) = Self::split_keyword(buf)?;

        let compression_flag = *value_slice
            .get(0)
            .ok_or_else(|| DecodingError::from(TextDecodingError::MissingCompressionFlag))?;

        let compression_method = *value_slice
            .get(1)
            .ok_or_else(|| DecodingError::from(TextDecodingError::InvalidCompressionMethod))?;

        let second_null_byte_index = value_slice[2..]
            .iter()
            .position(|&b| b == 0)
            .ok_or_else(|| DecodingError::from(TextDecodingError::MissingNullSeparator))?
            + 2;

        let language_tag_slice = &value_slice[2..second_null_byte_index];

        let third_null_byte_index = value_slice[second_null_byte_index + 1..]
            .iter()
            .position(|&b| b == 0)
            .ok_or_else(|| DecodingError::from(TextDecodingError::MissingNullSeparator))?
            + (second_null_byte_index + 1);

        let translated_keyword_slice =
            &value_slice[second_null_byte_index + 1..third_null_byte_index];

        let text_slice = &value_slice[third_null_byte_index + 1..];

        self.info.as_mut().unwrap().utf8_text.push(
            ITXtChunk::decode(
                keyword_slice,
                compression_flag,
                compression_method,
                language_tag_slice,
                translated_keyword_slice,
                text_slice,
            )
            .map_err(DecodingError::from)?,
        );

        Ok(Decoded::Nothing)
    }
}

impl Info<'_> {
    fn validate(&self, fc: &FrameControl) -> Result<(), DecodingError> {
        // Validate mathematically: fc.width + fc.x_offset <= self.width
        let in_x_bounds = Some(fc.width) <= self.width.checked_sub(fc.x_offset);
        // Validate mathematically: fc.height + fc.y_offset <= self.height
        let in_y_bounds = Some(fc.height) <= self.height.checked_sub(fc.y_offset);

        if !in_x_bounds || !in_y_bounds {
            return Err(DecodingError::Format(
                // TODO: do we want to display the bad bounds?
                FormatErrorInner::BadSubFrameBounds {}.into(),
            ));
        }

        Ok(())
    }
}

impl Default for StreamingDecoder {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ChunkState {
    fn default() -> Self {
        ChunkState {
            type_: ChunkType([0; 4]),
            crc: Crc32::new(),
            remaining: 0,
            raw_bytes: Vec::with_capacity(CHUNCK_BUFFER_SIZE),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ScaledFloat;
    use super::SourceChromaticities;
    use std::fs::File;

    #[test]
    fn image_gamma() -> Result<(), ()> {
        fn trial(path: &str, expected: Option<ScaledFloat>) {
            let decoder = crate::Decoder::new(File::open(path).unwrap());
            let reader = decoder.read_info().unwrap();
            let actual: Option<ScaledFloat> = reader.info().source_gamma;
            assert!(actual == expected);
        }
        trial("tests/pngsuite/f00n0g08.png", None);
        trial("tests/pngsuite/f00n2c08.png", None);
        trial("tests/pngsuite/f01n0g08.png", None);
        trial("tests/pngsuite/f01n2c08.png", None);
        trial("tests/pngsuite/f02n0g08.png", None);
        trial("tests/pngsuite/f02n2c08.png", None);
        trial("tests/pngsuite/f03n0g08.png", None);
        trial("tests/pngsuite/f03n2c08.png", None);
        trial("tests/pngsuite/f04n0g08.png", None);
        trial("tests/pngsuite/f04n2c08.png", None);
        trial("tests/pngsuite/f99n0g04.png", None);
        trial("tests/pngsuite/tm3n3p02.png", None);
        trial("tests/pngsuite/g03n0g16.png", Some(ScaledFloat::new(0.35)));
        trial("tests/pngsuite/g03n2c08.png", Some(ScaledFloat::new(0.35)));
        trial("tests/pngsuite/g03n3p04.png", Some(ScaledFloat::new(0.35)));
        trial("tests/pngsuite/g04n0g16.png", Some(ScaledFloat::new(0.45)));
        trial("tests/pngsuite/g04n2c08.png", Some(ScaledFloat::new(0.45)));
        trial("tests/pngsuite/g04n3p04.png", Some(ScaledFloat::new(0.45)));
        trial("tests/pngsuite/g05n0g16.png", Some(ScaledFloat::new(0.55)));
        trial("tests/pngsuite/g05n2c08.png", Some(ScaledFloat::new(0.55)));
        trial("tests/pngsuite/g05n3p04.png", Some(ScaledFloat::new(0.55)));
        trial("tests/pngsuite/g07n0g16.png", Some(ScaledFloat::new(0.7)));
        trial("tests/pngsuite/g07n2c08.png", Some(ScaledFloat::new(0.7)));
        trial("tests/pngsuite/g07n3p04.png", Some(ScaledFloat::new(0.7)));
        trial("tests/pngsuite/g10n0g16.png", Some(ScaledFloat::new(1.0)));
        trial("tests/pngsuite/g10n2c08.png", Some(ScaledFloat::new(1.0)));
        trial("tests/pngsuite/g10n3p04.png", Some(ScaledFloat::new(1.0)));
        trial("tests/pngsuite/g25n0g16.png", Some(ScaledFloat::new(2.5)));
        trial("tests/pngsuite/g25n2c08.png", Some(ScaledFloat::new(2.5)));
        trial("tests/pngsuite/g25n3p04.png", Some(ScaledFloat::new(2.5)));
        Ok(())
    }

    #[test]
    fn image_source_chromaticities() -> Result<(), ()> {
        fn trial(path: &str, expected: Option<SourceChromaticities>) {
            let decoder = crate::Decoder::new(File::open(path).unwrap());
            let reader = decoder.read_info().unwrap();
            let actual: Option<SourceChromaticities> = reader.info().source_chromaticities;
            assert!(actual == expected);
        }
        trial(
            "tests/pngsuite/ccwn2c08.png",
            Some(SourceChromaticities::new(
                (0.3127, 0.3290),
                (0.64, 0.33),
                (0.30, 0.60),
                (0.15, 0.06),
            )),
        );
        trial(
            "tests/pngsuite/ccwn3p08.png",
            Some(SourceChromaticities::new(
                (0.3127, 0.3290),
                (0.64, 0.33),
                (0.30, 0.60),
                (0.15, 0.06),
            )),
        );
        trial("tests/pngsuite/basi0g01.png", None);
        trial("tests/pngsuite/basi0g02.png", None);
        trial("tests/pngsuite/basi0g04.png", None);
        trial("tests/pngsuite/basi0g08.png", None);
        trial("tests/pngsuite/basi0g16.png", None);
        trial("tests/pngsuite/basi2c08.png", None);
        trial("tests/pngsuite/basi2c16.png", None);
        trial("tests/pngsuite/basi3p01.png", None);
        trial("tests/pngsuite/basi3p02.png", None);
        trial("tests/pngsuite/basi3p04.png", None);
        trial("tests/pngsuite/basi3p08.png", None);
        trial("tests/pngsuite/basi4a08.png", None);
        trial("tests/pngsuite/basi4a16.png", None);
        trial("tests/pngsuite/basi6a08.png", None);
        trial("tests/pngsuite/basi6a16.png", None);
        trial("tests/pngsuite/basn0g01.png", None);
        trial("tests/pngsuite/basn0g02.png", None);
        trial("tests/pngsuite/basn0g04.png", None);
        trial("tests/pngsuite/basn0g08.png", None);
        trial("tests/pngsuite/basn0g16.png", None);
        trial("tests/pngsuite/basn2c08.png", None);
        trial("tests/pngsuite/basn2c16.png", None);
        trial("tests/pngsuite/basn3p01.png", None);
        trial("tests/pngsuite/basn3p02.png", None);
        trial("tests/pngsuite/basn3p04.png", None);
        trial("tests/pngsuite/basn3p08.png", None);
        trial("tests/pngsuite/basn4a08.png", None);
        trial("tests/pngsuite/basn4a16.png", None);
        trial("tests/pngsuite/basn6a08.png", None);
        trial("tests/pngsuite/basn6a16.png", None);
        trial("tests/pngsuite/bgai4a08.png", None);
        trial("tests/pngsuite/bgai4a16.png", None);
        trial("tests/pngsuite/bgan6a08.png", None);
        trial("tests/pngsuite/bgan6a16.png", None);
        trial("tests/pngsuite/bgbn4a08.png", None);
        trial("tests/pngsuite/bggn4a16.png", None);
        trial("tests/pngsuite/bgwn6a08.png", None);
        trial("tests/pngsuite/bgyn6a16.png", None);
        trial("tests/pngsuite/cdfn2c08.png", None);
        trial("tests/pngsuite/cdhn2c08.png", None);
        trial("tests/pngsuite/cdsn2c08.png", None);
        trial("tests/pngsuite/cdun2c08.png", None);
        trial("tests/pngsuite/ch1n3p04.png", None);
        trial("tests/pngsuite/ch2n3p08.png", None);
        trial("tests/pngsuite/cm0n0g04.png", None);
        trial("tests/pngsuite/cm7n0g04.png", None);
        trial("tests/pngsuite/cm9n0g04.png", None);
        trial("tests/pngsuite/cs3n2c16.png", None);
        trial("tests/pngsuite/cs3n3p08.png", None);
        trial("tests/pngsuite/cs5n2c08.png", None);
        trial("tests/pngsuite/cs5n3p08.png", None);
        trial("tests/pngsuite/cs8n2c08.png", None);
        trial("tests/pngsuite/cs8n3p08.png", None);
        trial("tests/pngsuite/ct0n0g04.png", None);
        trial("tests/pngsuite/ct1n0g04.png", None);
        trial("tests/pngsuite/cten0g04.png", None);
        trial("tests/pngsuite/ctfn0g04.png", None);
        trial("tests/pngsuite/ctgn0g04.png", None);
        trial("tests/pngsuite/cthn0g04.png", None);
        trial("tests/pngsuite/ctjn0g04.png", None);
        trial("tests/pngsuite/ctzn0g04.png", None);
        trial("tests/pngsuite/f00n0g08.png", None);
        trial("tests/pngsuite/f00n2c08.png", None);
        trial("tests/pngsuite/f01n0g08.png", None);
        trial("tests/pngsuite/f01n2c08.png", None);
        trial("tests/pngsuite/f02n0g08.png", None);
        trial("tests/pngsuite/f02n2c08.png", None);
        trial("tests/pngsuite/f03n0g08.png", None);
        trial("tests/pngsuite/f03n2c08.png", None);
        trial("tests/pngsuite/f04n0g08.png", None);
        trial("tests/pngsuite/f04n2c08.png", None);
        trial("tests/pngsuite/f99n0g04.png", None);
        trial("tests/pngsuite/g03n0g16.png", None);
        trial("tests/pngsuite/g03n2c08.png", None);
        trial("tests/pngsuite/g03n3p04.png", None);
        trial("tests/pngsuite/g04n0g16.png", None);
        trial("tests/pngsuite/g04n2c08.png", None);
        trial("tests/pngsuite/g04n3p04.png", None);
        trial("tests/pngsuite/g05n0g16.png", None);
        trial("tests/pngsuite/g05n2c08.png", None);
        trial("tests/pngsuite/g05n3p04.png", None);
        trial("tests/pngsuite/g07n0g16.png", None);
        trial("tests/pngsuite/g07n2c08.png", None);
        trial("tests/pngsuite/g07n3p04.png", None);
        trial("tests/pngsuite/g10n0g16.png", None);
        trial("tests/pngsuite/g10n2c08.png", None);
        trial("tests/pngsuite/g10n3p04.png", None);
        trial("tests/pngsuite/g25n0g16.png", None);
        trial("tests/pngsuite/g25n2c08.png", None);
        trial("tests/pngsuite/g25n3p04.png", None);
        trial("tests/pngsuite/oi1n0g16.png", None);
        trial("tests/pngsuite/oi1n2c16.png", None);
        trial("tests/pngsuite/oi2n0g16.png", None);
        trial("tests/pngsuite/oi2n2c16.png", None);
        trial("tests/pngsuite/oi4n0g16.png", None);
        trial("tests/pngsuite/oi4n2c16.png", None);
        trial("tests/pngsuite/oi9n0g16.png", None);
        trial("tests/pngsuite/oi9n2c16.png", None);
        trial("tests/pngsuite/PngSuite.png", None);
        trial("tests/pngsuite/pp0n2c16.png", None);
        trial("tests/pngsuite/pp0n6a08.png", None);
        trial("tests/pngsuite/ps1n0g08.png", None);
        trial("tests/pngsuite/ps1n2c16.png", None);
        trial("tests/pngsuite/ps2n0g08.png", None);
        trial("tests/pngsuite/ps2n2c16.png", None);
        trial("tests/pngsuite/s01i3p01.png", None);
        trial("tests/pngsuite/s01n3p01.png", None);
        trial("tests/pngsuite/s02i3p01.png", None);
        trial("tests/pngsuite/s02n3p01.png", None);
        trial("tests/pngsuite/s03i3p01.png", None);
        trial("tests/pngsuite/s03n3p01.png", None);
        trial("tests/pngsuite/s04i3p01.png", None);
        trial("tests/pngsuite/s04n3p01.png", None);
        trial("tests/pngsuite/s05i3p02.png", None);
        trial("tests/pngsuite/s05n3p02.png", None);
        trial("tests/pngsuite/s06i3p02.png", None);
        trial("tests/pngsuite/s06n3p02.png", None);
        trial("tests/pngsuite/s07i3p02.png", None);
        trial("tests/pngsuite/s07n3p02.png", None);
        trial("tests/pngsuite/s08i3p02.png", None);
        trial("tests/pngsuite/s08n3p02.png", None);
        trial("tests/pngsuite/s09i3p02.png", None);
        trial("tests/pngsuite/s09n3p02.png", None);
        trial("tests/pngsuite/s32i3p04.png", None);
        trial("tests/pngsuite/s32n3p04.png", None);
        trial("tests/pngsuite/s33i3p04.png", None);
        trial("tests/pngsuite/s33n3p04.png", None);
        trial("tests/pngsuite/s34i3p04.png", None);
        trial("tests/pngsuite/s34n3p04.png", None);
        trial("tests/pngsuite/s35i3p04.png", None);
        trial("tests/pngsuite/s35n3p04.png", None);
        trial("tests/pngsuite/s36i3p04.png", None);
        trial("tests/pngsuite/s36n3p04.png", None);
        trial("tests/pngsuite/s37i3p04.png", None);
        trial("tests/pngsuite/s37n3p04.png", None);
        trial("tests/pngsuite/s38i3p04.png", None);
        trial("tests/pngsuite/s38n3p04.png", None);
        trial("tests/pngsuite/s39i3p04.png", None);
        trial("tests/pngsuite/s39n3p04.png", None);
        trial("tests/pngsuite/s40i3p04.png", None);
        trial("tests/pngsuite/s40n3p04.png", None);
        trial("tests/pngsuite/tbbn0g04.png", None);
        trial("tests/pngsuite/tbbn2c16.png", None);
        trial("tests/pngsuite/tbbn3p08.png", None);
        trial("tests/pngsuite/tbgn2c16.png", None);
        trial("tests/pngsuite/tbgn3p08.png", None);
        trial("tests/pngsuite/tbrn2c08.png", None);
        trial("tests/pngsuite/tbwn0g16.png", None);
        trial("tests/pngsuite/tbwn3p08.png", None);
        trial("tests/pngsuite/tbyn3p08.png", None);
        trial("tests/pngsuite/tm3n3p02.png", None);
        trial("tests/pngsuite/tp0n0g08.png", None);
        trial("tests/pngsuite/tp0n2c08.png", None);
        trial("tests/pngsuite/tp0n3p08.png", None);
        trial("tests/pngsuite/tp1n3p08.png", None);
        trial("tests/pngsuite/z00n2c08.png", None);
        trial("tests/pngsuite/z03n2c08.png", None);
        trial("tests/pngsuite/z06n2c08.png", None);
        Ok(())
    }
}
