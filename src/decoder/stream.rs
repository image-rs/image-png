extern crate crc32fast;

use std::convert::{From, TryInto};
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
use crate::Limits;

/// TODO check if these size are reasonable
pub const CHUNCK_BUFFER_SIZE: usize = 32 * 1024;

/// Determines if checksum checks should be disabled globally.
///
/// This is used only in fuzzing. `afl` automatically adds `--cfg fuzzing` to RUSTFLAGS which can
/// be used to detect that build.
const CHECKSUM_DISABLED: bool = cfg!(fuzzing);

/// Kind of `u32` value that is being read via `State::U32`.
#[derive(Debug)]
enum U32ValueKind {
    /// First 4 bytes of the PNG signature - see
    /// http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#PNG-file-signature
    Signature1stU32,
    /// Second 4 bytes of the PNG signature - see
    /// http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#PNG-file-signature
    Signature2ndU32,
    /// Chunk length - see
    /// http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#Chunk-layout
    Length,
    /// Chunk type - see
    /// http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#Chunk-layout
    Type { length: u32 },
    /// Chunk checksum - see
    /// http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html#Chunk-layout
    Crc(ChunkType),
    /// Sequence number from an `fdAT` chunk - see
    /// https://wiki.mozilla.org/APNG_Specification#.60fdAT.60:_The_Frame_Data_Chunk
    ApngSequenceNumber,
}

#[derive(Debug)]
enum State {
    /// In this state we are reading a u32 value from external input.  We start with
    /// `accumulated_count` set to `0`. After reading or accumulating the required 4 bytes we will
    /// call `parse_32` which will then move onto the next state.
    U32 {
        kind: U32ValueKind,
        bytes: [u8; 4],
        accumulated_count: usize,
    },
    /// In this state we are reading chunk data from external input, and appending it to
    /// `ChunkState::raw_bytes`.
    ReadChunkData(ChunkType),
    /// In this state we check if all chunk data has been already read into `ChunkState::raw_bytes`
    /// and if so then we parse the chunk.  Otherwise, we go back to the `ReadChunkData` state.
    ParseChunkData(ChunkType),
    /// In this state we are reading image data from external input and feeding it directly into
    /// `StreamingDecoder::inflater`.
    ImageData(ChunkType),
}

impl State {
    fn new_u32(kind: U32ValueKind) -> Self {
        Self::U32 {
            kind,
            bytes: [0; 4],
            accumulated_count: 0,
        }
    }
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
    /// The image width or height is zero.
    InvalidDimensions,
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
        err: fdeflate::DecompressionError,
    },
    /// The image data chunk was too short for the expected pixel count.
    NoMoreImageData,
    /// Bad text encoding
    BadTextEncoding(TextDecodingError),
    /// fdAT shorter than 4 bytes
    FdatShorterThanFourBytes,
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
            InvalidDimensions => write!(fmt, "Invalid image dimensions"),
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
                write!(fmt, "{:?}", err)
            }
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
            FdatShorterThanFourBytes => write!(fmt, "fdAT chunk shorter than 4 bytes"),
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

/// Decoder configuration options
#[derive(Clone)]
pub struct DecodeOptions {
    ignore_adler32: bool,
    ignore_crc: bool,
    ignore_text_chunk: bool,
    skip_ancillary_crc_failures: bool,
}

impl Default for DecodeOptions {
    fn default() -> Self {
        Self {
            ignore_adler32: true,
            ignore_crc: false,
            ignore_text_chunk: false,
            skip_ancillary_crc_failures: true,
        }
    }
}

impl DecodeOptions {
    /// When set, the decoder will not compute and verify the Adler-32 checksum.
    ///
    /// Defaults to `true`.
    pub fn set_ignore_adler32(&mut self, ignore_adler32: bool) {
        self.ignore_adler32 = ignore_adler32;
    }

    /// When set, the decoder will not compute and verify the CRC code.
    ///
    /// Defaults to `false`.
    pub fn set_ignore_crc(&mut self, ignore_crc: bool) {
        self.ignore_crc = ignore_crc;
    }

    /// Flag to ignore computing and verifying the Adler-32 checksum and CRC
    /// code.
    pub fn set_ignore_checksums(&mut self, ignore_checksums: bool) {
        self.ignore_adler32 = ignore_checksums;
        self.ignore_crc = ignore_checksums;
    }

    /// Ignore text chunks while decoding.
    ///
    /// Defaults to `false`.
    pub fn set_ignore_text_chunk(&mut self, ignore_text_chunk: bool) {
        self.ignore_text_chunk = ignore_text_chunk;
    }

    /// Ignore ancillary chunks if CRC fails
    ///
    /// Defaults to `true`
    pub fn set_skip_ancillary_crc_failures(&mut self, skip_ancillary_crc_failures: bool) {
        self.skip_ancillary_crc_failures = skip_ancillary_crc_failures;
    }
}

/// PNG StreamingDecoder (low-level interface)
///
/// By default, the decoder does not verify Adler-32 checksum computation. To
/// enable checksum verification, set it with [`StreamingDecoder::set_ignore_adler32`]
/// before starting decompression.
pub struct StreamingDecoder {
    state: Option<State>,
    current_chunk: ChunkState,
    /// The inflater state handling consecutive `IDAT` and `fdAT` chunks.
    inflater: ZlibStream,
    /// The complete image info read from all prior chunks.
    pub(crate) info: Option<Info<'static>>,
    /// The animation chunk sequence number.
    current_seq_no: Option<u32>,
    /// Whether we have already seen a start of an IDAT chunk.  (Used to validate chunk ordering -
    /// some chunk types can only appear before or after an IDAT chunk.)
    have_idat: bool,
    decode_options: DecodeOptions,
    pub(crate) limits: Limits,
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
        StreamingDecoder::new_with_options(DecodeOptions::default())
    }

    pub fn new_with_options(decode_options: DecodeOptions) -> StreamingDecoder {
        let mut inflater = ZlibStream::new();
        inflater.set_ignore_adler32(decode_options.ignore_adler32);

        StreamingDecoder {
            state: Some(State::new_u32(U32ValueKind::Signature1stU32)),
            current_chunk: ChunkState::default(),
            inflater,
            info: None,
            current_seq_no: None,
            have_idat: false,
            decode_options,
            limits: Limits { bytes: usize::MAX },
        }
    }

    /// Resets the StreamingDecoder
    pub fn reset(&mut self) {
        self.state = Some(State::new_u32(U32ValueKind::Signature1stU32));
        self.current_chunk.crc = Crc32::new();
        self.current_chunk.remaining = 0;
        self.current_chunk.raw_bytes.clear();
        self.inflater.reset();
        self.info = None;
        self.current_seq_no = None;
        self.have_idat = false;
    }

    /// Provides access to the inner `info` field
    pub fn info(&self) -> Option<&Info<'static>> {
        self.info.as_ref()
    }

    pub fn set_ignore_text_chunk(&mut self, ignore_text_chunk: bool) {
        self.decode_options.set_ignore_text_chunk(ignore_text_chunk);
    }

    /// Return whether the decoder is set to ignore the Adler-32 checksum.
    pub fn ignore_adler32(&self) -> bool {
        self.inflater.ignore_adler32()
    }

    /// Set whether to compute and verify the Adler-32 checksum during
    /// decompression. Return `true` if the flag was successfully set.
    ///
    /// The decoder defaults to `true`.
    ///
    /// This flag cannot be modified after decompression has started until the
    /// [`StreamingDecoder`] is reset.
    pub fn set_ignore_adler32(&mut self, ignore_adler32: bool) -> bool {
        self.inflater.set_ignore_adler32(ignore_adler32)
    }

    /// Set whether to compute and verify the Adler-32 checksum during
    /// decompression.
    ///
    /// The decoder defaults to `false`.
    pub fn set_ignore_crc(&mut self, ignore_crc: bool) {
        self.decode_options.set_ignore_crc(ignore_crc)
    }

    /// Ignore ancillary chunks if CRC fails
    ///
    /// Defaults to `true`
    pub fn set_skip_ancillary_crc_failures(&mut self, skip_ancillary_crc_failures: bool) {
        self.decode_options
            .set_skip_ancillary_crc_failures(skip_ancillary_crc_failures)
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

    fn next_state(
        &mut self,
        buf: &[u8],
        image_data: &mut Vec<u8>,
    ) -> Result<(usize, Decoded), DecodingError> {
        use self::State::*;

        // Driver should ensure that state is never None
        let state = self.state.take().unwrap();

        match state {
            U32 {
                kind,
                mut bytes,
                mut accumulated_count,
            } => {
                debug_assert!(accumulated_count <= 4);
                if accumulated_count == 0 && buf.len() >= 4 {
                    // Handling these `accumulated_count` and `buf.len()` values in a separate `if`
                    // branch is not strictly necessary - the `else` statement below is already
                    // capable of handling these values.  The main reason for special-casing these
                    // values is that they occur fairly frequently and special-casing them results
                    // in performance gains.
                    const CONSUMED_BYTES: usize = 4;
                    self.parse_u32(kind, &buf[0..4], image_data)
                        .map(|decoded| (CONSUMED_BYTES, decoded))
                } else {
                    let remaining_count = 4 - accumulated_count;
                    let consumed_bytes = {
                        let available_count = min(remaining_count, buf.len());
                        bytes[accumulated_count..accumulated_count + available_count]
                            .copy_from_slice(&buf[0..available_count]);
                        accumulated_count += available_count;
                        available_count
                    };

                    if accumulated_count < 4 {
                        self.state = Some(U32 {
                            kind,
                            bytes,
                            accumulated_count,
                        });
                        Ok((consumed_bytes, Decoded::Nothing))
                    } else {
                        debug_assert_eq!(accumulated_count, 4);
                        self.parse_u32(kind, &bytes, image_data)
                            .map(|decoded| (consumed_bytes, decoded))
                    }
                }
            }
            ParseChunkData(type_str) => {
                debug_assert!(type_str != IDAT && type_str != chunk::fdAT);
                if self.current_chunk.remaining == 0 {
                    // Got complete chunk.
                    Ok((0, self.parse_chunk(type_str)?))
                } else {
                    // Make sure we have room to read more of the chunk.
                    // We need it fully before parsing.
                    self.reserve_current_chunk()?;

                    self.state = Some(ReadChunkData(type_str));
                    Ok((0, Decoded::PartialChunk(type_str)))
                }
            }
            ReadChunkData(type_str) => {
                debug_assert!(type_str != IDAT && type_str != chunk::fdAT);
                if self.current_chunk.remaining == 0 {
                    self.state = Some(State::new_u32(U32ValueKind::Crc(type_str)));
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
                        self.state = Some(ParseChunkData(type_str));
                        Ok((0, Decoded::Nothing))
                    } else {
                        let buf = &buf[..n as usize];
                        if !self.decode_options.ignore_crc {
                            crc.update(buf);
                        }
                        raw_bytes.extend_from_slice(buf);

                        *remaining -= n;
                        if *remaining == 0 {
                            self.state = Some(ParseChunkData(type_str));
                        } else {
                            self.state = Some(ReadChunkData(type_str));
                        }
                        Ok((n as usize, Decoded::Nothing))
                    }
                }
            }
            ImageData(type_str) => {
                debug_assert!(type_str == IDAT || type_str == chunk::fdAT);
                let len = std::cmp::min(buf.len(), self.current_chunk.remaining as usize);
                let buf = &buf[..len];
                let consumed = self.inflater.decompress(buf, image_data)?;
                self.current_chunk.crc.update(&buf[..consumed]);
                self.current_chunk.remaining -= consumed as u32;
                if self.current_chunk.remaining == 0 {
                    self.state = Some(State::new_u32(U32ValueKind::Crc(type_str)));
                } else {
                    self.state = Some(ImageData(type_str));
                }
                Ok((consumed, Decoded::ImageData))
            }
        }
    }

    fn parse_u32(
        &mut self,
        kind: U32ValueKind,
        u32_be_bytes: &[u8],
        image_data: &mut Vec<u8>,
    ) -> Result<Decoded, DecodingError> {
        debug_assert_eq!(u32_be_bytes.len(), 4);
        let bytes = u32_be_bytes.try_into().unwrap();
        let val = u32::from_be_bytes(bytes);

        match kind {
            U32ValueKind::Signature1stU32 => {
                if bytes == [137, 80, 78, 71] {
                    self.state = Some(State::new_u32(U32ValueKind::Signature2ndU32));
                    Ok(Decoded::Nothing)
                } else {
                    Err(DecodingError::Format(
                        FormatErrorInner::InvalidSignature.into(),
                    ))
                }
            }
            U32ValueKind::Signature2ndU32 => {
                if bytes == [13, 10, 26, 10] {
                    self.state = Some(State::new_u32(U32ValueKind::Length));
                    Ok(Decoded::Nothing)
                } else {
                    Err(DecodingError::Format(
                        FormatErrorInner::InvalidSignature.into(),
                    ))
                }
            }
            U32ValueKind::Length => {
                self.state = Some(State::new_u32(U32ValueKind::Type { length: val }));
                Ok(Decoded::Nothing)
            }
            U32ValueKind::Type { length } => {
                let type_str = ChunkType(bytes);
                if self.info.is_none() && type_str != IHDR {
                    return Err(DecodingError::Format(
                        FormatErrorInner::ChunkBeforeIhdr { kind: type_str }.into(),
                    ));
                }
                if type_str != self.current_chunk.type_
                    && (self.current_chunk.type_ == IDAT || self.current_chunk.type_ == chunk::fdAT)
                {
                    self.current_chunk.type_ = type_str;
                    self.inflater.finish_compressed_chunks(image_data)?;
                    self.inflater.reset();
                    self.state = Some(State::U32 {
                        kind,
                        bytes,
                        accumulated_count: 4,
                    });
                    return Ok(Decoded::ImageDataFlushed);
                }
                self.current_chunk.type_ = type_str;
                if !self.decode_options.ignore_crc {
                    self.current_chunk.crc.reset();
                    self.current_chunk.crc.update(&type_str.0);
                }
                self.current_chunk.remaining = length;
                self.current_chunk.raw_bytes.clear();
                self.state = match type_str {
                    chunk::fdAT => {
                        if length < 4 {
                            return Err(DecodingError::Format(
                                FormatErrorInner::FdatShorterThanFourBytes.into(),
                            ));
                        }
                        Some(State::new_u32(U32ValueKind::ApngSequenceNumber))
                    }
                    IDAT => {
                        self.have_idat = true;
                        Some(State::ImageData(type_str))
                    }
                    _ => Some(State::ReadChunkData(type_str)),
                };
                Ok(Decoded::ChunkBegin(length, type_str))
            }
            U32ValueKind::Crc(type_str) => {
                // If ignore_crc is set, do not calculate CRC. We set
                // sum=val so that it short-circuits to true in the next
                // if-statement block
                let sum = if self.decode_options.ignore_crc {
                    val
                } else {
                    self.current_chunk.crc.clone().finalize()
                };

                if val == sum || CHECKSUM_DISABLED {
                    self.state = Some(State::new_u32(U32ValueKind::Length));
                    if type_str == IEND {
                        Ok(Decoded::ImageEnd)
                    } else {
                        Ok(Decoded::ChunkComplete(val, type_str))
                    }
                } else if self.decode_options.skip_ancillary_crc_failures
                    && !chunk::is_critical(type_str)
                {
                    // Ignore ancillary chunk with invalid CRC
                    self.state = Some(State::new_u32(U32ValueKind::Length));
                    Ok(Decoded::Nothing)
                } else {
                    Err(DecodingError::Format(
                        FormatErrorInner::CrcMismatch {
                            crc_val: val,
                            crc_sum: sum,
                            chunk: type_str,
                        }
                        .into(),
                    ))
                }
            }
            U32ValueKind::ApngSequenceNumber => {
                debug_assert_eq!(self.current_chunk.type_, chunk::fdAT);
                let next_seq_no = val;

                // Should be verified by the FdatShorterThanFourBytes check earlier.
                debug_assert!(self.current_chunk.remaining >= 4);
                self.current_chunk.remaining -= 4;

                if let Some(seq_no) = self.current_seq_no {
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
                } else {
                    return Err(DecodingError::Format(FormatErrorInner::MissingFctl.into()));
                }

                if !self.decode_options.ignore_crc {
                    let data = next_seq_no.to_be_bytes();
                    self.current_chunk.crc.update(&data);
                }

                self.state = Some(State::ImageData(chunk::fdAT));
                Ok(Decoded::PartialChunk(chunk::fdAT))
            }
        }
    }

    fn reserve_current_chunk(&mut self) -> Result<(), DecodingError> {
        let max = self.limits.bytes;
        let buffer = &mut self.current_chunk.raw_bytes;

        // Double if necessary, but no more than until the limit is reached.
        let reserve_size = max.saturating_sub(buffer.capacity()).min(buffer.len());
        self.limits.reserve_bytes(reserve_size)?;
        buffer.reserve_exact(reserve_size);

        if buffer.capacity() == buffer.len() {
            Err(DecodingError::LimitsExceeded)
        } else {
            Ok(())
        }
    }

    fn parse_chunk(&mut self, type_str: ChunkType) -> Result<Decoded, DecodingError> {
        self.state = Some(State::new_u32(U32ValueKind::Crc(type_str)));
        let parse_result = match type_str {
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
            chunk::tEXt if !self.decode_options.ignore_text_chunk => self.parse_text(),
            chunk::zTXt if !self.decode_options.ignore_text_chunk => self.parse_ztxt(),
            chunk::iTXt if !self.decode_options.ignore_text_chunk => self.parse_itxt(),
            _ => Ok(Decoded::PartialChunk(type_str)),
        };

        if parse_result.is_err() {
            self.state = None;
        }
        parse_result
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
        self.inflater.reset();
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
            Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::PLTE }.into(),
            ))
        } else {
            self.limits
                .reserve_bytes(self.current_chunk.raw_bytes.len())?;
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
        self.limits
            .reserve_bytes(self.current_chunk.raw_bytes.len())?;
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
            Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::pHYs }.into(),
            ))
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
            Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::cHRM }.into(),
            ))
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
            Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::gAMA }.into(),
            ))
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
            Err(DecodingError::Format(
                FormatErrorInner::DuplicateChunk { kind: chunk::sRGB }.into(),
            ))
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
        } else if info.icc_profile.is_some() {
            // We have already encountered an iCCP chunk before.
            //
            // Section "4.2.2.4. iCCP Embedded ICC profile" of the spec says:
            //   > A file should contain at most one embedded profile,
            //   > whether explicit like iCCP or implicit like sRGB.
            // but
            //   * This is a "should", not a "must"
            //   * The spec also says that "All ancillary chunks are optional, in the sense that
            //     [...] decoders can ignore them."
            //   * The reference implementation (libpng) ignores the subsequent iCCP chunks
            //     (treating them as a benign error).
            Ok(Decoded::Nothing)
        } else {
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

            match fdeflate::decompress_to_vec_bounded(buf, self.limits.bytes) {
                Ok(profile) => {
                    self.limits.reserve_bytes(profile.len())?;
                    info.icc_profile = Some(Cow::Owned(profile));
                }
                Err(fdeflate::BoundedDecompressionError::DecompressionError { inner: err }) => {
                    return Err(DecodingError::Format(
                        FormatErrorInner::CorruptFlateStream { err }.into(),
                    ))
                }
                Err(fdeflate::BoundedDecompressionError::OutputTooLarge { .. }) => {
                    return Err(DecodingError::LimitsExceeded);
                }
            }

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
        if width == 0 || height == 0 {
            return Err(DecodingError::Format(
                FormatErrorInner::InvalidDimensions.into(),
            ));
        }
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

        if let Some(mut raw_row_len) = color_type.checked_raw_row_length(bit_depth, width) {
            if interlaced {
                // This overshoots, but overestimating should be fine.
                // TODO: Calculate **exact** IDAT size for interlaced images.
                raw_row_len = raw_row_len.saturating_mul(2);
            }
            self.inflater
                .set_max_total_output((height as usize).saturating_mul(raw_row_len));
        }

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
        self.limits.reserve_bytes(buf.len())?;

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
        self.limits.reserve_bytes(buf.len())?;

        let (keyword_slice, value_slice) = Self::split_keyword(buf)?;

        let compression_method = *value_slice
            .first()
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
        self.limits.reserve_bytes(buf.len())?;

        let (keyword_slice, value_slice) = Self::split_keyword(buf)?;

        let compression_flag = *value_slice
            .first()
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
        if fc.width == 0 || fc.height == 0 {
            return Err(DecodingError::Format(
                FormatErrorInner::InvalidDimensions.into(),
            ));
        }

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
    use crate::test_utils::*;
    use crate::{Decoder, DecodingError};
    use byteorder::WriteBytesExt;
    use std::fs::File;
    use std::io::Write;

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

    /// Test handling of a PNG file that contains *two* iCCP chunks.
    /// This is a regression test for https://github.com/image-rs/image/issues/1825.
    #[test]
    fn test_two_iccp_chunks() {
        // The test file has been taken from
        // https://github.com/image-rs/image/issues/1825#issuecomment-1321798639,
        // but the 2nd iCCP chunk has been altered manually (see the 2nd comment below for more
        // details).
        let decoder = crate::Decoder::new(File::open("tests/bugfixes/issue#1825.png").unwrap());
        let reader = decoder.read_info().unwrap();
        let icc_profile = reader.info().icc_profile.clone().unwrap().into_owned();

        // Assert that the contents of the *first* iCCP chunk are returned.
        //
        // Note that the 2nd chunk in the test file has been manually altered to have a different
        // content (`b"test iccp contents"`) which would have a different CRC (797351983).
        assert_eq!(4070462061, crc32fast::hash(&icc_profile));
    }

    /// Writes an acTL chunk.
    /// See https://wiki.mozilla.org/APNG_Specification#.60acTL.60:_The_Animation_Control_Chunk
    fn write_actl(w: &mut impl Write, animation: &crate::AnimationControl) {
        let mut data = Vec::new();
        data.write_u32::<byteorder::BigEndian>(animation.num_frames)
            .unwrap();
        data.write_u32::<byteorder::BigEndian>(animation.num_plays)
            .unwrap();
        write_chunk(w, b"acTL", &data);
    }

    /// Writes an fcTL chunk.
    /// See https://wiki.mozilla.org/APNG_Specification#.60fcTL.60:_The_Frame_Control_Chunk
    fn write_fctl(w: &mut impl Write, frame: &crate::FrameControl) {
        let mut data = Vec::new();
        data.write_u32::<byteorder::BigEndian>(frame.sequence_number)
            .unwrap();
        data.write_u32::<byteorder::BigEndian>(frame.width).unwrap();
        data.write_u32::<byteorder::BigEndian>(frame.height)
            .unwrap();
        data.write_u32::<byteorder::BigEndian>(frame.x_offset)
            .unwrap();
        data.write_u32::<byteorder::BigEndian>(frame.y_offset)
            .unwrap();
        data.write_u16::<byteorder::BigEndian>(frame.delay_num)
            .unwrap();
        data.write_u16::<byteorder::BigEndian>(frame.delay_den)
            .unwrap();
        data.write_u8(frame.dispose_op as u8).unwrap();
        data.write_u8(frame.blend_op as u8).unwrap();
        write_chunk(w, b"fcTL", &data);
    }

    /// Writes an fdAT chunk.
    /// See https://wiki.mozilla.org/APNG_Specification#.60fdAT.60:_The_Frame_Data_Chunk
    fn write_fdat(w: &mut impl Write, sequence_number: u32, image_data: &[u8]) {
        let mut data = Vec::new();
        data.write_u32::<byteorder::BigEndian>(sequence_number)
            .unwrap();
        data.write_all(image_data).unwrap();
        write_chunk(w, b"fdAT", &data);
    }

    /// Writes PNG signature and chunks that can precede an fdAT chunk that is expected
    /// to have
    /// - `sequence_number` set to 0
    /// - image data with rgba8 pixels in a `width` by `width` image
    fn write_fdat_prefix(w: &mut impl Write, num_frames: u32, width: u32) {
        write_png_sig(w);
        write_rgba8_ihdr_with_width(w, width);
        write_actl(
            w,
            &crate::AnimationControl {
                num_frames,
                num_plays: 0,
            },
        );

        let mut fctl = crate::FrameControl {
            width,
            height: width,
            ..Default::default()
        };
        write_fctl(w, &fctl);
        write_rgba8_idats(w, width, 0x7fffffff);

        fctl.sequence_number += 1;
        write_fctl(w, &fctl);
    }

    #[test]
    fn test_fdat_chunk_payload_length_0() {
        let mut png = Vec::new();
        write_fdat_prefix(&mut png, 2, 8);
        write_chunk(&mut png, b"fdAT", &[]);

        let decoder = Decoder::new(png.as_slice());
        let mut reader = decoder.read_info().unwrap();
        let mut buf = vec![0; reader.output_buffer_size()];
        reader.next_frame(&mut buf).unwrap();

        // 0-length fdAT should result in an error.
        let err = reader.next_frame(&mut buf).unwrap_err();
        assert!(matches!(&err, DecodingError::Format(_)));
        let msg = format!("{err}");
        assert_eq!("fdAT chunk shorter than 4 bytes", msg);
    }

    #[test]
    fn test_fdat_chunk_payload_length_3() {
        let mut png = Vec::new();
        write_fdat_prefix(&mut png, 2, 8);
        write_chunk(&mut png, b"fdAT", &[1, 0, 0]);

        let decoder = Decoder::new(png.as_slice());
        let mut reader = decoder.read_info().unwrap();
        let mut buf = vec![0; reader.output_buffer_size()];
        reader.next_frame(&mut buf).unwrap();

        // 3-bytes-long fdAT should result in an error.
        let err = reader.next_frame(&mut buf).unwrap_err();
        assert!(matches!(&err, DecodingError::Format(_)));
        let msg = format!("{err}");
        assert_eq!("fdAT chunk shorter than 4 bytes", msg);
    }

    #[test]
    fn test_frame_split_across_two_fdat_chunks() {
        // Generate test data where the 2nd animation frame is split across 2 fdAT chunks.
        //
        // This is similar to the example given in
        // https://wiki.mozilla.org/APNG_Specification#Chunk_Sequence_Numbers:
        //
        // ```
        //    Sequence number    Chunk
        //    (none)             `acTL`
        //    0                  `fcTL` first frame
        //    (none)             `IDAT` first frame / default image
        //    1                  `fcTL` second frame
        //    2                  first `fdAT` for second frame
        //    3                  second `fdAT` for second frame
        // ```
        let png = {
            let mut png = Vec::new();
            write_fdat_prefix(&mut png, 2, 8);
            let image_data = generate_rgba8_with_width_and_height(8, 8);
            write_fdat(&mut png, 2, &image_data[..30]);
            write_fdat(&mut png, 3, &image_data[30..]);
            write_iend(&mut png);
            png
        };

        // Start decoding.
        let decoder = Decoder::new(png.as_slice());
        let mut reader = decoder.read_info().unwrap();
        let mut buf = vec![0; reader.output_buffer_size()];
        let Some(animation_control) = reader.info().animation_control else {
            panic!("No acTL");
        };
        assert_eq!(animation_control.num_frames, 2);

        // Process the 1st animation frame.
        let first_frame: Vec<u8>;
        {
            reader.next_frame(&mut buf).unwrap();
            first_frame = buf.clone();

            // Note that the doc comment of `Reader::next_frame` says that "[...]
            // can be checked afterwards by calling `info` **after** a successful call and
            // inspecting the `frame_control` data.".  (Note the **emphasis** on "after".)
            let Some(frame_control) = reader.info().frame_control else {
                panic!("No fcTL (1st frame)");
            };
            // The sequence number is taken from the `fcTL` chunk that comes before the `IDAT`
            // chunk.
            assert_eq!(frame_control.sequence_number, 0);
        }

        // Process the 2nd animation frame.
        let second_frame: Vec<u8>;
        {
            reader.next_frame(&mut buf).unwrap();
            second_frame = buf.clone();

            // Same as above - updated `frame_control` is available *after* the `next_frame` call.
            let Some(frame_control) = reader.info().frame_control else {
                panic!("No fcTL (2nd frame)");
            };
            // The sequence number is taken from the `fcTL` chunk that comes before the two `fdAT`
            // chunks.  Note that sequence numbers inside `fdAT` chunks are not publically exposed
            // (but they are still checked when decoding to verify that they are sequential).
            assert_eq!(frame_control.sequence_number, 1);
        }

        assert_eq!(first_frame, second_frame);
    }

    #[test]
    fn test_idat_bigger_than_image_size_from_ihdr() {
        let png = {
            let mut png = Vec::new();
            write_png_sig(&mut png);
            write_rgba8_ihdr_with_width(&mut png, 8);

            // Here we want to test an invalid image where the `IDAT` chunk contains more data
            // (data for 8x256 image) than declared in the `IHDR` chunk (which only describes an
            // 8x8 image).
            write_chunk(
                &mut png,
                b"IDAT",
                &generate_rgba8_with_width_and_height(8, 256),
            );

            write_iend(&mut png);
            png
        };
        let decoder = Decoder::new(png.as_slice());
        let mut reader = decoder.read_info().unwrap();
        let mut buf = vec![0; reader.output_buffer_size()];

        // TODO: Should this return an error instead?  For now let's just have test assertions for
        // the current behavior.
        reader.next_frame(&mut buf).unwrap();
        assert_eq!(3093270825, crc32fast::hash(&buf));
    }
}
