use fdeflate::Decompressor;

use super::stream::{DecodeOptions, Decoded, DecodingError, FormatErrorInner, StreamingDecoder};
use super::unfiltering_buffer::UnfilteringBuffer;
use super::Limits;

use std::io::{BufRead, ErrorKind, Read, Seek};

use crate::chunk;
use crate::common::Info;

struct DecompressState {
    /// The decompressor used for zlib streams.
    zlib: Box<fdeflate::Decompressor>,
    /// Number of bytes from `reader` that have been consumed by `decoder`, but not yet decompressed into the output.
    pending_idat_bytes: usize,
    /// Whether there is more data to read from the input stream.
    more_data: bool,
}

/// Helper for encapsulating reading input from `Read` and feeding it into a `StreamingDecoder`
/// while hiding low-level `Decoded` events and only exposing a few high-level reading operations
/// like:
///
/// * `read_header_info` - reading until `IHDR` chunk
/// * `read_until_image_data` - reading until `IDAT` / `fdAT` sequence
/// * `decode_image_data` - reading from `IDAT` / `fdAT` sequence into `Vec<u8>`
/// * `finish_decoding_image_data()` - discarding remaining data from `IDAT` / `fdAT` sequence
/// * `read_until_end_of_input()` - reading until `IEND` chunk
pub(crate) struct ReadDecoder<R: Read> {
    reader: R,
    decoder: StreamingDecoder,
    decompress: Option<DecompressState>,
    ignore_adler32: bool,
}

impl<R: BufRead + Seek> ReadDecoder<R> {
    pub fn new(r: R) -> Self {
        Self {
            reader: r,
            decoder: StreamingDecoder::new(),
            decompress: None,
            ignore_adler32: true,
        }
    }

    pub fn with_options(r: R, options: DecodeOptions) -> Self {
        let ignore_adler32 = options.ignore_adler32;
        let mut decoder = StreamingDecoder::new_with_options(options);
        decoder.limits = Limits::default();

        Self {
            reader: r,
            decoder,
            decompress: None,
            ignore_adler32,
        }
    }

    pub fn set_limits(&mut self, limits: Limits) {
        self.decoder.limits = limits;
    }

    pub fn reserve_bytes(&mut self, bytes: usize) -> Result<(), DecodingError> {
        self.decoder.limits.reserve_bytes(bytes)
    }

    pub fn set_ignore_text_chunk(&mut self, ignore_text_chunk: bool) {
        self.decoder.set_ignore_text_chunk(ignore_text_chunk);
    }

    pub fn set_ignore_iccp_chunk(&mut self, ignore_iccp_chunk: bool) {
        self.decoder.set_ignore_iccp_chunk(ignore_iccp_chunk);
    }

    pub fn ignore_checksums(&mut self, ignore_checksums: bool) {
        self.ignore_adler32 = ignore_checksums;
        self.decoder.set_ignore_crc(ignore_checksums);
    }

    fn decode_next(&mut self) -> Result<Decoded, DecodingError> {
        let (consumed, result) = {
            let buf = self.reader.fill_buf()?;
            if buf.is_empty() {
                return Err(DecodingError::IoError(ErrorKind::UnexpectedEof.into()));
            }
            self.decoder.update(buf)?
        };
        self.reader.consume(consumed);
        Ok(result)
    }

    /// Reads until the end of `IHDR` chunk.
    ///
    /// Prerequisite: None (idempotent).
    pub fn read_header_info(&mut self) -> Result<&Info<'static>, DecodingError> {
        while self.info().is_none() {
            if let Decoded::ImageEnd = self.decode_next()? {
                unreachable!()
            }
        }
        Ok(self.info().unwrap())
    }

    /// Reads until the start of the next `IDAT` or `fdAT` chunk.
    ///
    /// Prerequisite: **Not** within `IDAT` / `fdAT` chunk sequence.
    pub fn read_until_image_data(&mut self) -> Result<(), DecodingError> {
        loop {
            match self.decode_next()? {
                Decoded::ChunkBegin(_, chunk::IDAT) | Decoded::ChunkBegin(_, chunk::fdAT) => break,
                Decoded::ImageEnd => {
                    return Err(DecodingError::Format(
                        FormatErrorInner::MissingImageData.into(),
                    ))
                }
                // Ignore all other chunk events. Any other chunk may be between IDAT chunks, fdAT
                // chunks and their control chunks.
                _ => {}
            }
        }
        Ok(())
    }

    /// Reads `image_data` and reports whether there may be additional data afterwards (i.e. if it
    /// is okay to call `decode_image_data` and/or `finish_decoding_image_data` again)..
    ///
    /// Prerequisite: Input is currently positioned within `IDAT` / `fdAT` chunk sequence.
    pub fn decode_image_data(
        &mut self,
        image_data: &mut UnfilteringBuffer,
    ) -> Result<ImageDataCompletionStatus, DecodingError> {
        if self.decompress.is_none() {
            let mut zlib = Box::new(Decompressor::new());
            if self.ignore_adler32 {
                zlib.ignore_adler32();
            }
            self.decompress = Some(DecompressState {
                zlib,
                pending_idat_bytes: 0,
                more_data: true,
            });
        }
        let decompress = self.decompress.as_mut().unwrap();

        if decompress.more_data && decompress.pending_idat_bytes == 0 {
            loop {
                let buf = self.reader.fill_buf()?;
                if buf.is_empty() {
                    return Err(DecodingError::IoError(ErrorKind::UnexpectedEof.into()));
                }

                let (consumed, decoded) = self.decoder.update(buf)?;
                match decoded {
                    Decoded::ImageData => {
                        decompress.pending_idat_bytes = consumed;
                        break;
                    }
                    Decoded::ImageEnd | Decoded::ImageDataFlushed => {
                        decompress.more_data = false;
                        self.reader.consume(consumed);
                        break;
                    }
                    // Ignore other events that may happen within an `IDAT` / `fdAT` chunks sequence.
                    Decoded::Nothing
                    | Decoded::ChunkComplete(_, _)
                    | Decoded::ChunkBegin(_, _)
                    | Decoded::PartialChunk(_) => {
                        //return Ok(ImageDataCompletionStatus::ExpectingMoreData)
                        self.reader.consume(consumed);
                        continue;
                    }
                    // Other kinds of events shouldn't happen, unless we have been (incorrectly) called
                    // when outside of a sequence of `IDAT` / `fdAT` chunks.
                    unexpected => unreachable!("{:?}", unexpected),
                }
            }
        }

        let (output, old_pos) = image_data.uncompress_buffer();

        // if decompress.pending_idat_bytes > 0 {
        let buf = self.reader.fill_buf()?;
        let (in_consumed, out_consumed) = decompress
            .zlib
            .read(
                &buf[..decompress.pending_idat_bytes],
                output,
                old_pos,
                !decompress.more_data,
            )
            .map_err(|err| {
                DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
            })?;

        self.reader.consume(in_consumed);
        decompress.pending_idat_bytes -= in_consumed;
        image_data.marked_valid(out_consumed);
        // }

        if decompress.more_data || decompress.pending_idat_bytes > 0 {
            Ok(ImageDataCompletionStatus::ExpectingMoreData)
        } else {
            self.decompress = None;
            Ok(ImageDataCompletionStatus::Done)
        }
    }

    /// Consumes and discards the rest of an `IDAT` / `fdAT` chunk sequence.
    ///
    /// Prerequisite: Input is currently positioned within `IDAT` / `fdAT` chunk sequence.
    pub fn finish_decoding_image_data(&mut self) -> Result<(), DecodingError> {
        self.decompress = None;
        while !matches!(
            self.decode_next()?,
            Decoded::ImageDataFlushed | Decoded::ImageEnd
        ) {}
        Ok(())
    }

    /// Reads until the `IEND` chunk.
    ///
    /// Prerequisite: `IEND` chunk hasn't been reached yet.
    pub fn read_until_end_of_input(&mut self) -> Result<(), DecodingError> {
        while !matches!(self.decode_next()?, Decoded::ImageEnd) {}
        Ok(())
    }

    pub fn info(&self) -> Option<&Info<'static>> {
        self.decoder.info.as_ref()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ImageDataCompletionStatus {
    ExpectingMoreData,
    Done,
}
