use super::{stream::FormatErrorInner, unfiltering_buffer::UnfilteringBuffer, DecodingError};

use fdeflate::Decompressor;

/// An inplace buffer for decompression and filtering of PNG rowlines.
///
/// The underlying data structure is a vector, with additional markers denoting a region of bytes
/// that are utilized by the decompression but not yet available to arbitrary modifications. The
/// caller can still shift around data between calls to the stream decompressor as long as the data
/// in the marked region is not modified and the indices adjusted accordingly. See
/// [`UnfilterRegion`] that contains these markers.
///
/// Violating the invariants, i.e. modifying bytes in the marked region, results in absurdly wacky
/// decompression output or panics but not undefined behavior.
pub struct UnfilterBuf<'data> {
    /// The data container. Starts with arbitrary data unrelated to the decoder, a slice of decoder
    /// private data followed by free space for further decoder output. The regions are delimited
    /// by `filled` and `available` which must be updated accordingly.
    pub(crate) buffer: &'data mut Vec<u8>,
    /// Where we record changes to the out position.
    pub(crate) filled: &'data mut usize,
    /// Where we record changes to the available byte.
    pub(crate) available: &'data mut usize,
}

/// A region into a buffer utilized as a [`UnfilterBuf`].
///
/// The span of data denoted by `filled..available` is the region of bytes that must be preserved
/// for use by the decompression algorithm. It may be moved, e.g. by subtracting the same amount
/// from both of these fields. Always ensure that `filled <= available`, the library does not
/// violate this invariant when modifying this struct as an [`UnfilterBuf`].
#[derive(Default, Clone, Copy)]
pub struct UnfilterRegion {
    /// The past-the-end index of byte that are allowed to be modified.
    pub available: usize,
    /// The past-the-end of bytes that have been written to.
    pub filled: usize,
}

/// Ergonomics wrapper around `miniz_oxide::inflate::stream` for zlib compressed data.
pub(super) struct ZlibStream {
    /// Current decoding state.
    state: Box<fdeflate::Decompressor>,
    /// If there has been a call to decompress already.
    started: bool,
    /// Ignore and do not calculate the Adler-32 checksum. Defaults to `true`.
    ///
    /// This flag overrides `TINFL_FLAG_COMPUTE_ADLER32`.
    ///
    /// This flag should not be modified after decompression has started.
    ignore_adler32: bool,
}

impl ZlibStream {
    // [PNG spec](https://www.w3.org/TR/2003/REC-PNG-20031110/#10Compression) says that
    // "deflate/inflate compression with a sliding window (which is an upper bound on the
    // distances appearing in the deflate stream) of at most 32768 bytes".
    //
    // `fdeflate` requires that we keep this many most recently decompressed bytes in the
    // `out_buffer` - this allows referring back to them when handling "length and distance
    // codes" in the deflate stream).
    const LOOKBACK_SIZE: usize = 32768;

    pub(crate) fn new() -> Self {
        ZlibStream {
            state: Box::new(Decompressor::new()),
            started: false,
            ignore_adler32: true,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.started = false;
        *self.state = Decompressor::new();
    }

    /// Set the `ignore_adler32` flag and return `true` if the flag was
    /// successfully set.
    ///
    /// The default is `true`.
    ///
    /// This flag cannot be modified after decompression has started until the
    /// [ZlibStream] is reset.
    pub(crate) fn set_ignore_adler32(&mut self, flag: bool) -> bool {
        if !self.started {
            self.ignore_adler32 = flag;
            true
        } else {
            false
        }
    }

    /// Return the `ignore_adler32` flag.
    pub(crate) fn ignore_adler32(&self) -> bool {
        self.ignore_adler32
    }

    /// Fill the decoded buffer as far as possible from `data`.
    /// On success returns the number of consumed input bytes.
    pub(crate) fn decompress(
        &mut self,
        data: &[u8],
        image_data: &mut UnfilterBuf<'_>,
    ) -> Result<usize, DecodingError> {
        // There may be more data past the adler32 checksum at the end of the deflate stream. We
        // match libpng's default behavior and ignore any trailing data. In the future we may want
        // to add a flag to control this behavior.
        if self.state.is_done() {
            return Ok(data.len());
        }

        if !self.started && self.ignore_adler32 {
            self.state.ignore_adler32();
        }

        let (buffer, filled) = image_data.borrow_mut();
        let output_limit = (filled + UnfilteringBuffer::GROWTH_BYTES).min(buffer.len());
        let (in_consumed, out_consumed) = self
            .state
            .read(data, &mut buffer[..output_limit], filled, false)
            .map_err(|err| {
                DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
            })?;

        self.started = true;
        let filled = filled + out_consumed;
        image_data.filled(filled);

        if self.state.is_done() {
            image_data.commit(filled);
        } else {
            // See [`Self::LOOKBACK_SIZE`].
            image_data.commit(filled.saturating_sub(Self::LOOKBACK_SIZE));
        }

        Ok(in_consumed)
    }

    /// Called after all consecutive IDAT chunks were handled.
    ///
    /// The compressed stream can be split on arbitrary byte boundaries. This enables some cleanup
    /// within the decompressor and flushing additional data which may have been kept back in case
    /// more data were passed to it.
    pub(crate) fn finish_compressed_chunks(
        &mut self,
        image_data: &mut UnfilterBuf<'_>,
    ) -> Result<Flush, DecodingError> {
        if !self.started {
            return Ok(Flush::Complete);
        }

        if self.state.is_done() {
            // We can end up here only after the [`decompress`] call above has detected the state
            // to be done, too. In this case the filled and committed amount of data are already
            // equal to each other. So neither of them needs to be touched in any way.
            return Ok(Flush::Complete);
        }

        let (_, filled) = image_data.borrow_mut();
        let mut provided = 0;

        // First read without indicating an end. The caller can then react to the total amount of
        // available bytes in this stream, before we check for an _missing_ end of stream. This
        // ensures that the error signalled on a missing EOF is consistent no matter into which
        // slices the input file has been split when passed to the decoder.
        while !self.state.is_done() {
            let (buffer, _) = image_data.borrow_mut();

            let (_in_consumed, out_consumed) =
                self.state.read(&[], buffer, filled, false).map_err(|err| {
                    DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
                })?;

            if out_consumed == 0 {
                break;
            }

            provided += out_consumed;

            if !self.state.is_done() {
                image_data.flush_allocate();
            }
        }

        if provided > 0 {
            let filled = filled + provided;
            image_data.filled(filled);
            image_data.commit(filled);
            return Ok(Flush::ProducedMoreData);
        }

        if image_data.commit_all_and_check_for_more() {
            return Ok(Flush::ProducedMoreData);
        }

        let (buffer, filled) = image_data.borrow_mut();
        let (_in_consumed, out_consumed) =
            self.state.read(&[], buffer, filled, true).map_err(|err| {
                DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
            })?;

        // We mustn't produce any more data after this point
        debug_assert_eq!(out_consumed, 0);

        image_data.filled(filled);
        image_data.commit(filled);

        Ok(Flush::Complete)
    }
}

#[derive(Debug)]
pub(crate) enum Flush {
    ProducedMoreData,
    Complete,
}

impl UnfilterRegion {
    /// Use this region to decompress new filtered rowline data.
    ///
    /// Pass the wrapped buffer to
    /// [`StreamingDecoder::update`][`super::stream::StreamingDecoder::update`] to fill it with
    /// data and update the region indices.
    pub fn as_buf<'data>(&'data mut self, buffer: &'data mut Vec<u8>) -> UnfilterBuf<'data> {
        UnfilterBuf {
            buffer,
            filled: &mut self.filled,
            available: &mut self.available,
        }
    }
}

impl UnfilterBuf<'_> {
    pub(crate) fn borrow_mut(&mut self) -> (&mut [u8], usize) {
        (self.buffer, *self.filled)
    }

    pub(crate) fn filled(&mut self, filled: usize) {
        *self.filled = filled;
    }

    pub(crate) fn commit(&mut self, howmany: usize) {
        *self.available = howmany;
    }

    /// Effectively close the stream, making all bytes available.
    ///
    /// Returns if that made any more bytes available.
    pub(crate) fn commit_all_and_check_for_more(&mut self) -> bool {
        let available = *self.available;
        let filled = *self.filled;

        *self.available = filled;
        available != filled
    }

    pub(crate) fn flush_allocate(&mut self) {
        let len = self.buffer.len() + 32 * 1024;
        self.buffer.resize(len, 0);
    }
}
