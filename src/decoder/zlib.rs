use super::{stream::FormatErrorInner, unfiltering_buffer::UnfilteringBuffer, DecodingError};

use fdeflate::Decompressor;

/// A buffer for decompression and in-place filtering of PNG rowlines.
///
/// The underlying data structure is a vector, with additional markers dividing
/// the vector into specific regions of bytes - see [`UnfilterRegion`] for more
/// details.
pub struct UnfilterBuf<'data> {
    /// The data container.
    pub(crate) buffer: &'data mut Vec<u8>,
    /// The past-the-end index of the region that is allowed to be modified.
    pub(crate) available: &'data mut usize,
    /// The past-the-end index of the region with decompressed bytes.
    pub(crate) filled: &'data mut usize,
}

/// `UnfilterRegion` divides a `Vec<u8>` buffer into three consecutive regions:
///
/// * `vector[0..available]` - bytes that may be mutated (this typically means
///   bytes that were decompressed earlier, but user of the buffer may also use
///   this region for storing other data)
/// * `vector[available..filled]` - already decompressed bytes that need to be
///   preserved. (Future decompressor calls may reference and copy bytes from
///   this region.  The maximum `filled - available` "look back" distance for
///   [PNG compression method 0](https://www.w3.org/TR/png-3/#10CompressionCM0)
///   is 32768 bytes)
/// * `vector[filled..]` - buffer where future decompressor calls can write
///   additional decompressed bytes
///
/// Even though only `vector[0..available]` bytes can be mutated, it is allowed
/// to "shift" or "move" the contents of vector, as long as the:
///
/// * `vector[available..filled]` bytes are preserved
/// * `available` and `filled` offsets are updated
///
/// Violating the invariants described above (e.g. mutating the bytes in the
/// `vector[available..filled]` region) may result in absurdly wacky
/// decompression output or panics, but not undefined behavior.
#[derive(Default, Clone, Copy)]
pub struct UnfilterRegion {
    /// The past-the-end index of the region that is allowed to be modified.
    pub available: usize,
    /// The past-the-end index of the region with decompressed bytes.
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

        let in_consumed = image_data.decompress(&mut self.state, Some(data))?;
        self.started = true;

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

        // First read without indicating an end. The caller can then react to the total amount of
        // available bytes in this stream, before we check for an _missing_ end of stream. This
        // ensures that the error signalled on a missing EOF is consistent no matter into which
        // slices the input file has been split when passed to the decoder.
        let mut eof_input = Some(&[][..]);
        let mut decompression_produced = 0;

        while !self.state.is_done() {
            let in_consumed = image_data.decompress(&mut self.state, eof_input)?;
            decompression_produced += in_consumed;

            if in_consumed == 0 {
                if decompression_produced > 0 {
                    return Ok(Flush::ProducedMoreData);
                };

                // There is no more data produced and we have no more input. We're done. As a final
                // step we offer all decompressed bytes to be unfiltered. Maybe it's incidentally
                // enough. If not, we will reach this point again with the condition being `false`.
                // We then let the decompressor fail with an appropriate error if it has not
                // reached its end of stream.
                if image_data.commit_all_and_check_for_more() {
                    return Ok(Flush::ProducedMoreData);
                }

                // Now indicate EOF input for the next read, causing an error if there was no EOF.
                eof_input = None;
            }

            if !self.state.is_done() {
                image_data.grow_buffer_if_needed_for_final_flushing();
            }
        }

        Ok(Flush::Complete)
    }
}

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
    ///
    /// May panic if invariants of [`UnfilterRegion`] are violated.
    pub fn as_buf<'data>(&'data mut self, buffer: &'data mut Vec<u8>) -> UnfilterBuf<'data> {
        assert!(self.available <= self.filled);
        assert!(self.filled <= buffer.len());
        UnfilterBuf {
            buffer,
            filled: &mut self.filled,
            available: &mut self.available,
        }
    }
}

impl UnfilterBuf<'_> {
    /// [PNG spec](https://www.w3.org/TR/2003/REC-PNG-20031110/#10Compression) says that
    /// "deflate/inflate compression with a sliding window (which is an upper bound on the
    /// distances appearing in the deflate stream) of at most 32768 bytes".
    ///
    /// `fdeflate` requires that we keep this many most recently decompressed bytes in the
    /// `out_buffer` - this allows referring back to them when handling "length and distance
    /// codes" in the deflate stream).
    const LOOKBACK_SIZE: usize = 32768;

    /// Pushes `input` into `fdeflate` crate and appends decompressed bytes to `self.buffer`
    /// (adjusting `self.filled` and `self.available` depending on how many bytes have been
    /// decompressed).
    ///
    /// Returns how many bytes of `input` have been consumed.
    #[inline]
    fn decompress(
        &mut self,
        decompressor: &mut fdeflate::Decompressor,
        input: Option<&[u8]>,
    ) -> Result<usize, DecodingError> {
        let (input, end_of_input) = match input {
            Some(input) => (input, false),
            None => ([].as_slice(), true),
        };

        let output_limit = (*self.filled + UnfilteringBuffer::GROWTH_BYTES).min(self.buffer.len());
        let (in_consumed, out_consumed) = decompressor
            .read(
                input,
                &mut self.buffer[*self.available..output_limit],
                *self.filled - *self.available,
                end_of_input,
            )
            .map_err(|err| {
                DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
            })?;

        *self.filled += out_consumed;

        if decompressor.is_done() {
            *self.available = *self.filled;
        } else if let Some(new_available) = self.filled.checked_sub(Self::LOOKBACK_SIZE) {
            // The decompressed data may have started in the middle of the buffer,
            // so ensure that `self.available` never goes backward.  This is needed
            // to avoid miscommunicating the size of the "look-back" window when calling
            // `fdeflate::Decompressor::read` a bit earlier and passing
            // `&mut self.buffer[*self.available..output_limit]`.
            if new_available > *self.available {
                *self.available = new_available;
            }
        }

        Ok(in_consumed)
    }

    /// Grows buffer if needed for final flushing of decompressor output.
    fn grow_buffer_if_needed_for_final_flushing(&mut self) {
        // Only grow the buffer if needed / if the buffer is full.  In particular,
        // calls from `ZlibStream::decompress` to `UnfilterBuf::decompress` will
        // output at most `GROWTH_BYTES` bytes, which may be insufficient to fill
        // all of the buffer grown by `SIZE_DELTA`.
        if *self.filled == self.buffer.len() {
            const SIZE_DELTA: usize = 32 * 1024;
            let len = self.buffer.len() + SIZE_DELTA;
            self.buffer.resize(len, 0);
        }
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
}
