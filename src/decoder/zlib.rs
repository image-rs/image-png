use super::{stream::FormatErrorInner, DecodingError};

use fdeflate::Decompressor;

pub struct UnfilterBuf<'data> {
    // The data to be fed into the decoder for (zlib) decompression.
    pub(crate) buffer: &'data mut Vec<u8>,
    // Where we record changes to the out position.
    pub(crate) filled: &'data mut usize,
    // Where we record changes to the available byte.
    pub(crate) available: &'data mut usize,
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
        let (in_consumed, out_consumed) =
            self.state
                .read(data, buffer, filled, false)
                .map_err(|err| {
                    DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
                })?;

        self.started = true;
        image_data.filled(filled + out_consumed);
        image_data.commit((filled + out_consumed).saturating_sub(Self::LOOKBACK_SIZE));

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
    ) -> Result<(), DecodingError> {
        if !self.started {
            return Ok(());
        }

        let (_, mut filled) = image_data.borrow_mut();
        while !self.state.is_done() {
            let (buffer, _) = image_data.borrow_mut();
            let (_in_consumed, out_consumed) =
                self.state.read(&[], buffer, filled, true).map_err(|err| {
                    DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
                })?;

            filled += out_consumed;

            if !self.state.is_done() {
                image_data.flush_allocate();
            }
        }

        image_data.filled(filled);
        image_data.commit(filled);

        Ok(())
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

    pub(crate) fn flush_allocate(&mut self) {
        let len = self.buffer.len() + 32 * 1024;
        self.buffer.resize(len, 0);
    }
}
