use super::{stream::FormatErrorInner, DecodingError};

use fdeflate::Decompressor;

const OUT_BUFFER_CHUNK_SIZE: usize = 8 * 1024;

/// Ergonomics wrapper around `miniz_oxide::inflate::stream` for zlib compressed data.
pub(super) struct ZlibStream {
    /// Current decoding state.
    state: Box<fdeflate::Decompressor>,
    /// If there has been a call to decompress already.
    started: bool,
    /// Limit on how many bytes can be decompressed in total.  This field is mostly used for
    /// performance optimizations (e.g. to avoid allocating and zeroing out large buffers when only
    /// a small image is being decoded).
    max_total_output: usize,
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
            // out_buffer: Vec::new(),
            // out_pos: 0,
            // read_pos: 0,
            max_total_output: usize::MAX,
            ignore_adler32: true,
        }
    }

    pub(crate) fn reset(&mut self) {
        self.started = false;
        *self.state = Decompressor::new();
    }

    pub(crate) fn set_max_total_output(&mut self, n: usize) {
        self.max_total_output = n;
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
        image_data: &mut Vec<u8>,
    ) -> Result<usize, DecodingError> {
        // There may be more data past the adler32 checksum at the end of the deflate stream. We
        // match libpng's default behavior and ignore any trailing data. In the future we may want
        // to add a flag to control this behavior.
        if self.state.is_done() {
            return Ok(data.len());
        }

        // self.prepare_vec_for_appending();

        if !self.started && self.ignore_adler32 {
            self.state.ignore_adler32();
        }

        let old_len = image_data.len();
        image_data.resize(
            (old_len + OUT_BUFFER_CHUNK_SIZE).min(self.max_total_output),
            0,
        );

        let (in_consumed, out_consumed) = self
            .state
            .read(data, image_data, old_len, false)
            .map_err(|err| {
                DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
            })?;

        self.started = true;

        image_data.resize(old_len + out_consumed, 0);

        // self.out_pos += out_consumed;
        // self.transfer_finished_data(image_data);
        // self.compact_out_buffer_if_needed();

        Ok(in_consumed)
    }

    /// Called after all consecutive IDAT chunks were handled.
    ///
    /// The compressed stream can be split on arbitrary byte boundaries. This enables some cleanup
    /// within the decompressor and flushing additional data which may have been kept back in case
    /// more data were passed to it.
    pub(crate) fn finish_compressed_chunks(
        &mut self,
        image_data: &mut Vec<u8>,
    ) -> Result<(), DecodingError> {
        if !self.started {
            return Ok(());
        }

        let old_len = image_data.len();
        image_data.resize(
            (old_len + OUT_BUFFER_CHUNK_SIZE).min(self.max_total_output),
            0,
        );

        let (_in_consumed, out_consumed) =
            self.state
                .read(&[], image_data, old_len, true)
                .map_err(|err| {
                    DecodingError::Format(FormatErrorInner::CorruptFlateStream { err }.into())
                })?;

        image_data.resize(old_len + out_consumed, 0);
        Ok(())
    }
}
