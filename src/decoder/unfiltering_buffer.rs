use super::stream::{DecodingError, FormatErrorInner};
use super::zlib::UnfilterBuf;
use crate::common::BytesPerPixel;
use crate::filter::{unfilter, RowFilter};

// Buffer for temporarily holding decompressed, not-yet-`unfilter`-ed rows.
pub(crate) struct UnfilteringBuffer {
    /// Vec containing the uncompressed image data currently being processed.
    data_stream: Vec<u8>,
    /// Index in `data_stream` where the previous row starts.
    /// This excludes the filter type byte - it points at the first byte of actual pixel data.
    /// The pixel data is already-`unfilter`-ed.
    ///
    /// If `prev_start == current_start` then it means that there is no previous row.
    prev_start: usize,
    /// Index in `data_stream` where the current row starts.
    /// This points at the filter type byte of the current row (i.e. the actual pixel data starts at `current_start + 1`)
    /// The pixel data is not-yet-`unfilter`-ed.
    ///
    /// `current_start` can wrap around the length.
    current_start: usize,
    /// Logical length of data that must be preserved.
    filled: usize,
    /// Length of data that can be modified.
    available: usize,
    /// The maximum number of bytes that we should allocate in the `data_stream`.
    allocation_limit: usize,
}

impl UnfilteringBuffer {
    /// Asserts in debug builds that all the invariants hold.  No-op in release
    /// builds.  Intended to be called after creating or mutating `self` to
    /// ensure that the final state preserves the invariants.
    fn debug_assert_invariants(&self) {
        debug_assert!(self.prev_start <= self.current_start);
        debug_assert!(self.current_start <= self.available);
        debug_assert!(self.available <= self.filled);
        debug_assert!(self.filled <= self.data_stream.len());
    }

    pub fn new(max_rowlen: usize) -> Self {
        let result = Self {
            data_stream: Vec::with_capacity(max_rowlen * 32),
            prev_start: 0,
            current_start: 0,
            filled: 0,
            available: 0,
            allocation_limit: max_rowlen * 32,
        };

        result.debug_assert_invariants();
        result
    }

    /// Called to indicate that there is no previous row (e.g. when the current
    /// row is the first scanline of a given Adam7 pass).
    pub fn reset_prev_row(&mut self) {
        self.prev_start = self.current_start;
        self.debug_assert_invariants();
    }

    pub fn reset_all(&mut self) {
        self.data_stream.clear();
        self.prev_start = 0;
        self.current_start = 0;
        self.filled = 0;
        self.available = 0;
    }

    /// Returns the previous (already `unfilter`-ed) row.
    pub fn prev_row(&self) -> &[u8] {
        &self.data_stream[self.prev_start..self.current_start]
    }

    /// Returns how many bytes of the current row are present in the buffer.
    pub fn curr_row_len(&self) -> usize {
        self.available - self.current_start
    }

    /// Returns a `&mut Vec<u8>` suitable for passing to
    /// `ReadDecoder.decode_image_data` or `StreamingDecoder.update`.
    ///
    /// Invariants of `self` depend on the assumption that the caller will only
    /// append new bytes to the returned vector (which is indeed the behavior of
    /// `ReadDecoder` and `StreamingDecoder`).  TODO: Consider protecting the
    /// invariants by returning an append-only view of the vector
    /// (`FnMut(&[u8])`??? or maybe `std::io::Write`???).
    pub fn as_unfilled_buffer(&mut self) -> UnfilterBuf<'_> {
        if self.prev_start >= 128 * 1024 {
            // We have to relocate the data to the start of the buffer.
            self.data_stream
                .copy_within(self.prev_start..self.filled, 0);

            // The data kept its relative position to `filled` which now lands exactly at
            // the distance between prev_start and filled.
            self.current_start -= self.prev_start;
            self.available -= self.prev_start;
            self.filled -= self.prev_start;
            self.prev_start = 0;
        }

        if self.filled + 8 * 1024 > self.data_stream.len() {
            self.data_stream.resize(self.filled + 8 * 1024, 0);
        }

        UnfilterBuf {
            buffer: &mut self.data_stream,
            filled: &mut self.filled,
            available: &mut self.available,
        }
    }

    /// Runs `unfilter` on the current row, and then shifts rows so that the current row becomes the previous row.
    ///
    /// Will panic if `self.curr_row_len() < rowlen`.
    pub fn unfilter_curr_row(
        &mut self,
        rowlen: usize,
        bpp: BytesPerPixel,
    ) -> Result<(), DecodingError> {
        debug_assert!(rowlen >= 2); // 1 byte for `FilterType` and at least 1 byte of pixel data.

        let (prev, row) = self.data_stream.split_at_mut(self.current_start);
        let prev: &[u8] = &prev[self.prev_start..];

        debug_assert!(prev.is_empty() || prev.len() == (rowlen - 1));

        // Get the filter type.
        let filter = RowFilter::from_u8(row[0]).ok_or(DecodingError::Format(
            FormatErrorInner::UnknownFilterMethod(row[0]).into(),
        ))?;
        let row = &mut row[1..rowlen];

        unfilter(filter, bpp, prev, row);

        self.prev_start = self.current_start + 1;
        self.current_start += rowlen;

        self.debug_assert_invariants();

        Ok(())
    }
}
