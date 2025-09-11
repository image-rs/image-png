use super::stream::{DecodingError, FormatErrorInner};
use super::zlib::UnfilterBuf;
use crate::common::BytesPerPixel;
use crate::filter::{unfilter, RowFilter};
use crate::Info;

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
    /// The number of bytes before we shift the buffer back.
    shift_back_limit: usize,
}

impl UnfilteringBuffer {
    pub const GROWTH_BYTES: usize = 8 * 1024;

    /// Asserts in debug builds that all the invariants hold.  No-op in release
    /// builds.  Intended to be called after creating or mutating `self` to
    /// ensure that the final state preserves the invariants.
    #[track_caller]
    fn debug_assert_invariants(&self) {
        // The previous row pointer is always behind the current row pointer.
        debug_assert!(self.prev_start <= self.current_start);
        // The current row pointer is always into filled bytes (but sometimes may point to
        // read-only back reference buffer, see `curr_row_len_with_unfilter_ahead`).
        debug_assert!(self.current_start <= self.filled);
        // The mut-available region is always well-defined.
        debug_assert!(self.available <= self.filled);
        // The logically filled region is always within bounds of the data stream.
        debug_assert!(self.filled <= self.data_stream.len());
    }

    /// Create a buffer tuned for filtering rows of the image type.
    pub fn new(info: &Info<'_>) -> Self {
        // We don't need all of `info` here so if that becomes a structural problem then these
        // derived constants can be extracted into a parameter struct. For instance they may be
        // adjusted according to platform hardware such as cache sizes.
        let data_stream_capacity = {
            let max_data = info
                .checked_raw_row_length()
                // In the current state this is really dependent on IDAT sizes and the compression
                // settings. We aim to avoid overallocation here, but that occurs in part due to
                // the algorithm for draining the buffer, which at the time of writing is at each
                // individual IDAT chunk boundary. So this is set for a quadratic image roughly
                // fitting into a single 4k chunk at compression.. A very arbitrary choice made
                // from (probably overfitting) a benchmark of that image size. With a different
                // algorithm we may come to different buffer uses and have to re-evaluate.
                .and_then(|v| v.checked_mul(info.height.min(128) as usize))
                // In the worst case this is additional room for use of unmasked SIMD moves. But
                // the other idea here is that the allocator generally aligns the buffer.
                .and_then(|v| checked_next_multiple_of(v, 256))
                .unwrap_or(usize::MAX);
            // We do not want to pre-allocate too much in case of a faulty image (no DOS by
            // pretending to be very very large) and also we want to avoid allocating more data
            // than we need for the image itself.
            max_data.min(128 * 1024)
        };

        let shift_back_limit = {
            // Prefer shifting by powers of two and only after having done some number of
            // lines that then become free at the end of the buffer.
            let rowlen_pot = info
                .checked_raw_row_length()
                // Ensure some number of rows are actually present before shifting back, i.e. next
                // time around we want to be able to decode them without reallocating the buffer.
                .and_then(|v| v.checked_mul(4))
                // And also, we should be able to use aligned memcopy on the whole thing. Well at
                // least that is the idea but the parameter is just benchmarking. Higher numbers
                // did not result in performance gains but lowers also, so this is fickle. Maybe
                // our shift back behavior can not be tuned very well.
                .and_then(|v| checked_next_multiple_of(v, 64))
                .unwrap_or(isize::MAX as usize);
            // But never shift back before we have a number of pages freed.
            rowlen_pot.max(128 * 1024)
        };

        let result = Self {
            data_stream: Vec::with_capacity(data_stream_capacity),
            prev_start: 0,
            current_start: 0,
            filled: 0,
            available: 0,
            shift_back_limit,
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
        self.debug_assert_invariants();
    }

    /// Returns the previous (already `unfilter`-ed) row.
    pub fn prev_row(&self) -> &[u8] {
        &self.data_stream[self.prev_start..self.current_start]
    }

    /// Returns how many bytes of the current row are present in the buffer.
    pub fn curr_row_len(&self) -> usize {
        self.available - self.current_start
    }

    /// Returns how many bytes of the current row are available in the buffer in the mutable region.
    ///
    /// A return value of `0` indicates that no mutable row is available. You can still try to read
    /// ahead.
    ///
    /// Background: There's a backreference section in the buffer that is reserved for the zlib /
    /// deflate decoder (or any other for that matter). We must not modify that part of the data
    /// but if we only *read* the row then the start of our current_start is allowed to run into
    /// that section. So as a less efficient fallback where partial data is crucial we can unfilter
    /// outside the read area instead of in-place but still indicate how many bytes we have
    /// consumed. This lets you check if the fallback path is needed.
    pub(crate) fn mutable_rowdata_length(&self) -> usize {
        self.available.saturating_sub(self.current_start)
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
        let shift_back = self.prev_start.min(self.available);

        if shift_back >= self.shift_back_limit
            // Avoid the shift back if the buffer is still very empty. Consider how we got here: a
            // previous decompression filled the buffer, then we unfiltered, we're now refilling
            // the buffer again. The condition implies, the previous decompression filled at most
            // half the buffer. Likely the same will happen again so the following decompression
            // attempt will not yet be limited by the buffer length.
            && self.filled >= self.data_stream.len() / 2
        {
            // We have to relocate the data to the start of the buffer. Benchmarking suggests that
            // the codegen for an unbounded range is better / different than the one for a bounded
            // range. We prefer the former if the data overhead is not too high. `16` was
            // determined experimentally and might be system (memory) dependent. There's also the
            // question if we could be a little smarter and avoid crossing page boundaries when
            // that is not required. Alas, microbenchmarking TBD.
            if let Some(16..) = self.data_stream.len().checked_sub(self.filled) {
                self.data_stream.copy_within(shift_back..self.filled, 0);
            } else {
                self.data_stream.copy_within(shift_back.., 0);
            }

            self.debug_assert_invariants();

            // The data kept its relative position to `filled` which now lands exactly at
            // the distance between prev_start and filled.
            self.current_start -= shift_back;
            self.available -= shift_back;
            self.filled -= shift_back;
            self.prev_start -= shift_back;

            self.debug_assert_invariants();
        }

        if self.filled + Self::GROWTH_BYTES > self.data_stream.len() {
            self.data_stream.resize(self.filled + Self::GROWTH_BYTES, 0);
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
    ///
    /// For correctness this also assumes that `curr_row_len_with_unfilter_ahead` is greater than
    /// `rowlen`.
    pub fn unfilter_curr_row(
        &mut self,
        rowlen: usize,
        bpp: BytesPerPixel,
    ) -> Result<(), DecodingError> {
        debug_assert!(rowlen >= 2); // 1 byte for `FilterType` and at least 1 byte of pixel data.
        debug_assert_eq!(rowlen as isize as usize, rowlen);
        debug_assert!(self.mutable_rowdata_length() >= rowlen);

        let (prev, row) = self.data_stream.split_at_mut(self.current_start);
        let prev: &[u8] = &prev[self.prev_start..];

        let prev = if prev.is_empty() {
            prev
        } else {
            &prev[..rowlen - 1]
        };

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

    /// Unfilter but allow the current_start to exceed `available` at the cost of some compute.
    /// This will be called when we encounter an `UnexpectedEof` and want to push out all
    /// interlaced rows that we can, i.e. it is not in the usual critical path of decoding.
    pub(crate) fn unfilter_ahead_row(
        &mut self,
        rowlen: usize,
        bpp: BytesPerPixel,
    ) -> Result<bool, DecodingError> {
        if self.filled - self.current_start < rowlen {
            return Ok(false);
        }

        // We can not really mutate the row data to unfilter it! So where do we put the unfiltered
        // row then? In our interface it should occur at `self.prev_start` after this method is
        // finished. So really simple, we just copy it back there.
        //
        // There is of course subtlety. First we need to make sure that the buffer of the previous
        // row does not overlap with the data for the decoder as we will overwrite it. That is
        // usually trivial, when it was previously unfiltered we had already mutated it so just
        // reuse, except that at the first line of each interlace pass we start with an _empty_
        // previous row and consequently need to potentially move all our data further back.

        // The fixup discussed. Make space for a previous row. It does not matter where we put it
        // so just put it right before the minimum of `current_start` and `available`. In this case
        // however we should also pass an empty row to `unfilter`.
        if self.prev_start == self.current_start {
            let potential_end_of_new_prev = self.current_start.min(self.available);
            // Insert free space between what we treat as the previous row and the current row.
            // NOTE: this is because of the decoder's `commit` function which will take a fixed
            // window of data back from its filled state before it is done. But we move that region
            // upwards so it may erroneously point backwards by more than necessary. That just
            // freezes data but it might also freeze a portion that we are using as the unfilter
            // buffer / the space to put the 'previous row'.
            let padding = rowlen - 1;

            let start_of_new_prev = potential_end_of_new_prev.saturating_sub(padding);
            let end_of_new_prev = start_of_new_prev + padding;

            // Shift everything up as required.
            let current_shift = end_of_new_prev - potential_end_of_new_prev;

            self.data_stream.splice(
                start_of_new_prev..start_of_new_prev,
                core::iter::repeat(0u8).take(current_shift),
            );

            self.current_start += current_shift;
            // Temporary in case of error.
            self.prev_start = self.current_start;
            self.available += current_shift;
            self.filled += current_shift;

            self.debug_assert_invariants();

            let (prev, row) = self.data_stream.split_at_mut(self.current_start);
            let prev = &mut prev[start_of_new_prev..][..rowlen - 1];

            let filter = RowFilter::from_u8(row[0]).ok_or(DecodingError::Format(
                FormatErrorInner::UnknownFilterMethod(row[0]).into(),
            ))?;

            prev.copy_from_slice(&row[1..rowlen]);
            unfilter(filter, bpp, &[], prev);

            self.prev_start = start_of_new_prev;
            self.current_start += rowlen;
            self.debug_assert_invariants();

            Ok(true)
        } else {
            let (prev, row) = self.data_stream.split_at_mut(self.current_start);

            assert!(
                self.available - self.prev_start >= rowlen - 1,
                "prev {prev}, cur {cur}, avail {avail}, fill {fill}, rowlen {rowlen}",
                prev = self.prev_start,
                cur = self.current_start,
                avail = self.available,
                fill = self.filled,
            );

            let prev = &mut prev[self.prev_start..][..rowlen - 1];

            let filter = RowFilter::from_u8(row[0]).ok_or(DecodingError::Format(
                FormatErrorInner::UnknownFilterMethod(row[0]).into(),
            ))?;

            // Unfilter this in a temporary buffer.
            let mut row = row[1..rowlen].to_vec();
            unfilter(filter, bpp, prev, &mut row);
            prev.copy_from_slice(&row);

            // Do NOT modify prev_start
            self.current_start += rowlen;
            self.debug_assert_invariants();

            Ok(true)
        }
    }
}

fn checked_next_multiple_of(val: usize, factor: usize) -> Option<usize> {
    if factor == 0 {
        return None;
    }

    let remainder = val % factor;

    if remainder > 0 {
        val.checked_add(factor - remainder)
    } else {
        Some(val)
    }
}

#[test]
fn next_multiple_of_backport_testsuite() {
    assert_eq!(checked_next_multiple_of(1, 0), None);
    assert_eq!(checked_next_multiple_of(2, 0), None);
    assert_eq!(checked_next_multiple_of(1, 2), Some(2));
    assert_eq!(checked_next_multiple_of(2, 2), Some(2));
    assert_eq!(checked_next_multiple_of(2, 5), Some(5));
    assert_eq!(checked_next_multiple_of(1, usize::MAX), Some(usize::MAX));
    assert_eq!(checked_next_multiple_of(usize::MAX, 2), None);
}

/// Checks that intermittent `unfilter_ahead_row` is equivalent to doing unfilters on the complete
/// data without any read-ahead.
#[test]
fn independent_of_readahead() {
    // Simulate a decompression as if by `UnfilterBuf::decompress`.
    fn decompress_from_constant(buf: UnfilterBuf<'_>, data: &mut &[u8]) {
        let target_region = *buf.filled..;
        let target = &mut buf.buffer[target_region];

        let to_copy = target.len().min(data.len());
        target[..to_copy].copy_from_slice(&data[..to_copy]);

        *buf.filled += to_copy;
        let available = (*buf.filled).saturating_sub(UnfilterBuf::LOOKBACK_SIZE);
        *buf.available = available.max(*buf.available);
    }

    let mut line = vec![];
    for _ in 0..1024 {
        line.push(crate::filter::RowFilter::Sub as u8);
        line.extend_from_slice(&core::array::from_fn::<u8, 16, _>(|i| i as u8));
    }

    // We will unfilter in two different ways and compare results. Once where we do call read-ahead
    // and once where we do not.

    let mut data = line.as_slice();
    let rows_with_no_readahead = {
        let mut buf = UnfilteringBuffer::new(&Info::with_size(1024, 16));
        let mut rows = vec![];

        while rows.len() < 1024 {
            let xbuf = buf.as_unfilled_buffer();

            decompress_from_constant(xbuf, &mut data);
            if data.is_empty() {
                buf.available = buf.filled;
            }

            while buf.mutable_rowdata_length() >= 17 && rows.len() < 1024 {
                buf.unfilter_curr_row(17, BytesPerPixel::One).unwrap();
                rows.push(buf.prev_row().to_vec());
            }
        }

        rows
    };

    let rows_with_readahead = {
        let mut buf = UnfilteringBuffer::new(&Info::with_size(1024, 16));
        let mut rows = vec![];

        while rows.len() < 1024 {
            let xbuf = buf.as_unfilled_buffer();

            decompress_from_constant(xbuf, &mut data);
            if data.is_empty() {
                buf.available = buf.filled;
            }

            while buf.mutable_rowdata_length() >= 17 && rows.len() < 1024 {
                buf.unfilter_curr_row(17, BytesPerPixel::One).unwrap();
                rows.push(buf.prev_row().to_vec());
            }

            if rows.len() < 1024 && buf.unfilter_ahead_row(17, BytesPerPixel::One).unwrap() {
                rows.push(buf.prev_row().to_vec());
            }
        }

        rows
    };

    assert_eq!(rows_with_no_readahead.len(), rows_with_readahead.len());

    for (idx, (a, b)) in rows_with_no_readahead
        .iter()
        .zip(rows_with_readahead.iter())
        .enumerate()
    {
        assert_eq!(a, b, "row {idx} differs");
    }
}
