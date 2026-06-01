use std::io::{BufRead, BufReader, Read, Take};

/// Buffered input for the PNG decoder, with optional boundary-limited refills.
///
/// This trait is similar to [`BufRead`], but `fill_buf_limited` receives the maximum number of
/// bytes the decoder currently expects to consume before reaching the next PNG parser boundary.
///
/// To avoid reading beyond PNG boundaries, see [`LimitBufReader`] for an implementation that
/// honors the provided `limit`.
pub trait LimitBufRead {
    /// Same as [`BufRead::fill_buf`], but with a hint to cap reads to at most `limit` bytes.
    ///
    /// The `limit` boundary serves as a hint to avoid overshooting past `IEND`. Implementors
    /// are free to discard this value: [`BufRead`] implements this trait discarding `limit`
    /// entirely.
    ///
    /// Implementations that can enforce the `limit` should use it to cap the number of reads, i.e.
    /// it is valid to return a slice bigger than `limit` if there are more than `limit` unconsumed
    /// bytes.
    fn fill_buf_limited(&mut self, limit: usize) -> std::io::Result<&[u8]>;

    /// Same as [`BufRead::consume`].
    fn consume(&mut self, amt: usize);
}

/// Blanket implementation for all [`BufRead`] types.
///
/// [`LimitBufRead`] replaces the [`BufRead`] trait bound historically required by
/// [`Decoder`](crate::Decoder) and [`Reader`](crate::Reader) in a backwards-compatible manner.
///
/// This implementation deliberately ignores the `limit` dynamic boundary contract, aggressively
/// pre-fetching data into standard memory buffers to maximize standard I/O caching performance.
/// However, it does not prevent the underlying reader from being advanced past PNG boundaries.
impl<T: BufRead> LimitBufRead for T {
    fn fill_buf_limited(&mut self, _limit: usize) -> std::io::Result<&[u8]> {
        self.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        BufRead::consume(self, amt);
    }
}

/// A wrapper for [`Read`] streams that implements controlled buffering to prevent reading past
/// `IEND` chunk boundaries.
///
/// # Performance note
///
/// Because this wrapper caps reads strictly to what the state machine expects, it can trigger more
/// frequent operating system read syscalls in files characterized by many small chunks. This can
/// cause context-switching regressions compared to standard large-block pre-fetching (e.g.
/// `std::io::BufReader`). [`LimitBufReader`] should only be used when boundary safety is required.
pub struct LimitBufReader<R>(BufReader<Take<R>>);

impl<R: Read> LimitBufReader<R> {
    /// Creates a new [`LimitBufReader`] wrapping the given [`Read`] stream with the default
    /// [`BufReader`] capacity.
    pub fn new(inner: R) -> Self {
        Self(BufReader::new(inner.take(u64::MAX)))
    }

    /// Creates a new [`LimitBufReader`] wrapping the given [`Read`] stream with a specified
    /// `capacity` limit.
    pub fn with_capacity(capacity: usize, inner: R) -> Self {
        Self(BufReader::with_capacity(capacity, inner.take(u64::MAX)))
    }
}

impl<R: Read> LimitBufRead for LimitBufReader<R> {
    fn fill_buf_limited(&mut self, limit: usize) -> std::io::Result<&[u8]> {
        self.0.get_mut().set_limit(limit as u64);
        self.0.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        BufRead::consume(&mut self.0, amt);
    }
}
