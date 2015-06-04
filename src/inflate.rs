extern crate inflate;
//use inflate;

use std::cmp;
use std::io;

use utils;

/// Flush mode this enum does not have any effect
#[allow(dead_code)]
pub enum Flush {
    /// Default strategy
    None,
    /// Tries to output as much as possible
    Sync,
    /// Most efficient if the output buffer is big enough
    /// to decompress everythin at once.
    Finish,
}

/// Inflater wraps `InflateStream`
pub struct Inflater {
    stream: inflate::InflateStream,
    buffer: Vec<u8>,
    pos: usize,
    eof: bool
}

impl Inflater {
    pub fn new() -> Inflater {
        Inflater {
            stream: inflate::InflateStream::from_zlib(),
            buffer: Vec::with_capacity(1024),
            pos: 0,
            eof: false
        }
    }
    
    /// Inflates the `input` stream.
    ///
    /// The decoded bytes are written into `output` which is returns as `out` which has been
    /// truncated to only contain the decoded bytes. `flush` sets the flushing strategy.
    ///
    /// The return value `Ok(at_eof, consumed, out)` indicates how many bytes have been
    /// `consumed` from `input`. `out` contains the bytes that were decoded from the input
    /// stream. This function should be called until `at_eof` indicates that the end of the
    /// stream has been reached.
    pub fn inflate<'a>(&mut self, input: &[u8], output: &'a mut [u8], flush: Flush)
    -> io::Result<(bool, usize, &'a mut [u8])> {
        if self.buffer.len() > 0 {
            let to_copy = cmp::min(self.buffer.len()-self.pos, output.len());
            utils::copy_memory(&self.buffer[self.pos..self.pos+to_copy], &mut output[..to_copy]);
            self.pos += to_copy;
            if self.pos == self.buffer.len() {
                self.pos = 0;
                self.buffer.clear();
            }
            Ok((false, 0, &mut output[..to_copy]))
        } else {
            let (len, buf) = try!(self.stream.update(input).map_err(
                |msg| io::Error::new(io::ErrorKind::Other, msg))
            );
            let to_copy = cmp::min(buf.len(), output.len());
            utils::copy_memory(&buf[..to_copy], &mut output[..to_copy]);
            self.buffer.push_all(&buf[to_copy..]);
            Ok((false, len, &mut output[..to_copy]))
        }
    }
}