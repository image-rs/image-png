// Copyright 2013 The Servo Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use libc::{c_void, c_char, c_int, c_uint, c_ulong};
use std::ptr::{null_mut, null};
use std::ffi::CStr;
use std::vec;

type alloc_func = extern "C" fn(/*opaque*/ *const c_void, /*items*/ c_uint, /*size*/ c_uint);
type free_func = extern "C" fn(/*opaque*/ *const c_void, /*address*/ *const c_void);

struct z_stream {
    next_in: *const u8,       /* next input byte */
    avail_in: c_uint,   /* number of bytes available at next_in */
    total_in: c_ulong,   /* total number of input bytes read so far */

    next_out: *mut u8,  /* next output byte should be put there */
    avail_out: c_uint,  /* remaining free space at next_out */
    total_out: c_ulong, /* total number of bytes output so far */

    msg: *const c_char,       /* last error message, NULL if no error */
    state: *const c_void,     /* not visible by applications */

    zalloc: alloc_func, /* used to allocate the internal state */
    zfree: free_func,   /* used to free the internal state */
    opaque: *const c_void,    /* private data object passed to zalloc and zfree */

    data_type: c_int,   /* best guess about the data type: binary or text */
    adler: c_ulong,      /* adler32 value of the uncompressed data */
    reserved: c_ulong    /* reserved for future use */
}

#[link(name="z")]
extern {
    fn zlibVersion() -> *const c_char;
    fn zError(err: c_int) -> *const c_char;

    fn inflateInit_(stream: *mut z_stream, version: *const c_char, stream_size: c_int) -> c_int;
    fn inflate(stream: *mut z_stream, flush: c_int) -> c_int;
    fn inflateEnd(stream: *mut z_stream) -> c_int;
}

unsafe fn inflateInit(stream: *mut z_stream) -> c_int {
    // HACK(eddyb) zlib does this:
    // #define inflateInit(strm) inflateInit_((strm), ZLIB_VERSION, (int)sizeof(z_stream))
    let ZLIB_VERSION = zlibVersion();
    inflateInit_(stream, ZLIB_VERSION, ::std::mem::size_of::<z_stream>() as c_int)
}

unsafe fn error_to_str(err: c_int) -> String {
    let s = CStr::from_ptr(zError(err));
    String::from_utf8_lossy(s.to_bytes()).into_owned()
}

pub struct InflateStream {
    buffer: Vec<u8>,
    stream: z_stream
}

impl InflateStream {
    pub fn new(buffer_size: usize) -> InflateStream {
        let mut stream = InflateStream {
            buffer: Vec::with_capacity(buffer_size),
            stream: z_stream {
                next_in: null(),
                avail_in: 0,
                total_in: 0,

                next_out: null_mut(),
                avail_out: 0,
                total_out: 0,

                msg: null(),
                state: null(),

                zalloc: unsafe {::std::mem::transmute(0usize)},
                zfree: unsafe {::std::mem::transmute(0usize)},
                opaque: null(),

                data_type: 0,
                adler: 0,
                reserved: 0
            }
        };

        let err = unsafe {
            inflateInit(&mut stream.stream as *mut z_stream)
        };
        // TODO(eddyb) handle errors.
        if err != 0 {
            panic!("zlib::inflateInit error `{}` ({})", err, unsafe { error_to_str(err) });
        }

        stream
    }

    pub fn from_zlib() -> InflateStream {
        // FIXME(eddyb) harcoded buffer size.
        InflateStream::new(32 * 1024)
    }

    pub fn update<'a>(&'a mut self, data: &[u8]) -> Result<(usize, &'a [u8]), String> {
        self.stream.next_in = data.as_ptr();
        self.stream.avail_in = data.len() as c_uint;

        self.stream.next_out = self.buffer.as_mut_ptr();
        self.stream.avail_out = self.buffer.capacity() as c_uint;

        let err = unsafe {
            // TODO(eddyb) do proper flushing.
            inflate(&mut self.stream as *mut z_stream, 0)
        };
        if err < 0 && err != -5 {
            return Err(format!("inflate error `{}` ({})", err, unsafe { error_to_str(err) }));
        }

        let used_in = data.len() - self.stream.avail_in as usize;
        let used_out = self.buffer.capacity() - self.stream.avail_out as usize;

        unsafe {
            self.buffer.set_len(used_out);
        }

        Ok((used_in, self.buffer.as_slice()))
    }
}

impl Drop for InflateStream {
    fn drop(&mut self) {
        let err = unsafe {
            inflateEnd(&mut self.stream as *mut z_stream)
        };
        // TODO(eddyb) handle errors properly.
        if err != 0 {
            panic!("zlib::inflateEnd error `{}` ({})", err, unsafe { error_to_str(err) });
        }
    }
}

/// ---

use std::io;

use utils;
use std::cmp;

/// Inflater wraps `InflateStream`
pub struct Inflater {
    stream: InflateStream,
    state: Vec<u8>,
    pos: usize,
    eof: bool
}


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

impl Inflater {
    pub fn new() -> Inflater {
        Inflater {
            stream: InflateStream::from_zlib(),
            state: Vec::new(),
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
        if self.state.len() > 0 {
            let to_copy = cmp::min(self.state.len()-self.pos, output.len());
            utils::copy_memory(&self.state[self.pos..self.pos+to_copy], &mut output[..to_copy]);
            self.pos += to_copy;
            if self.pos == self.state.len() {
                self.pos = 0;
                self.state.clear();
            }
            Ok((false, 0, &mut output[..to_copy]))
        } else {
            let (len, buf) = try!(self.stream.update(input).map_err(
                |msg| io::Error::new(io::ErrorKind::Other, msg))
            );
            if buf.len() > 0 {
                self.state.extend(buf.iter().map(|&v| v))
            }
            Ok((false, len, &mut []))
        }
    }
}