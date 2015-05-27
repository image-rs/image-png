extern crate inflate;
extern crate compress;

use self::compress::flate;


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
    state: Vec<u8>,
    pos: usize,
    eof: bool
}

impl Inflater {
    pub fn new() -> Inflater {
        Inflater {
            stream: inflate::InflateStream::from_zlib(),
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
        } else if self.eof {
            Ok((true, 0, &mut []))
        } else {
            let (len, buf) = try!(self.stream.update(input).map_err(
                |msg| io::Error::new(io::ErrorKind::Other, msg))
            );
            if len > 0 {
                self.state.extend(buf.iter().map(|&v| v))
            } else if input.len() == 0 {
                self.eof = true
            }
            Ok((false, len, &mut []))
        }
    }
}

/// Inflater wraps `InflateStream`
pub struct ZlibReader {
    stream: inflate::InflateStream,
}

impl ZlibReader {
    pub fn new() -> ZlibReader {
        ZlibReader {
            stream: inflate::InflateStream::from_zlib(),
        }
    }
    
    pub fn read_all(&mut self, mut input: &[u8]) -> io::Result<Vec<u8>> {
        let mut out = Vec::new();
        while input.len() > 0 {
            let (used, buf) = try!(self.stream.update(input).map_err(
                |msg| io::Error::new(io::ErrorKind::Other, msg))
            );
            input = &input[used..];
            out.extend(buf.iter().map(|&v| v))
        }
        Ok(out)
    }
}

#[test]
fn decode_correct() {
    use std::fs::File;
    use std::io::Read;
    let files = [
        //"tests/samples/PNG_transparency_demonstration_1.png",
        //"tests/pngsuite/oi4n0g16.png",
        "issue300.png"
    ];
    for file in files.iter() {
        let mut decoder = ::Decoder::new(File::open(file).unwrap());
        let (_, mut reader) = decoder.read_info().unwrap();
        let mut raw_out = Vec::new();
        let mut out = Vec::new();
        while let Some(obj) = reader.decode_next().unwrap() {
            match obj {
                ::Decoded::PartialChunk(_, data) => raw_out.extend(data.iter().map(|&v| v)),
                ::Decoded::ImageData(data) => out.extend(data.iter().map(|&v| v)),
                _ => ()
            }
        }
        //let data = ZlibReader::new().read_all(&raw_out).unwrap();
        let mut data2 = Vec::new();
        flate::Decoder::new(&*raw_out).read_to_end(&mut data2).unwrap();
        assert_eq!(data2, out);
    }
}

/*
#[test]
fn decompression() {
    let input = [0x78_u8, 0xda, 0xff, 0x80, 0x30, 0x30, 0x82, 0x00, 0x80, 0x00, 0xcb, 0x00, 0x30];
    let data = ZlibReader::new().read_all(&input).unwrap();
    assert_eq!(&data, &[0xbf])
}*/