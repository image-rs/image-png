//! Chunk types and functions
#![allow(dead_code)]
#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
use core::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChunkType(pub [u8; 4]);

// -- Critical chunks --

/// Image header
pub const IHDR: ChunkType = ChunkType([b'I', b'H', b'D', b'R']);
/// Palette
pub const PLTE: ChunkType = ChunkType([b'P', b'L', b'T', b'E']);
/// Image data
pub const IDAT: ChunkType = ChunkType([b'I', b'D', b'A', b'T']);
/// Image trailer
pub const IEND: ChunkType = ChunkType([b'I', b'E', b'N', b'D']);

// -- Ancillary chunks --

/// Transparency
pub const tRNS: ChunkType = ChunkType([b't', b'R', b'N', b'S']);
/// Background colour
pub const bKGD: ChunkType = ChunkType([b'b', b'K', b'G', b'D']);
/// Image last-modification time
pub const tIME: ChunkType = ChunkType([b't', b'I', b'M', b'E']);
/// Physical pixel dimensions
pub const pHYs: ChunkType = ChunkType([b'p', b'H', b'Y', b's']);
/// Source system's pixel chromaticities
pub const cHRM: ChunkType = ChunkType([b'c', b'H', b'R', b'M']);
/// Source system's gamma value
pub const gAMA: ChunkType = ChunkType([b'g', b'A', b'M', b'A']);
/// sRGB color space chunk
pub const sRGB: ChunkType = ChunkType([b's', b'R', b'G', b'B']);

// -- Extension chunks --

/// Animation control
pub const acTL: ChunkType = ChunkType([b'a', b'c', b'T', b'L']);
/// Frame control
pub const fcTL: ChunkType = ChunkType([b'f', b'c', b'T', b'L']);
/// Frame data
pub const fdAT: ChunkType = ChunkType([b'f', b'd', b'A', b'T']);

// -- Chunk type determination --

/// Returns true if the chunk is critical.
pub fn is_critical(ChunkType(type_): ChunkType) -> bool {
    type_[0] & 32 == 0
}

/// Returns true if the chunk is private.
pub fn is_private(ChunkType(type_): ChunkType) -> bool {
    type_[1] & 32 != 0
}

/// Checks whether the reserved bit of the chunk name is set.
/// If it is set the chunk name is invalid.
pub fn reserved_set(ChunkType(type_): ChunkType) -> bool {
    type_[2] & 32 != 0
}

/// Returns true if the chunk is safe to copy if unknown.
pub fn safe_to_copy(ChunkType(type_): ChunkType) -> bool {
    type_[3] & 32 != 0
}

impl fmt::Debug for ChunkType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct DebugType([u8; 4]);

        impl fmt::Debug for DebugType {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                for &c in &self.0[..] {
                    write!(f, "{:?}", char::from(c).escape_debug())?;
                }
                Ok(())
            }
        }

        f.debug_struct("ChunkType")
            .field("type", &DebugType(self.0))
            .field("critical", &is_critical(*self))
            .field("private", &is_private(*self))
            .field("reserved", &reserved_set(*self))
            .field("safecopy", &safe_to_copy(*self))
            .finish()
    }
}

use crate::{BitDepth, ColorType};
use io::Write;
use std::io;

pub fn encode_chunk<W: Write>(w: &mut W, chunk: ChunkType, data: &[u8]) -> io::Result<()> {
    w.write_all(&(data.len() as u32).to_be_bytes())?;
    w.write_all(&chunk.0)?;
    w.write_all(data)?;

    let mut crc = crc32fast::Hasher::new();
    crc.update(&chunk.0);
    crc.update(data);
    w.write_all(&crc.finalize().to_be_bytes())
}

pub fn IHDR_encode<W: Write>(
    w: &mut W,
    width: u32,
    height: u32,
    bit_depth: BitDepth,
    color_type: ColorType,
    interlaced: bool,
) -> io::Result<()> {
    let mut data = [0; 13];
    data[..4].copy_from_slice(&width.to_be_bytes());
    data[4..8].copy_from_slice(&height.to_be_bytes());
    data[8] = bit_depth as u8;
    data[9] = color_type as u8;
    data[12] = interlaced as u8;
    encode_chunk(w, IHDR, &data)
}

pub fn fdAT_encode<W: Write>(w: &mut W, seq_num: u32, data: &[u8]) -> io::Result<()> {
    let mut all_data = vec![0u8; 4 + data.len()];
    all_data[..4].copy_from_slice(&seq_num.to_be_bytes());
    all_data[4..].copy_from_slice(&data);
    encode_chunk(w, fdAT, &all_data)
}
