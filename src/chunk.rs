//! Chunk types and functions
#![allow(dead_code)]
#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
use core::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChunkType(pub [u8; 4]);

// -- Critical chunks --

/// Image header
pub const IHDR: ChunkType = ChunkType(*b"IHDR");
/// Palette
pub const PLTE: ChunkType = ChunkType(*b"PLTE");
/// Image data
pub const IDAT: ChunkType = ChunkType(*b"IDAT");
/// Image trailer
pub const IEND: ChunkType = ChunkType(*b"IEND");

// -- Ancillary chunks --

/// Transparency
pub const tRNS: ChunkType = ChunkType(*b"tRNS");
/// Background colour
pub const bKGD: ChunkType = ChunkType(*b"bKGD");
/// Image last-modification time
pub const tIME: ChunkType = ChunkType(*b"tIME");
/// Physical pixel dimensions
pub const pHYs: ChunkType = ChunkType(*b"pHYs");
/// Source system's pixel chromaticities
pub const cHRM: ChunkType = ChunkType(*b"cHRM");
/// Source system's gamma value
pub const gAMA: ChunkType = ChunkType(*b"gAMA");

// -- Extension chunks --

/// Animation control
pub const acTL: ChunkType = ChunkType(*b"acTL");
/// Frame control
pub const fcTL: ChunkType = ChunkType(*b"fcTL");
/// Frame data
pub const fdAT: ChunkType = ChunkType(*b"fdAT");

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

use crate::{
    AnimationControl, BitDepth, ColorType, FrameControl, ScaledFloat, SourceChromaticities, Time,
};
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

#[rustfmt::skip]
fn chromaticities_to_be_bytes(c: SourceChromaticities) -> [u8; 32] {
    let white_x = c.white.0.into_scaled().to_be_bytes();
    let white_y = c.white.1.into_scaled().to_be_bytes();
    let red_x   = c.red.0.into_scaled().to_be_bytes();
    let red_y   = c.red.1.into_scaled().to_be_bytes();
    let green_x = c.green.0.into_scaled().to_be_bytes();
    let green_y = c.green.1.into_scaled().to_be_bytes();
    let blue_x  = c.blue.0.into_scaled().to_be_bytes();
    let blue_y  = c.blue.1.into_scaled().to_be_bytes();
    [
        white_x[0], white_x[1], white_x[2], white_x[3],
        white_y[0], white_y[1], white_y[2], white_y[3],
        red_x[0],   red_x[1],   red_x[2],   red_x[3],
        red_y[0],   red_y[1],   red_y[2],   red_y[3],
        green_x[0], green_x[1], green_x[2], green_x[3],
        green_y[0], green_y[1], green_y[2], green_y[3],
        blue_x[0],  blue_x[1],  blue_x[2],  blue_x[3],
        blue_y[0],  blue_y[1],  blue_y[2],  blue_y[3],
    ]
}

pub fn cHRM_encode<W: Write>(w: &mut W, c: SourceChromaticities) -> io::Result<()> {
    encode_chunk(w, cHRM, &chromaticities_to_be_bytes(c))
}

pub fn PLTE_encode<W: Write>(w: &mut W, palette: &[u8]) -> io::Result<()> {
    let len = palette.len() - palette.len() % 3;
    encode_chunk(w, PLTE, &palette[..len])
}

pub fn gAMA_encode<W: Write>(w: &mut W, g: ScaledFloat) -> io::Result<()> {
    encode_chunk(w, gAMA, &g.into_scaled().to_be_bytes())
}

pub fn tIME_encode<W: Write>(w: &mut W, t: Time) -> io::Result<()> {
    // validate data (month < 12 ...)
    let [y0, y1] = t.year.to_be_bytes();
    encode_chunk(
        w,
        tIME,
        &[y0, y1, t.month, t.day, t.hour, t.minute, t.second],
    )
}

pub fn fcTL_encode<W: Write>(w: &mut W, fc: FrameControl) -> io::Result<()> {
    let mut data = [0u8; 26];
    data[..4].copy_from_slice(&fc.sequence_number.to_be_bytes());
    data[4..8].copy_from_slice(&fc.width.to_be_bytes());
    data[8..12].copy_from_slice(&fc.height.to_be_bytes());
    data[12..16].copy_from_slice(&fc.x_offset.to_be_bytes());
    data[16..20].copy_from_slice(&fc.y_offset.to_be_bytes());
    data[20..22].copy_from_slice(&fc.delay_num.to_be_bytes());
    data[22..24].copy_from_slice(&fc.delay_den.to_be_bytes());
    data[24] = fc.dispose_op as u8;
    data[25] = fc.blend_op as u8;

    encode_chunk(w, fcTL, &data)
}

pub fn acTL_encode<W: Write>(w: &mut W, ac: AnimationControl) -> io::Result<()> {
    let mut data = [0; 8];
    data[..4].copy_from_slice(&ac.num_frames.to_be_bytes());
    data[4..].copy_from_slice(&ac.num_plays.to_be_bytes());
    encode_chunk(w, acTL, &data)
}

pub fn fdAT_encode<W: Write>(w: &mut W, seq_num: u32, data: &[u8]) -> io::Result<()> {
    let mut all_data = vec![0u8; 4 + data.len()];
    all_data[..4].copy_from_slice(&seq_num.to_be_bytes());
    all_data[4..].copy_from_slice(&data);
    encode_chunk(w, fdAT, &all_data)
}
