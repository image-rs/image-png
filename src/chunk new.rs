//! Chunk types and functions
#![allow(dead_code)]
// #![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

macro_rules! chunk_types {
    ($($chunk:ident[$type:expr]($size:expr)$(: $desc:literal)?)*) => {
        #[derive(PartialEq, Eq, Clone, Copy)]
        pub enum ChunkType {
            $($chunk,)*
            Unknown([u8; 4])
        }

        impl ChunkType {
            pub const fn type_of(data: [u8; 4]) -> Self {
                match data {
                    $($chunk::TYPE => Self::$chunk,)*
                    data => Self::Unknown(data)
                }
            }
        }

        impl std::ops::Deref for ChunkType {
            type Target = [u8; 4];

            fn deref(&self) -> &Self::Target {
                match self {
                    $(Self::$chunk => &$chunk,)*
                    Self::Unknown(ref data) => data
                }
            }
        }

        impl std::fmt::Debug for ChunkType {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(Self::$chunk => write!(f, "{:?}", $chunk),)*
                    Self::Unknown(data) => f.debug_struct("ChunkType")
                        .field("type", data)
                        .field("critical", &is_critical(*data))
                        .field("private", &is_private(*data))
                        .field("reserved", &reserved_set(*data))
                        .field("safecopy", &safe_to_copy(*data))
                        .finish()
                }
            }
        }

        $(
            $(#[doc=$desc])?
            #[derive(PartialEq, Eq)]
            pub struct $chunk;

            impl Chunk for $chunk {
                const TYPE: [u8; 4] = $type;
                const MAX_LENGTH: usize = $size as usize;
            }

            impl std::ops::Deref for $chunk {
                type Target = [u8; 4];

                fn deref(&self) -> &Self::Target {
                    &Self::TYPE
                }
            }

            impl std::fmt::Debug for $chunk {
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    f.debug_struct("ChunkType")
                        .field("type", &stringify!($chunk))
                        $(.field("description", &$desc))?
                        .field("critical", &Self::is_critical())
                        .field("private", &Self::is_private())
                        .field("reserved", &Self::reserved_set())
                        .field("safecopy", &Self::safe_to_copy())
                        .finish()
                }
            }
        )*
    };
}

// -- Chunk type determination --

/// Returns true if the chunk is critical.
const fn is_critical(chunk: [u8; 4]) -> bool {
    chunk[0] & 0b_00100000 == 0
}

/// Returns true if the chunk is private.
const fn is_private(chunk: [u8; 4]) -> bool {
    chunk[1] & 0b_00100000 != 0
}

/// Checks whether the reserved bit of the chunk name is set.
/// If it is set the chunk name is invalid.
const fn reserved_set(chunk: [u8; 4]) -> bool {
    chunk[2] & 0b_00100000 != 0
}

/// Returns true if the chunk is safe to copy if unknown.
const fn safe_to_copy(chunk: [u8; 4]) -> bool {
    chunk[3] & 0b_00100000 != 0
}

use io::Write;
use std::{io, ops};

pub trait Chunk: ops::Deref<Target = [u8; 4]> {
    const TYPE: [u8; 4];
    const MAX_LENGTH: usize;

    // -- Chunk type determination --

    /// Returns true if the chunk is critical.
    fn is_critical() -> bool {
        is_critical(Self::TYPE)
    }

    /// Returns true if the chunk is private.
    fn is_private() -> bool {
        is_private(Self::TYPE)
    }

    /// Checks whether the reserved bit of the chunk name is set.
    /// If it is set the chunk name is invalid.
    fn reserved_set() -> bool {
        reserved_set(Self::TYPE)
    }

    /// Returns true if the chunk is safe to copy if unknown.
    fn safe_to_copy() -> bool {
        safe_to_copy(Self::TYPE)
    }

    fn write<W: std::io::Write>(w: &mut W, data: &[u8]) -> io::Result<()> {
        assert!(data.len() <= Self::MAX_LENGTH);

        w.write_all(&data.len().to_be_bytes())?;
        w.write_all(&Self::TYPE)?;
        w.write_all(data)?;

        let mut crc = crc32fast::Hasher::new();
        crc.update(&Self::TYPE);
        crc.update(data);
        w.write_all(&crc.finalize().to_be_bytes())
    }
}

// we would need specialization for this to work (I think)
// impl<T: ChunkType> std::ops::Deref for T {
//     type Target = [u8; 4];
//
//     fn deref(&self) -> &Self::Target {
//         &Self::TYPE
//     }
// }

chunk_types! {
    // -- Critical chunks --

    IHDR[*b"IHDR"](13): "Image header"
    PLTE[*b"PLTE"](u32::MAX): "Palette"
    IDAT[*b"IDAT"](u32::MAX): "Image data"
    IEND[*b"IEND"](0): "Image trailer"

    // -- Ancillary chunks --

    tRNS[*b"tRNS"](u32::MAX / 3): "Transparency"
    bKGD[*b"bKGD"](6): "Background color"
    tIME[*b"tIME"](7): "Image last-modification time"
    pHYs[*b"pHYs"](9): "Physical pixel dimensions"
    cHRM[*b"cHRM"](32): "Source system's pixel chromaticities"
    gAMA[*b"gAMA"](4): "Source system's gamma value"

    // -- Extension chunks --

    acTL[*b"acTL"](8): "Animation control"
    fcTL[*b"fcTL"](26): "Frame control"
    fdAT[*b"fdAT"](u32::MAX - 4): "Frame data"

}

use crate::{
    AnimationControl, BitDepth, ColorType, FrameControl, ScaledFloat, SourceChromaticities, Time,
};

impl IHDR {
    pub fn encode<W: Write>(
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
        Self::write(w, &data)
    }
}

impl cHRM {
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

    pub fn encode<W: Write>(w: &mut W, c: SourceChromaticities) -> io::Result<()> {
        Self::write(w, &Self::chromaticities_to_be_bytes(c))
    }
}

impl PLTE {
    pub fn encode<W: Write>(w: &mut W, palette: &[u8]) -> io::Result<()> {
        let len = palette.len() - palette.len() % 3;
        Self::write(w, &palette[..len])
    }
}

impl gAMA {
    pub fn encode<W: Write>(w: &mut W, g: ScaledFloat) -> io::Result<()> {
        Self::write(w, &g.into_scaled().to_be_bytes())
    }
}

impl tIME {
    pub fn encode<W: Write>(w: &mut W, t: Time) -> io::Result<()> {
        // validate data (month < 12 ...)
        let [y0, y1] = t.year.to_be_bytes();
        Self::write(w, &[y0, y1, t.month, t.day, t.hour, t.minute, t.second])
    }
}

impl fcTL {
    pub fn encode<W: Write>(w: &mut W, fc: FrameControl) -> io::Result<()> {
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

        Self::write(w, &data)
    }
}

impl acTL {
    pub fn encode<W: Write>(w: &mut W, ac: AnimationControl) -> io::Result<()> {
        let mut data = [0; 8];
        data[..4].copy_from_slice(&ac.num_frames.to_be_bytes());
        data[4..].copy_from_slice(&ac.num_plays.to_be_bytes());
        Self::write(w, &data)
    }
}

impl fdAT {
    pub fn encode<W: Write>(w: &mut W, seq_num: u32, data: &[u8]) -> io::Result<()> {
        let mut all_data = vec![0u8; 4 + data.len()];
        all_data[..4].copy_from_slice(&seq_num.to_be_bytes());
        all_data[4..].copy_from_slice(&data);
        Self::write(w, &all_data)
    }
}
