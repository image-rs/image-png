//! Common types shared between the encoder and decoder

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PngColorType {
    Grayscale = 0,
    RGB = 2,
    Indexed = 3,
    GrayscaleAlpha = 4,
    RGBA = 6
}

impl PngColorType {
    /// Returns the number of samples used per pixel of `PngColorType`
    pub fn samples(&self) -> usize {
        use self::PngColorType::*;
        match *self {
            Grayscale | Indexed => 1,
            RGB => 3,
            GrayscaleAlpha => 2,
            RGBA => 4
        }
    }
    
    /// u8 -> Self. Temporary solution until Rust provides a canonical one.
    pub fn from_u8(n: u8) -> Option<PngColorType> {
        match n {
            0 => Some(PngColorType::Grayscale),
            2 => Some(PngColorType::RGB),
            3 => Some(PngColorType::Indexed),
            4 => Some(PngColorType::GrayscaleAlpha),
            6 => Some(PngColorType::RGBA),
            _ => None
        }
    }
}

/// Bit depth of the png file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BitDepth {
    One     = 1,
    Two     = 2,
    Four    = 4,
    Eight   = 8,
    Sixteen = 16,
}

impl BitDepth {
    /// u8 -> Self. Temporary solution until Rust provides a canonical one.
    pub fn from_u8(n: u8) -> Option<BitDepth> {
        match n {
            1 => Some(BitDepth::One),
            2 => Some(BitDepth::Two),
            4 => Some(BitDepth::Four),
            8 => Some(BitDepth::Eight),
            16 => Some(BitDepth::Sixteen),
            _ => None
        }
    }
}

/// Pixel dimensions information
#[derive(Clone, Copy, Debug)]
pub struct PixelDimensions {
    /// Pixels per unit, X axis
    pub xppu: u32,
    /// Pixels per unit, Y axis
    pub yppu: u32,
    /// Either *Meter* or *Unspecified*
    pub unit: Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
/// Physical unit of the pixel dimensions
pub enum Unit {
    Unspecified = 0,
    Meter = 1,
}

impl Unit {
    /// u8 -> Self. Temporary solution until Rust provides a canonical one.
    pub fn from_u8(n: u8) -> Option<Unit> {
        match n {
            0 => Some(Unit::Unspecified),
            1 => Some(Unit::Meter),
            _ => None
        }
    }
}

/// Frame control information
#[derive(Clone, Debug)]
pub struct FrameControl {
    /// Sequence number of the animation chunk, starting from 0
    pub sequence_number: u32,
    /// Width of the following frame
    pub width: u32,
    /// Height of the following frame
    pub height: u32,
    /// X position at which to render the following frame
    pub x_offset: u32,
    /// Y position at which to render the following frame
    pub y_offset: u32,
    /// Frame delay fraction numerator
    pub delay_num: u16,
    /// Frame delay fraction denominator
    pub delay_den: u16,
    /// Type of frame area disposal to be done after rendering this frame
    pub dispose_op: u8,
    /// Type of frame area rendering for this frame
    pub blend_op: u8,
}

/// Animation control information
#[derive(Clone, Copy, Debug)]
pub struct AnimationControl {
    /// Number of frames
    pub num_frames: u32,
    /// Number of times to loop this APNG.  0 indicates infinite looping.
    pub num_plays: u32,
}

/// PNG info struct
#[derive(Debug)]
pub struct Info {
    pub width: u32,
    pub height: u32,
    pub bit_depth: BitDepth,
    pub color_type: PngColorType,
    pub interlaced: bool,
    pub trns: Option<Vec<u8>>,
    pub pixel_dims: Option<PixelDimensions>,
    pub palette: Option<Vec<u8>>,
    pub frame_control: Option<FrameControl>,
    pub animation_control: Option<AnimationControl>
}

impl Default for Info {
    fn default() -> Info {
        Info {
            width: 0,
            height: 0,
            bit_depth: BitDepth::Eight,
            color_type: PngColorType::Grayscale,
            interlaced: false,
            palette: None,
            trns: None,
            pixel_dims: None,
            frame_control: None,
            animation_control: None
        }
    }
}

impl Info {
    /// Size of the image
    pub fn size(&self) -> (u32, u32) {
        (self.width, self.height)
    }
    
    /// Returns true if the image is an APNG image.
    pub fn is_animated(&self) -> bool {
        self.frame_control.is_some() && self.animation_control.is_some()
    }
    
    /// Returns the frame control information of the image
    pub fn animation_control(&self) -> Option<&AnimationControl> {
        self.animation_control.as_ref()
    }
    
    /// Returns the frame control information of the current frame
    pub fn frame_control(&self) -> Option<&FrameControl> {
        self.frame_control.as_ref()
    }
    
    /// Returns the bits per pixel
    pub fn bits_per_pixel(&self) -> usize {
        self.color_type.samples() * self.bit_depth as usize
    }
    
    /// Returns the bytes per pixel
    pub fn bytes_per_pixel(&self) -> usize {
        self.color_type.samples() * ((self.bit_depth as usize + 7) >> 3)
    }
    
    /// Returns the number of bytes needed for one deinterlaced image
    pub fn raw_bytes(&self) -> usize {
        self.height as usize * self.raw_row_length()
    }
    
    /// Returns the number of bytes needed for one deinterlaced row 
    pub fn raw_row_length(&self) -> usize {
        let bits = self.width as usize * self.color_type.samples() * self.bit_depth as usize;
        let extra = bits % 8;
        bits/8
        + match extra { 0 => 0, _ => 1 }
        + 1 // filter method
    }
    
    /// Returns the number of bytes needed for one deinterlaced row of width `width`
    pub fn raw_row_length_from_width(&self, width: u32) -> usize {
        let bits = width as usize * self.color_type.samples() * self.bit_depth as usize;
        let extra = bits % 8;
        bits/8
        + match extra { 0 => 0, _ => 1 }
        + 1 // filter method
    }
}
