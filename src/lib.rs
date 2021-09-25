//! # PNG encoder and decoder
//!
//! This crate contains a PNG encoder and decoder. It supports reading of single lines or whole frames.
//!
//! ## The decoder
//!
//! The most important types for decoding purposes are [`Decoder`](struct.Decoder.html) and
//! [`Reader`](struct.Reader.html). They both wrap a `std::io::Read`.
//! `Decoder` serves as a builder for `Reader`. Calling `Decoder::read_info` reads from the `Read` until the
//! image data is reached.
//!
//! ### Using the decoder
//! ```
//! use std::fs::File;
//! # use std::path::PathBuf;
//! # let mut path = PathBuf::from("tests/pngsuite/basi0g01.png");
//! # xtest_data::setup!().filter([xtest_data::FsItem::File(&mut path)]).build();
//! // The decoder is a build for reader and can be used to set various decoding options
//! // via `Transformations`. The default output transformation is `Transformations::EXPAND
//! // | Transformations::STRIP_ALPHA`.
//! let decoder = png::Decoder::new(File::open(path).unwrap());
//! let mut reader = decoder.read_info().unwrap();
//! // Allocate the output buffer.
//! let mut buf = vec![0; reader.output_buffer_size()];
//! // Read the next frame. An APNG might contain multiple frames.
//! let info = reader.next_frame(&mut buf).unwrap();
//! // Grab the bytes of the image.
//! let bytes = &buf[..info.buffer_size()];
//! // Inspect more details of the last read frame.
//! let in_animation = reader.info().frame_control.is_some();
//! ```
//!
//! ## Encoder
//! ### Using the encoder
//!
//! ```no_run
//! // For reading and opening files
//! use std::path::Path;
//! use std::fs::File;
//! use std::io::BufWriter;
//!
//! let path = Path::new(r"/path/to/image.png");
//! let file = File::create(path).unwrap();
//! let ref mut w = BufWriter::new(file);
//!
//! let mut encoder = png::Encoder::new(w, 2, 1); // Width is 2 pixels and height is 1.
//! encoder.set_color(png::ColorType::Rgba);
//! encoder.set_depth(png::BitDepth::Eight);
//! encoder.set_trns(vec!(0xFFu8, 0xFFu8, 0xFFu8, 0xFFu8));
//! encoder.set_source_gamma(png::ScaledFloat::from_scaled(45455)); // 1.0 / 2.2, scaled by 100000
//! encoder.set_source_gamma(png::ScaledFloat::new(1.0 / 2.2));     // 1.0 / 2.2, unscaled, but rounded
//! let source_chromaticities = png::SourceChromaticities::new(     // Using unscaled instantiation here
//!     (0.31270, 0.32900),
//!     (0.64000, 0.33000),
//!     (0.30000, 0.60000),
//!     (0.15000, 0.06000)
//! );
//! encoder.set_source_chromaticities(source_chromaticities);
//! let mut writer = encoder.write_header().unwrap();
//!
//! let data = [255, 0, 0, 255, 0, 0, 0, 255]; // An array containing a RGBA sequence. First pixel is red and second pixel is black.
//! writer.write_image_data(&data).unwrap(); // Save
//! ```
//!

#![forbid(unsafe_code)]

#[macro_use]
extern crate bitflags;

pub mod chunk;
mod common;
mod decoder;
mod encoder;
mod filter;
mod srgb;
mod traits;
mod utils;

pub use crate::common::*;
pub use crate::decoder::{
    Decoded, Decoder, DecodingError, Limits, OutputInfo, Reader, StreamingDecoder,
};
pub use crate::encoder::{Encoder, EncodingError, StreamWriter, Writer};
pub use crate::filter::{AdaptiveFilterType, FilterType};
