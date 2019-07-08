//! # PNG encoder and decoder
//! This crate contains a PNG encoder and decoder. It supports reading of single lines or whole frames.
//! ## The decoder
//! The most important types for decoding purposes are [`Decoder`](struct.Decoder.html) and
//! [`Reader`](struct.Reader.html). They both wrap a `std::io::Read`.
//! `Decoder` serves as a builder for `Reader`. Calling `Decoder::read_info` reads from the `Read` until the
//! image data is reached.
//! ### Using the decoder
//!     # extern crate image_core;
//!     use std::fs::File;
//!     use image_core::ImageDecoder;
//!     use png::PngDecoder;
//!
//!     let decoder = PngDecoder::new(File::open("tests/pngsuite/basi0g01.png").unwrap()).unwrap();
//!     let (width, height) = decoder.dimensions();
//!     let color_type = decoder.colortype();
//!     // Allocate the output buffer.
//!     let mut buf = vec![0; decoder.total_bytes() as usize];
//!     decoder.read_image(&mut buf).unwrap();
//! ## Encoder
//! ### Using the encoder
//! ```ignore
//!     // For reading and opening files
//!     use std::path::Path;
//!     use std::fs::File;
//!     use std::io::BufWriter;
//!     // To use encoder.set()
//!     use png::HasParameters;
//!
//!     let path = Path::new(r"/path/to/image.png");
//!     let file = File::create(path).unwrap();
//!     let ref mut w = BufWriter::new(file);
//!
//!     let mut encoder = png::Encoder::new(w, 2, 1); // Width is 2 pixels and height is 1.
//!     encoder.set(png::ColorType::RGBA).set(png::BitDepth::Eight);
//!      let mut writer = encoder.write_header().unwrap();
//!
//!     let data = [255, 0, 0, 255, 0, 0, 0, 255]; // An array containing a RGBA sequence. First pixel is red and second pixel is black.
//!     writer.write_image_data(&data).unwrap(); // Save
//! ```
//!
//#![cfg_attr(test, feature(test))]

#![deny(unsafe_code)]

extern crate image_core;
extern crate num_iter;

pub mod chunk;
mod crc;
mod decoder;
#[cfg(feature = "png-encoding")]
mod encoder;
mod filter;
mod traits;
mod common;
mod utils;

pub use common::*;
pub use decoder::{PngReader, PngDecoder, StreamingDecoder, Decoded};
#[cfg(feature = "png-encoding")]
pub use encoder::PngEncoder;

pub use traits::{Parameter, HasParameters};
