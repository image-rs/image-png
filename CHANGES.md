## Unreleased

## 0.17.13

* Fix `Send` bound on `Reader`.

## 0.17.12

* Reject zero-sized frames.
* Optimized decoding of paletted images.
* Removed remaining uses of miniz_oxide for decoding.
* Correct lifetime used for `Info` struct.
* Fix build issue with `-Z minimal-versions`.

## 0.17.11

* Ignore subsequent iCCP chunks to match libpng behavior.
* Added an option to ignore ancillary chunks with invalid CRC.
* Added `new_with_info` constructor for encoder.
* Removed hard-coded memory limits.
* No longer allow zero sized images.
* Added `Reader::finish` to read all the auxillary chunks that comes after the
  image.

## 0.17.10

* Added Transformations::ALPHA
* Enable encoding pixel dimensions

## 0.17.9

* Fixed a bug in ICC profile decompression.
* Improved unfilter performance.

## 0.17.8

* Increased MSRV to 1.57.0.
* Substantially optimized encoding and decoding:
  - Autovectorize filtering and unfiltering.
  - Make the "fast" compression preset use fdeflate.
  - Switch decompression to always use fdeflate.
  - Updated to miniz_oxide 0.7.
  - Added an option to ignore checksums.
* Added corpus-bench example which measures the compression ratio and time to
  re-encode and subsequently decode a corpus of images.
* More fuzz testing.

## 0.17.7

* Fixed handling broken tRNS chunk.
* Updated to miniz_oxide 0.6.

## 0.17.6

* Added `Decoder::read_header_info` to query the information contained in the
  PNG header.
* Switched to using the flate2 crate for encoding.

## 0.17.5

* Fixed a regression, introduced by chunk validation, that made the decoder
  sensitive to the order of `gAMA`, `cHRM`, and `sRGB` chunks.

## 0.17.4

* Added `{Decoder,StreamDecoder}::set_ignore_text_chunk` to disable decoding of
  ancillary text chunks during the decoding process (chunks decoded by default).
* Added duplicate chunk checks. The decoder now enforces that standard chunks
  such as palette, gamma, … occur at most once as specified.
* Added `#[forbid(unsafe_code)]` again. This may come at a minor performance
  cost when decoding ASCII text for now.
* Fixed a bug where decoding of large chunks (>32kB) failed to produce the
  correct result, or fail the image decoding. As new chunk types are decoded
  this introduced regressions relative to previous versions.

## 0.17.3

* Fixed a bug where `Writer::finish` would not drop the underlying writer. This
  would fail to flush and leak memory when using a buffered file writers.
* Calling `Writer::finish` will now eagerly flush the underlying writer,
  returning any error that this operation may result in.
* Errors in inflate are now diagnosed with more details.
* The color and depth combination is now checked in stream decoder.

## 0.17.2

* Added support for encoding and decoding tEXt/zTXt/iTXt chunks.
* Added `Encoder::validate_sequence` to enable validation of the written frame
  sequence, that is, if the number of written images is consistent with the
  animation state.
* Validation is now off by default. The basis of the new validation had been
  introduced in 0.17 but this fixes some cases where this validation was too
  aggressive compared to previous versions.
* Added `Writer::finish` to fully check the write of the end of an image
  instead of silently ignoring potential errors in `Drop`.
* The `Writer::write_chunk` method now validates that the computed chunk length
  does not overflow the limit set by PNG.
* Fix an issue where the library would panic or even abort the process when
  `flush` or `write` of an underlying writer panicked, or in some other uses of
  `StreamWriter`.

## 0.17.1

* Fix panic in adaptive filter method `sum_buffer`

## 0.17.0

* Increased MSRV to 1.46.0
* Rework output info usage
* Implement APNG encoding
* Improve ergonomics of encoder set_palette and set_trns methods
* Make Info struct non-exhaustive
* Make encoder a core feature
* Default Transformations to Identity
* Add Adaptive filtering method for encoding
* Fix SCREAM_CASE on ColorType variants
* Forbid unsafe code

## 0.16.7

* Added `Encoder::set_trns` to register a transparency table to be written.

## 0.16.6

* Fixed silent integer overflows in buffer size calculation, resulting in
  panics from assertions and out-of-bounds accesses when actually decoding.
  This improves the stability of 32-bit and 16-bit targets and make decoding
  run as stable as on 64-bit.
* Reject invalid color/depth combinations. Some would lead to mismatched output
  buffer size and panics during decoding.
* Add `Clone` impl for `Info` struct.

## 0.16.5

* Decoding of APNG subframes is now officially supported and specified. Note
  that dispose ops and positioning in the image need to be done by the caller.
* Added encoding of indexed data.
* Switched to `miniz_oxide` for decompressing image data, with 30%-50% speedup
  in common cases and up to 200% in special ones.
* Fix accepting images only with consecutive IDAT chunks, rules out data loss.

## 0.16.4

* The fdAT frames are no longer inspected when the main image is read. This
  would previously be the case for non-interlaced images. This would lead to
  incorrect failure and, e.g. an error of the form `"invalid filter method"`.
* Fix always validating the last IDAT-chunks checksum, was sometimes ignored.
* Prevent encoding color/bit-depth combinations forbidden by the specification.
* The fixes for APNG/fdAT enable further implementation. The _next_ release is
  expected to officially support APNG.

## 0.16.3

* Fix encoding with filtering methods Up, Avg, Paeth
* Optimize decoding throughput by up to +30%

## 0.16.2

* Added method constructing an owned stream encoder.

## 0.16.1

* Addressed files bloating the packed crate

## 0.16.0

* Fix a bug compressing images with deflate
* Address use of deprecated error interfaces

## 0.15.3

* Fix panic while trying to encode empty images. Such images are no longer
  accepted and error when calling `write_header` before any data has been
  written. The specification does not permit empty images.

## 0.15.2

* Fix `EXPAND` transformation to leave bit depths above 8 unchanged

## 0.15.1

* Fix encoding writing invalid chunks. Images written can be corrected: see
  https://github.com/image-rs/image/issues/1074 for a recovery.
* Fix a panic in bit unpacking with checked arithmetic (e.g. in debug builds)
* Added better fuzzer integration
* Update `term`, `rand` dev-dependency
* Note: The `show` example program requires a newer compiler than 1.34.2 on
  some targets due to depending on `glium`. This is not considered a breaking
  bug.

## 0.15

Begin of changelog
