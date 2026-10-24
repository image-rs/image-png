//! Usage example:
//!
//! ```
//! $ alias bench="rustup run nightly cargo bench"
//! $ bench --bench=expand_alpha --features=benchmarks -- --save-baseline my_baseline
//! ... tweak something ...
//! $ bench --bench=expand_alpha --features=benchmarks -- --baseline my_baseline
//! ```

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use png::benchable_apis::create_transform_fn;
use png::{BitDepth, ColorType, Info, Transformations};
use rand::Rng;

fn expand_alpha_all(c: &mut Criterion) {
    // RGB input (3) + RGBA output (4) + 1 for other data, as in `expand_paletted`,
    // keeps one row in a 32 KiB L1.
    let pixels = 32 * 1024 / (3 + 4 + 1);
    for color_type in [ColorType::Rgb, ColorType::Grayscale] {
        for trns in [false, true] {
            bench_expand_alpha(c, color_type, trns, pixels);
        }
    }
}

criterion_group!(benches, expand_alpha_all);
criterion_main!(benches);

fn bench_expand_alpha(c: &mut Criterion, color_type: ColorType, trns: bool, pixels: usize) {
    let samples = color_type.samples();
    let mut src = vec![0u8; pixels * samples];
    rand::rng().fill_bytes(&mut src);
    let trns_bytes = vec![0x42; samples];
    // Make 1 pixel in 16 the tRNS key, so the `trns=yes` cases write both alpha values.
    for pixel in src.chunks_exact_mut(samples).step_by(16) {
        pixel.copy_from_slice(&trns_bytes);
    }

    let mut info = Info::with_size(pixels as u32, 1);
    info.color_type = color_type;
    info.bit_depth = BitDepth::Eight;
    if trns {
        info.trns = Some(trns_bytes.into());
    }
    let transform_fn = create_transform_fn(&info, Transformations::ALPHA).unwrap();
    let mut dst = vec![0u8; pixels * (samples + 1)];

    let name = format!(
        "expand_alpha/{color_type:?}/trns={}",
        if trns { "yes" } else { "no" }
    );
    let mut group = c.benchmark_group(name);
    group.throughput(Throughput::Bytes(src.len() as u64));
    group.bench_function("row", |b| {
        b.iter(|| transform_fn(&src, &mut dst, &info));
    });
    group.finish();
}
