use std::fs;
use std::path::Path;

use criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkGroup, Criterion,
};
use png::Decoder;

criterion_group! {benches, load_all}
criterion_main!(benches);

fn load_all(c: &mut Criterion) {
    let mut g = c.benchmark_group("next_frame_info");
    bench_file(&mut g, Path::new("tests/animated/basic_f20.png"), 18, 35);
}

fn bench_file(
    g: &mut BenchmarkGroup<WallTime>,
    png_path: &Path,
    number_of_frames_to_skip: usize,
    expected_fctl_sequence_number: u32,
) {
    let data = fs::read(png_path).unwrap();
    let name = format!("{}: {} skips", png_path.display(), number_of_frames_to_skip);
    g.bench_with_input(&name, data.as_slice(), |b, data| {
        b.iter(|| {
            let decoder = Decoder::new(data);
            let mut reader = decoder.read_info().unwrap();
            for _ in 0..number_of_frames_to_skip {
                reader.next_frame_info().unwrap();
            }
            assert_eq!(
                reader
                    .info()
                    .frame_control
                    .as_ref()
                    .unwrap()
                    .sequence_number,
                expected_fctl_sequence_number,
            );
        })
    });
}
