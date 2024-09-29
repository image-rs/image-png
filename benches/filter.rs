//! Usage example:
//!
//! ```
//! $ alias bench="rustup run nightly cargo bench"
//! $ bench --bench=filter --features=benchmarks,unstable -- --save-baseline my_baseline
//! ... tweak something, say the Sub filter ...
//! $ bench --bench=filter --features=benchmarks,unstable -- filter=Sub --baseline my_baseline
//! ```

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use png::benchable_apis;
use png::FilterType;
use rand::Rng;

fn filter_all(c: &mut Criterion) {
    let bpps = [1, 2, 3, 4, 6, 8];
    let filters = [
        FilterType::Sub,
        FilterType::Up,
        FilterType::Avg,
        FilterType::Paeth,
    ];
    for &filter in filters.iter() {
        for &bpp in bpps.iter() {
            bench_filter(c, filter, bpp);
        }
    }
}

criterion_group!(benches, filter_all);
criterion_main!(benches);

fn bench_filter(c: &mut Criterion, filter: FilterType, bpp: u8) {
    let mut group = c.benchmark_group("filter");

    fn get_random_bytes<R: Rng>(rng: &mut R, n: usize) -> Vec<u8> {
        use rand::Fill;
        let mut result = vec![0u8; n];
        result.as_mut_slice().try_fill(rng).unwrap();
        result
    }
    let mut rng = rand::thread_rng();
    let row_size = 4096 * (bpp as usize);
    let two_rows = get_random_bytes(&mut rng, row_size * 2);
    let mut out = vec![0; row_size];

    group.throughput(Throughput::Bytes(row_size as u64));
    group.bench_with_input(
        format!("filter={filter:?}/bpp={bpp}"),
        &two_rows,
        |b, two_rows| {
            let (prev_row, curr_row) = two_rows.split_at(row_size);
            let mut curr_row = curr_row.to_vec();
            b.iter(|| benchable_apis::filter(filter, bpp, prev_row, curr_row.as_mut_slice(), &mut out));
        },
    );
}
