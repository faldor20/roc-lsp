use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_json::Value;
use std::fs::File;
use std::io::Read;

fn decode_json(c: &mut Criterion) {
    let mut file = File::open("../small-json.json").unwrap();
    let mut json_data = String::new();
    file.read_to_string(&mut json_data).unwrap();

    c.bench_function("decode_json", |b| {
        b.iter(|| {
            let value: Value = serde_json::from_str(black_box(&json_data)).unwrap();
            black_box(value);
        })
    });
}

criterion_group!(benches, decode_json);
criterion_main!(benches);
