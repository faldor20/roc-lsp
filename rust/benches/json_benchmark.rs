use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_json::{ser, Value};
use std::fs::File;
use std::io::Read;
#[derive(serde::Deserialize)]
struct Record {
    id: String,
}

fn decode_json(c: &mut Criterion) {
    let mut standard_json = String::new();
    let mut whitespace_json = String::new();
    File::open("../small-json.json")
        .unwrap()
        .read_to_string(&mut standard_json)
        .unwrap();
    File::open("../formatted-json.json")
        .unwrap()
        .read_to_string(&mut whitespace_json)
        .unwrap();

    c.bench_function("decode_json_value", |b| {
        b.iter(|| {
            let value: Value = serde_json::from_str(black_box(&standard_json)).unwrap();
            let val = value
                .as_array()
                .unwrap()
                .last()
                .unwrap()
                .as_object()
                .unwrap()["id"]
                .as_str()
                .unwrap();
            black_box(val);
            black_box(value);
        })
    });
    c.bench_function("decode_json_id_only", |b| {
        b.iter(|| {
            let value: Vec<Record> = serde_json::from_str(black_box(&standard_json)).unwrap();
            black_box(&value.last().unwrap().id);
            black_box(value);
        })
    });
    c.bench_function("decode_json_value_whitespace", |b| {
        b.iter(|| {
            let value: Value = serde_json::from_str(black_box(&whitespace_json)).unwrap();
            let val = value
                .as_array()
                .unwrap()
                .last()
                .unwrap()
                .as_object()
                .unwrap()["id"]
                .as_str()
                .unwrap();
            black_box(val);
            black_box(value);
        })
    });
}

criterion_group!(benches, decode_json);
criterion_main!(benches);
