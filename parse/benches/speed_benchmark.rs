use criterion::{criterion_group, criterion_main, Criterion};

pub fn bench_parsers(c: &mut Criterion) {
    let mut group = c.benchmark_group("Parsers");
    group.bench_function("Chumsky", |b| {
        b.iter(|| {
            for p in std::fs::read_dir("../example_programs").unwrap() {
                let src = std::fs::read_to_string(p.unwrap().path()).unwrap();

                let _cdoc = macor_parse::chumskyparse::parse_document(&src).0.unwrap();
            }
        })
    });
    group.bench_function("LALRPOP", |b| {
        b.iter(|| {
            for p in std::fs::read_dir("../example_programs").unwrap() {
                let src = std::fs::read_to_string(p.unwrap().path()).unwrap();

                let _ldoc = macor_parse::parse_document(&src);
            }
        })
    });
    group.finish();
}

criterion_group!(benches, bench_parsers);
criterion_main!(benches);
