use criterion::{criterion_group, criterion_main, Criterion};
use std::process::Command;

pub fn bench_macor(c: &mut Criterion) {
    let num_session = 1;
    let mut group = c.benchmark_group("Macor");
    for p in std::fs::read_dir("./example_programs/").unwrap() {
        let p = p.unwrap().path();

        group.bench_function(p.file_name().unwrap().to_str().unwrap(), |b| {
            b.iter(|| {
                Command::new("macor")
                    .arg("verify")
                    .arg("-n")
                    .arg(num_session.to_string())
                    .arg("--iter")
                    .arg("100")
                    .arg(&p.canonicalize().unwrap())
                    .output()
                    .expect("Could not find path for MACOR");
            })
        });
    }
    group.finish();
}

pub fn bench_compare(c: &mut Criterion) {
    let num_session = 1;
    let mut group = c.benchmark_group("Compare Macor and OFMC");
    for p in std::fs::read_dir("./example_programs/").unwrap() {
        let p = p.unwrap().path();

        let test_name = p.file_name().unwrap().to_str().unwrap();

        group.bench_function(format!("{test_name} - macor"), |b| {
            b.iter(|| {
                Command::new("macor")
                    .arg("verify")
                    .arg("-n")
                    .arg(num_session.to_string())
                    .arg(&p.canonicalize().unwrap())
                    .output()
                    .expect("Could not find path for MACOR");
            })
        });
        group.bench_function(format!("{test_name} - ofmc"), |b| {
            b.iter(|| {
                Command::new("ofmc")
                    .arg("--numSess")
                    .arg(num_session.to_string())
                    .arg(&p.canonicalize().unwrap())
                    .output()
                    .expect("Could not find path for OFMC");
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench_macor, bench_compare);
criterion_main!(benches);
