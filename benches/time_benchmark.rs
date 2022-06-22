use criterion::{criterion_group, criterion_main, Criterion};
use std::{path::PathBuf, process::Command};

pub fn bench_macor(c: &mut Criterion) {
    let macor_path = "macor";
    let protocol = "../example_programs/KeyEx1";
    let num_session = 1;
    let num_samples = 10;
    let mut group = c.benchmark_group("Macor");
    group.bench_function("MACOR", |b| {
        b.iter(|| {
            for _ in 0..num_samples {
                // macor verify -n 2 path/to/my/Protocol.AnB
                let _macor = Command::new(&macor_path)
                    .arg("verify")
                    .arg("-n")
                    .arg(num_session.to_string())
                    .arg(&protocol)
                    .output()
                    .expect("Could not find path for MACOR");
            }
        })
    });
    group.finish();
}

pub fn bench_compare(c: &mut Criterion) {
    //TODO ofmc path not hardcoded
    let ofmc_path: PathBuf = "ofmc".into();
    let macor_path = "macor";
    let protocol = "../example_programs/KeyEx1";
    let num_session = 1;
    let num_samples = 10;
    let mut group = c.benchmark_group("Programs");
    group.bench_function("OFMC", |b| {
        b.iter(|| {
            for _ in 0..num_samples {
                // ofmc --numSess 2 path/to/my/Protocol.AnB
                let _ofmc = Command::new(&ofmc_path)
                    .arg("--numSess")
                    .arg(num_session.to_string())
                    .arg(&protocol)
                    .output()
                    .expect("Could not find path for OFMC");
            }
        })
    });

    group.bench_function("MACOR", |b| {
        b.iter(|| {
            for _ in 0..num_samples {
                let _macor = Command::new(&macor_path)
                    .arg("verify")
                    .arg("-n")
                    .arg(num_session.to_string())
                    .arg(&protocol)
                    .output()
                    .expect("Could not find path for MACOR");
            }
        })
    });
    group.finish();
}

//criterion_group!(benches, bench_compare);
criterion_group!(benches, bench_macor);
criterion_main!(benches);
