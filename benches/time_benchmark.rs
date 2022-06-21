use criterion::{criterion_group, criterion_main, Criterion};
use macor::protocol::{Protocol};
use std::{
    path::PathBuf,
    process::Command,
    time::{Duration, Instant},
};

pub fn bench_programs(c: &mut Criterion){
    let ofmc_path: PathBuf = "/Users/rebeccaviuff/Desktop/rust/ofmc-mac".into();
    let macor_path: PathBuf = "/Users/rebeccaviuff/Documents/UNI_ALL/Uni-MSc/2022-Summer/Rust-Special-Course/macor/target/release/macor".into();
    let protocol: PathBuf =  "/Users/rebeccaviuff/Documents/UNI_ALL/Uni-MSc/2022-Summer/Rust-Special-Course/macor/example_programs/KeyEx1.AnB".into();
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
            for _ in 0..num_samples{
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


criterion_group!(benches, bench_programs);
criterion_main!(benches);

