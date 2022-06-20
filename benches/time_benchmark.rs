// use criterion::{criterion_group, criterion_main, Criterion};
// use std::{
//     path::PathBuf,
//     process::Command,
//     time::{Duration, Instant},
// };

// pub struct Evaluation {
//     pub ofmc_path: PathBuf,
//     pub macor_path: PathBuf,
//     pub protocol: PathBuf,
//     pub num_session: usize,
// }

// impl Evaluation {

// pub fn bench_evalutate_time(&self, c: &mut Criterion, num_samples: u32 ){
//     let mut group = c.benchmark_group("Programs");
//     let mut ofmc_time = Duration::ZERO;
//     let mut macor_time = Duration::ZERO;

//     group.bench_function("OFMC", |b| {
//         b.iter(|| {
//             for _ in 0..num_samples {
//                 let now = Instant::now();
//                 // ofmc --numSess 2 path/to/my/Protocol.AnB
//                 let ofmc = Command::new(&self.ofmc_path)
//                     .arg("--numSess")
//                     .arg(self.num_session.to_string())
//                     .arg(&self.protocol)
//                     .output()
//                     .expect("Could not find path for OFMC");
//                 ofmc_time += now.elapsed();
//                 let o_time = ofmc_time / num_samples;
//             }
//         })
//     });

//     group.bench_function("MACOR", |b| {
//         b.iter(|| {
//             for _ in 0..num_samples{
//                 let now = Instant::now();
//                 // macor verify -n 2 path/to/my/Protocol.AnB
//                 let macor = Command::new(&self.macor_path)
//                     .arg("verify")
//                     .arg("-n")
//                     .arg(self.num_session.to_string())
//                     .arg(&self.protocol)
//                     .output()
//                     .expect("Could not find path for MACOR");
//                 macor_time += now.elapsed();
//                 let m_time = macor_time / num_samples;
//                 }
//         })
//     });
//     group.finish();
// }
// }

// criterion_group!(benches, Evaluation{num}.bench_evalutate_time);
// criterion_main!(benches);
