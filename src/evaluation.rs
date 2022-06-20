use std::{
    path::PathBuf,
    process::Command,
    time::{Duration, Instant},
};

pub struct TimingResult {
    pub ofmc_time: Duration,
    pub macor_time: Duration,
}

pub struct Evaluation {
    pub ofmc_path: PathBuf,
    pub macor_path: PathBuf,
    pub protocol: PathBuf,
    pub num_session: usize,
}

impl Evaluation {
    pub fn evaluate_time(&self, num_samples: u32) -> TimingResult {
        let mut ofmc_time = Duration::ZERO;
        let mut macor_time = Duration::ZERO;

        for _ in 0..num_samples {
            let now = Instant::now();
            // ofmc --numSess 2 path/to/my/Protocol.AnB
            let ofmc = Command::new(&self.ofmc_path)
                .arg("--numSess")
                .arg(self.num_session.to_string())
                .arg(&self.protocol)
                .output()
                .expect("Could not find path for OFMC");
            ofmc_time += now.elapsed();

            let now = Instant::now();
            // macor verify -n 2 path/to/my/Protocol.AnB
            let macor = Command::new(&self.macor_path)
                .arg("verify")
                .arg("-n")
                .arg(self.num_session.to_string())
                .arg(&self.protocol)
                .output()
                .expect("Could not find path for MACOR");
            macor_time += now.elapsed();
        }

        TimingResult {
            ofmc_time: ofmc_time / num_samples,
            macor_time: macor_time / num_samples,
        }
    }

    fn evaluate_output(&self) {
        let ofmc = Command::new(&self.ofmc_path)
            .arg("--numSess")
            .arg(self.num_session.to_string())
            .arg(&self.protocol)
            .output()
            .expect("Could not find path for OFMC");

        let macor = Command::new(&self.macor_path)
            .arg("verify")
            .arg("-n")
            .arg(self.num_session.to_string())
            .arg(&self.protocol)
            .output()
            .expect("Could not find path for MACOR");
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::messages::{Converter, Knowledge};

    // TODO: needs to handle message with commas like {|A, B|} as 1 message (call parser)
    macro_rules! scenario {
        (knowledge : $k:tt ; goals : $($g:expr),+ ;) => {
            let knowledge: Vec<_> = $k.split(",").into_iter().map(|s| s.to_string()).collect();
            let goals = vec![$(stringify!($g)),+];

            let mut unifier = Default::default();
            let mut mapper = Default::default();
            let mut converter = Converter::new(&mut unifier, &mut mapper);

            let mut knowledge = Knowledge(
                knowledge
                    .iter()
                    .map(|k| converter.register_ast_message(macor_parse::parse_message(k).unwrap().into()))
                    .collect_vec(),
            );

            let goals = goals
                .iter()
                .map(|g| {
                    if let Some(g) = g.strip_prefix('!') {
                        (
                            false,
                            converter.register_ast_message(macor_parse::parse_message(g).unwrap().into()),
                        )
                    } else {
                        (
                            true,
                            converter.register_ast_message(macor_parse::parse_message(g).unwrap().into()),
                        )
                    }
                })
                .collect_vec();

            crate::dolev_yao::augment_knowledge(&mut knowledge, &mut unifier);
            for (expected, goal) in goals {
                assert_eq!(
                    expected,
                    crate::dolev_yao::can_derive(&knowledge, goal, &mut unifier)
                );
            }

        };
    }
    #[test]
    fn my_test() {
        scenario! {
            knowledge: "k_1, k_2, f";
            goals: f(k_1, k_2), !h(k_1, k_2), { k_1 }(f(f(k_1), k_2));
        };
    }

    #[test]
    fn requires_augment() {
        scenario! {
            knowledge: "key, { data }(inv(key))";
            goals: data;
        };
    }

    #[test]
    fn derive_sym_enc() {
        scenario! {
            knowledge: "key, {| data |}(key)";
            goals: data;
        };
    }
}
