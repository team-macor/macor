use std::{path::PathBuf, process::Command};

// pub struct TimingResult {
//     pub ofmc_time: Duration,
//     pub macor_time: Duration,
// }

pub struct AttackResult {
    pub ofmc_result: bool,
    pub macor_result: bool,
}
pub struct Evaluation {
    pub ofmc_path: PathBuf,
    pub macor_path: PathBuf,
    pub protocol: PathBuf,
    pub num_session: usize,
}

impl Evaluation {
    // pub fn evaluate_time(&self, num_samples: u32) -> TimingResult {
    //     let mut ofmc_time = Duration::ZERO;
    //     let mut macor_time = Duration::ZERO;

    //     for _ in 0..num_samples {
    //         let now = Instant::now();
    //         // ofmc --numSess 2 path/to/my/Protocol.AnB
    //         Command::new(&self.ofmc_path)
    //             .arg("--numSess")
    //             .arg(self.num_session.to_string())
    //             .arg(&self.protocol)
    //             .output()
    //             .expect("Could not find path for OFMC");
    //         ofmc_time += now.elapsed();

    //         let now = Instant::now();
    //         // macor verify -n 2 path/to/my/Protocol.AnB
    //         Command::new(&self.macor_path)
    //             .arg("verify")
    //             .arg("-n")
    //             .arg(self.num_session.to_string())
    //             .arg(&self.protocol)
    //             .output()
    //             .expect("Could not find path for MACOR");
    //         macor_time += now.elapsed();
    //     }
    //     dbg!("macor time: {:?}, ofmc_time: {:?}", macor_time/num_samples, ofmc_time/num_samples );
    //     TimingResult {
    //         ofmc_time: ofmc_time / num_samples,
    //         macor_time: macor_time / num_samples,
    //     }
    // }

    fn evaluate_output(&self) -> AttackResult {
        let ofmc_result = Command::new(&self.ofmc_path)
            .arg("--numSess")
            .arg(self.num_session.to_string())
            .arg(&self.protocol)
            .output()
            .expect("Could not find path for OFMC");

        let ofmc_result = String::from_utf8(ofmc_result.stdout)
            .unwrap()
            .contains("NO_ATTACK_FOUND");

        let macor_result = Command::new(&self.macor_path)
            .arg("verify")
            .arg("-n")
            .arg(self.num_session.to_string())
            .arg(&self.protocol)
            .output()
            .expect("Could not find path for MACOR");

        let macor_result = String::from_utf8(macor_result.stdout)
            .unwrap()
            .contains("No attack found!");

        AttackResult {
            ofmc_result,
            macor_result,
        }
    }
}

#[test]
fn compare_result() {
    Evaluation {
        ofmc_path: "ofmc".into(),
        macor_path: "macor".into(),
        protocol: "../example_programs/KeyEx1.AnB".into(),
        num_session: 1,
    }
    .evaluate_output();
}

// #[test]
// fn compare_time() {
//     Evaluation {
//         ofmc_path: "ofmc".into(),
//         macor_path: "macor".into(),
//         protocol:  "../example_programs/KeyEx1.AnB".into(),
//         num_session: 1,
//     }.evaluate_time(10);
// }
