#![feature(box_syntax)]

use std::path::PathBuf;

use clap::Parser;
use macor::verifier::{Verification, Verifier};

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
enum Args {
    /// Prints the constraints for session of the given protocol
    Sessions {
        /// Number of sessions
        #[clap(short, long, value_parser)]
        num_sessions: u32,

        /// The path to the protocol src
        #[clap(value_parser)]
        file: PathBuf,
    },
    /// Attempts to find an attack on the given protocol
    Verify {
        /// Number of sessions
        #[clap(short, long, value_parser)]
        num_sessions: u32,

        /// The path to the protocol src
        #[clap(value_parser)]
        file: PathBuf,
    },
    Pretty {
        /// The path to the protocol src
        #[clap(value_parser)]
        file: PathBuf,
    },
}

fn main() -> miette::Result<()> {
    miette::set_hook(box |_| {
        box miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .context_lines(4)
            .force_graphical(true)
            .build()
    })?;

    let args = Args::parse();

    match args {
        Args::Sessions { num_sessions, file } => {
            let src = std::fs::read_to_string(file).unwrap();
            Verifier::with_num_sessions(num_sessions).print_sessions(&src)?;
        }
        Args::Verify { num_sessions, file } => {
            let src = std::fs::read_to_string(file).unwrap();
            match Verifier::with_num_sessions(num_sessions).verify(&src)? {
                Verification::Attack(trace) => {
                    println!("Found attack:");
                    println!("{}", trace);
                }
                Verification::NoAttack => println!("No attack found!"),
            }
        }
        Args::Pretty { file } => {
            let src = std::fs::read_to_string(file).unwrap();
            print!("{}", macor_fmt::prettify(src).unwrap());
        }
    }

    Ok(())
}
