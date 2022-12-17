use clap::Parser;
use std::path::PathBuf;

#[derive(Debug, Parser)]
pub enum Cli {
    /// Generate a Rust file representing an implementation of the model
    Generate {
        #[clap(short, long)]
        out: Option<PathBuf>,
        path: PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli::Generate { out, path } => {
            let src = std::fs::read_to_string(&path)?;
            let doc = macor_parse::parse_document(&src)?;
            let output = omg_gen::v2::generate(&src, doc)?;

            let output = {
                use std::{
                    io::Write,
                    process::{Command, Stdio},
                };
                let mut child = Command::new("rustfmt")
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()?;
                let mut stdin = child.stdin.take().unwrap();
                write!(stdin, "{output}")?;
                drop(stdin);
                child.wait_with_output()?
            };
            let output = String::from_utf8(output.stdout)?;

            match out {
                Some(out) => std::fs::write(out, output)?,
                None => println!("{output:?}"),
            }
        }
    }

    Ok(())
}
