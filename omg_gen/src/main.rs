use clap::Parser;
use itertools::Itertools;
use macor::protocol::{Direction, Func, Protocol, ProtocolAgent, Term};
use macor_parse::ast::TypesKey;
use omg_gen::Omg;
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

const PRELUDE: &str = r#"
#![allow(non_camel_case_types, non_snake_case)]

use anyhow::{Result, bail};
use std::collections::HashMap;
use omg::{Effect, DynTerm, Base, Func, Pattern};

"#;

fn main() -> anyhow::Result<()> {
    use std::fmt::Write;

    let cli = Cli::parse();

    match cli {
        Cli::Generate { out, path } => {
            let src = std::fs::read_to_string(&path)?;

            let doc = macor_parse::parse_document(&src)?;

            let omg = Omg::new(&src, doc)?;

            let mut buf = String::new();

            writeln!(buf, "{PRELUDE}")?;

            writeln!(buf)?;

            writeln!(buf, "pub trait Terms: Base {{")?;
            for (name, (args, _ret)) in &omg.protocol.functions {
                // TODO: Only include public functions
                writeln!(
                    buf,
                    "\ttype User_{}: serde::Serialize + serde::de::DeserializeOwned;",
                    name
                )?;
                writeln!(buf, "\t{};", omg.fn_signature(name.as_str(), args))?;
            }
            writeln!(buf, "}}")?;

            writeln!(buf, "impl Terms for () {{")?;
            for (name, (args, _ret)) in &omg.protocol.functions {
                writeln!(buf, "\ttype User_{} = ();", name)?;
                writeln!(buf, "\t{} {{ () }}", omg.fn_signature(name.as_str(), args))?;
            }
            writeln!(buf, "}}")?;

            writeln!(buf, "impl Terms for Pattern {{")?;
            for (name, (args, _ret)) in &omg.protocol.functions {
                writeln!(buf, "\ttype User_{} = DynTerm<Self>;", name)?;
                writeln!(
                    buf,
                    "\t{} {{ todo!() }}",
                    omg.fn_signature(name.as_str(), args)
                )?;
            }
            writeln!(buf, "}}")?;

            writeln!(
                buf,
                "#[derive(Debug, serde::Serialize, serde::Deserialize)]"
            )?;
            writeln!(buf, "#[serde(bound = \"\")]")?;
            writeln!(buf, "pub enum Message<T: Terms> {{")?;
            for agent in &omg.protocol.agents {
                writeln!(buf, "\tAgent_{}(Agent_{}_Msg<T>),", agent.name, agent.name)?;
            }
            writeln!(buf, "}}")?;

            for agent in &omg.protocol.agents {
                writeln!(buf, "{}", omg.generate_for_agent(agent)?)?;
            }

            let dolev_yao_src = include_str!("./dolev_yao.rs");

            let dolev_yao_patched = dolev_yao_src
                .replace(
                    "impl<T: Base> Knowledge<T>",
                    "impl<T: super::Terms> Knowledge<T>",
                )
                .replace(
                    r#"todo!("TEMPLATE FOR FUNCTION COMPOSITION")"#,
                    &format!(
                        "match func.as_str() {{ {} _ => todo!() }}",
                        omg.protocol
                            .functions
                            .iter()
                            .map(|(name, (args, _))| format!("\"{name}\" => {{ continue; }},"))
                            .format("\n")
                    ),
                );

            writeln!(buf, "mod dolev_yao {{ {dolev_yao_patched} }}")?;

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
                write!(stdin, "{buf}")?;
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
