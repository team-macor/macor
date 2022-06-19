#![feature(box_syntax)]

use std::collections::VecDeque;

use itertools::Itertools;
use macor::{
    messages::{self, Execution},
    protocol::{Protocol, SessionId},
};
use rayon::iter::{IntoParallelRefIterator, ParallelBridge, ParallelExtend, ParallelIterator};

fn main() -> miette::Result<()> {
    miette::set_hook(box |_| {
        box miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .context_lines(4)
            .force_graphical(true)
            .build()
    })?;

    for arg in std::env::args().skip(1) {
        let src = std::fs::read_to_string(arg).unwrap();
        let parsed = macor::parse_document(&src)?;

        let protocol =
            Protocol::new(src.clone(), parsed).map_err(|x| x.first().cloned().unwrap())?;

        let mut unifier = Default::default();
        let sessions: std::sync::Arc<Vec<_>> = (0..1)
            .map(|i| messages::Session::new(&protocol, SessionId(i), &mut unifier))
            .collect_vec()
            .into();

        let start = std::time::Instant::now();
        let mut num_executions = 0;

        let parallel = false;

        if parallel {
            let mut list_a = vec![Execution::new(unifier.clone(), sessions.clone())];
            let mut list_b = vec![];

            while !list_a.is_empty() {
                if let Some(execution) = list_a
                    .iter_mut()
                    .find_map(|execution| execution.has_compromised_secrets().then(|| execution))
                {
                    execution.print_trace();
                    todo!("pretty print attack");
                }
                list_b.clear();
                list_b.par_extend(
                    list_a
                        .par_iter()
                        .flat_map(|exe| exe.possible_next().par_bridge()),
                );
                list_a.clear();

                num_executions += list_b.len() as u32;

                println!(
                    "solcreme i mit øje 😎👌: {} executions, {:?}/execution",
                    num_executions,
                    start.elapsed() / num_executions
                );

                std::mem::swap(&mut list_a, &mut list_b);
            }
        } else {
            let num_samples = 1;

            for _ in 0..num_samples {
                let mut worklist = VecDeque::new();

                worklist.push_back(Execution::new(unifier.clone(), sessions.clone()));

                while let Some(mut execution) = worklist.pop_front() {
                    num_executions += 1;
                    let mut nexts = execution.possible_next().peekable();
                    if nexts.peek().is_none() {
                        drop(nexts);
                        execution.print_trace();
                    } else {
                        for mut next in nexts {
                            if next.has_compromised_secrets() {
                                next.print_trace();
                                todo!("pretty print attack");
                            }

                            worklist.push_back(next);
                        }
                    }
                }
            }

            println!(
                "solcreme i mit øje 😎👌: {} executions, {:?}/execution",
                num_executions,
                start.elapsed() / num_executions
            )
        }
    }

    Ok(())
}
