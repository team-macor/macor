use std::collections::VecDeque;

use itertools::Itertools;

use crate::{
    messages::{self, Converter, Execution, Unifier},
    protocol::{Func, Protocol, SessionId},
};
use rayon::iter::{IntoParallelRefIterator, ParallelBridge, ParallelExtend, ParallelIterator};

enum Participant {
    Intruder,
    Actor(String),
}

impl std::fmt::Display for Participant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Participant::Intruder => write!(f, "i"),
            Participant::Actor(actor) => write!(f, "{}", actor),
        }
    }
}

enum Message {
    Variable(String),
    Agent(String),
    Constant(String),
    Composition(Func<String>, Vec<Message>),
    Tuple(Vec<Message>),
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Message::Variable(inner) | Message::Agent(inner) | Message::Constant(inner) => {
                write!(f, "{}", inner)
            }
            Message::Composition(func, args) => match func {
                Func::SymEnc => write!(f, "{{|{}|}}({})", args[0], args[1]),
                Func::AsymEnc => write!(f, "{{{}}}({})", args[0], args[1]),
                Func::Exp => write!(f, "exp({})", args.iter().format(", ")),
                Func::Inv => write!(f, "inv({})", args.iter().format(", ")),
                Func::User(name) => write!(f, "{}({})", name, args.iter().format(", ")),
            },
            Message::Tuple(args) => write!(f, "<{}>", args.iter().format(", ")),
        }
    }
}

impl Message {
    fn from_message_id(msg: messages::MessageId, unifier: &mut Unifier) -> Message {
        match unifier.probe_value(msg) {
            messages::Message::Variable(var) => Message::Variable(var.unwrap()),
            messages::Message::Agent(agent) => Message::Agent(agent.unwrap()),
            messages::Message::Constant(con) => Message::Constant(con.1.unwrap().to_string()),
            messages::Message::Composition(func, args) => {
                let func = match func {
                    Func::User(id) => Func::User(id.1.unwrap().to_string()),
                    Func::SymEnc => Func::SymEnc,
                    Func::AsymEnc => Func::AsymEnc,
                    Func::Exp => Func::Exp,
                    Func::Inv => Func::Inv,
                };

                Message::Composition(
                    func,
                    args.into_iter()
                        .map(|msg| Message::from_message_id(msg, unifier))
                        .collect(),
                )
            }
            messages::Message::Tuple(args) => Message::Tuple(
                args.into_iter()
                    .map(|msg| Message::from_message_id(msg, unifier))
                    .collect(),
            ),
        }
    }
}

struct TraceEntry {
    sender: Participant,
    receiver: Participant,
    messages: Vec<Message>,
}

impl std::fmt::Display for TraceEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}: {}",
            self.sender,
            self.receiver,
            self.messages.iter().format(", ")
        )
    }
}

impl TraceEntry {
    fn from_messages_trace(entry: &messages::TraceEntry, unifier: &mut Unifier) -> Self {
        let sender = match &entry.sender {
            Some(sender) => match unifier.probe_value(sender.1) {
                messages::Message::Agent(a) => Participant::Actor(a.unwrap()),
                messages::Message::Constant(s) => Participant::Actor(s.1.unwrap().to_string()),
                u => unreachable!("Sender must be agent (or constant agents), was {:?}", u),
            },
            None => Participant::Intruder,
        };

        let receiver = match &entry.receiver {
            Some(receiver) => match unifier.probe_value(receiver.1) {
                messages::Message::Agent(a) => Participant::Actor(a.unwrap()),
                messages::Message::Constant(s) => Participant::Actor(s.1.unwrap().to_string()),
                u => unreachable!("Receiver must be agent (or constant agents), was {:?}", u),
            },
            None => Participant::Intruder,
        };

        let messages = entry
            .messages
            .iter()
            .map(|msg| Message::from_message_id(*msg, unifier))
            .collect();

        Self {
            sender,
            receiver,
            messages,
        }
    }
}

pub struct Trace(Vec<TraceEntry>);

impl std::fmt::Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for entry in &self.0 {
            writeln!(f, "{}", entry)?;
        }
        Ok(())

        // println!("---------------------------");
        // for t in &self.trace {
        //     println!(
        //         "[{}] {}->{}: {:?}",
        //         t.session.0,
        //         t.sender
        //             .as_ref()
        //             .map(|s| format!("{:?}", self.unifier.resolve_full(s.1)))
        //             .unwrap_or_else(|| "?".to_string()),
        //         t.receiver
        //             .as_ref()
        //             .map(|r| format!("{:?}", self.unifier.resolve_full(r.1)))
        //             .unwrap_or_else(|| "?".to_string()),
        //         t.messages
        //             .iter()
        //             .map(|&msg| self.unifier.resolve_full(msg))
        //             .format(", ")
        //     );
        // }
    }
}

pub enum Verification {
    Attack(Trace),
    NoAttack,
}

pub struct Verifier {
    num_sessions: u32,
    parallel: bool,
}

impl Verifier {
    pub fn with_num_sessions(num_sessions: u32) -> Self {
        Self {
            num_sessions,
            parallel: false,
        }
    }

    pub fn set_parallel(mut self, parallel: bool) -> Self {
        self.parallel = parallel;
        self
    }

    pub fn print_sessions(&self, protocol: &str) -> miette::Result<()> {
        let parsed = crate::parse_document(protocol)?;

        let protocol =
            Protocol::new(protocol.to_string(), parsed).map_err(|x| x.first().cloned().unwrap())?;

        let mut unifier = Unifier::default();
        let mut mapper = Default::default();
        let mut converter = Converter::new(&mut unifier, &mut mapper);
        let sessions: std::sync::Arc<Vec<_>> = (0..self.num_sessions)
            .map(|i| messages::Session::new(&protocol, SessionId(i), &mut converter))
            .collect_vec()
            .into();

        Execution::new(&protocol, mapper, unifier, sessions).print_sessions();

        Ok(())
    }

    pub fn verify(self, protocol: &str) -> miette::Result<Verification> {
        let parsed = crate::parse_document(protocol)?;

        let protocol =
            Protocol::new(protocol.to_string(), parsed).map_err(|x| x.first().cloned().unwrap())?;

        let mut unifier = Default::default();
        let mut mapper = Default::default();
        let mut converter = Converter::new(&mut unifier, &mut mapper);
        let sessions: std::sync::Arc<Vec<_>> = (0..self.num_sessions)
            .map(|i| messages::Session::new(&protocol, SessionId(i), &mut converter))
            .collect_vec()
            .into();

        let start = std::time::Instant::now();
        let mut num_executions = 0;

        if self.parallel {
            let mut list_a = vec![Execution::new(
                &protocol,
                mapper.clone(),
                unifier.clone(),
                sessions,
            )];
            let mut list_b = vec![];

            while !list_a.is_empty() {
                if let Some(execution) = list_a
                    .iter_mut()
                    .find_map(|execution| execution.has_compromised_secrets().then(|| execution))
                {
                    return Ok(Verification::Attack(Trace(
                        execution
                            .trace
                            .iter()
                            .map(|entry| {
                                TraceEntry::from_messages_trace(entry, &mut execution.unifier)
                            })
                            .collect(),
                    )));
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

                worklist.push_back(Execution::new(
                    &protocol,
                    mapper.clone(),
                    unifier.clone(),
                    sessions.clone(),
                ));

                while let Some(mut execution) = worklist.pop_front() {
                    // execution.print_trace();
                    num_executions += 1;
                    let mut nexts = execution.possible_next().peekable();
                    if nexts.peek().is_none() {
                        drop(nexts);
                        // execution.print_trace();
                    } else {
                        for mut next in nexts {
                            if next.has_compromised_secrets() {
                                return Ok(Verification::Attack(Trace(
                                    next.trace
                                        .iter()
                                        .map(|entry| {
                                            TraceEntry::from_messages_trace(
                                                entry,
                                                &mut next.unifier,
                                            )
                                        })
                                        .collect(),
                                )));
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

        Ok(Verification::NoAttack)
    }
}