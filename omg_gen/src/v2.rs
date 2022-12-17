use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use macor::protocol::{
    AgentName, Direction, Func, Message, MessageNr, MessagePattern, Protocol, ProtocolAgent, Term,
};
use macor_parse::ast::{Document, Ident};

use crate::output::term_to_rust_ty;

struct Knowledge {
    initial: Vec<(usize, InfoId)>,
    info: Vec<Info>,
}

#[derive(Debug, Clone)]
enum Validation {
    Trust,
    CheckAgainstKnowledge { id: InfoId },
}

#[derive(Debug)]
enum ExpansionResult {
    Failed,
    NothingToDo,
    Success {
        requires: Vec<InfoId>,
        produces: Vec<Term>,
    },
}

impl Knowledge {
    fn try_expand(&self, t: &Term) -> ExpansionResult {
        match t {
            Term::Variable(_) => ExpansionResult::NothingToDo,
            Term::Constant(_) => ExpansionResult::NothingToDo,
            Term::Composition { func, args } => match func {
                Func::SymEnc => {
                    let body = &args[0];
                    let key = &args[1];

                    let deps = if let Some(deps) = self.find_construction_deps(key) {
                        deps
                    } else {
                        return ExpansionResult::Failed;
                    };

                    ExpansionResult::Success {
                        requires: deps,
                        produces: vec![body.clone()],
                    }
                }
                Func::AsymEnc => todo!(),
                Func::Exp => todo!(),
                Func::Inv => todo!(),
                Func::User(_) => todo!(),
            },
            Term::Tuple(ts) => ExpansionResult::Success {
                requires: vec![],
                produces: ts.clone(),
            },
        }
    }

    fn construct(&mut self, nr: MessageNr, t: &Term) -> InfoId {
        if let Some(i) = self.info.iter().find(|i| &i.term == t) {
            return i.id;
        }

        match t {
            Term::Variable(_) => todo!("cannot construct a variable we do not initiate"),
            Term::Constant(_) => todo!("cannot construct a constant we do not initiate"),
            Term::Composition { func, args } => match func {
                Func::SymEnc => {
                    let body = self.construct(nr, &args[0]);
                    let key = self.construct(nr, &args[1]);

                    self.retrieve(
                        TermOrigin::SymEnc {
                            for_msg: nr,
                            body,
                            key,
                        },
                        t.clone(),
                    )
                }
                Func::AsymEnc => todo!(),
                Func::Exp => todo!(),
                Func::Inv => todo!(),
                Func::User(_) => todo!(),
            },
            Term::Tuple(ts) => {
                let from = ts.iter().map(|t| self.construct(nr, t)).collect();
                self.retrieve(TermOrigin::CreateTuple { for_msg: nr, from }, t.clone())
            }
        }
    }
    fn find_construction_deps(&self, t: &Term) -> Option<Vec<InfoId>> {
        if let Some(i) = self.info.iter().find(|i| &i.term == t) {
            return Some(vec![i.id]);
        }

        match t {
            Term::Variable(_) => None,
            Term::Constant(_) => None,
            Term::Composition { func, args } => match func {
                Func::SymEnc => {
                    let body = &args[0];
                    let key = &args[1];

                    let mut a = self.find_construction_deps(body)?;
                    let b = self.find_construction_deps(key)?;
                    a.extend_from_slice(&b);
                    Some(a)
                }
                Func::AsymEnc => todo!(),
                Func::Exp => todo!(),
                Func::Inv => todo!(),
                Func::User(f) => {
                    // TODO
                    None
                }
            },
            Term::Tuple(ts) => {
                let mut total = vec![];

                for t in ts {
                    if let Some(deps) = self.find_construction_deps(t) {
                        total.extend_from_slice(&deps);
                    } else {
                        return None;
                    }
                }

                Some(total)
            }
        }
    }
    fn find_validation_deps(&self, t: &Term) -> Option<Validation> {
        if let Some(i) = self.info.iter().find(|i| &i.term == t) {
            return Some(Validation::CheckAgainstKnowledge { id: i.id });
        }

        match t {
            Term::Variable(_) => Some(Validation::Trust),
            Term::Constant(_) => todo!(),
            Term::Composition { func, args } => match func {
                // NOTE: We don't validate symmetric encryptions directly,
                // instead we decompose them, and validate their contents.
                Func::SymEnc => None,
                Func::AsymEnc => todo!(),
                Func::Exp => todo!(),
                Func::Inv => todo!(),
                Func::User(_) => None,
            },
            Term::Tuple(_) => None,
        }
    }
    fn retrieve_more(&mut self, origin: TermOrigin, ts: Vec<Term>) {
        for t in ts {
            self.retrieve(origin.clone(), t);
        }
    }
    fn retrieve(&mut self, origin: TermOrigin, t: Term) -> InfoId {
        let id = InfoId(self.info.len());
        let validation = self.find_validation_deps(&t).unwrap_or(Validation::Trust);
        self.info.push(Info {
            id,
            term: t,
            origin: origin.clone(),
            expansion: match &origin {
                TermOrigin::Initiated { .. }
                | TermOrigin::CreateTuple { .. }
                | TermOrigin::SymEnc { .. } => Expansion::DontExpand,
                _ => Expansion::None,
            },
            validation,
        });
        id
    }

    pub fn augment_knowledge(&mut self, msg_nr: MessageNr) {
        for i in self.info.clone() {
            match &i.expansion {
                Expansion::None => {}
                Expansion::ExpandedAt { .. } | Expansion::DontExpand => continue,
            }

            match &i.term {
                Term::Variable(_) => {}
                Term::Constant(_) => {}
                Term::Composition { func, args } => match func {
                    Func::SymEnc => match self.try_expand(&i.term) {
                        ExpansionResult::Failed => {}
                        ExpansionResult::NothingToDo => {}
                        ExpansionResult::Success { requires, produces } => {
                            self.info[i.id.0].expansion = Expansion::ExpandedAt {
                                nr: msg_nr,
                                depends_on: requires.clone(),
                            };

                            self.retrieve_more(
                                TermOrigin::DecryptSymmetric {
                                    term: i.id,
                                    key: requires[0],
                                    after_msg: msg_nr,
                                },
                                produces,
                            );
                        }
                    },
                    Func::AsymEnc => todo!(),
                    Func::Exp => todo!(),
                    Func::Inv => todo!(),
                    Func::User(_) => {}
                },
                Term::Tuple(ts) => {
                    self.info[i.id.0].expansion = Expansion::ExpandedAt {
                        nr: msg_nr,
                        depends_on: vec![],
                    };

                    for (index, t) in ts.iter().enumerate() {
                        self.retrieve(
                            TermOrigin::Decompose {
                                from: i.id,
                                index,
                                after_msg: msg_nr,
                            },
                            t.clone(),
                        );
                    }
                }
            }
        }
    }

    fn generate_instructions(&self, msg: &MessagePattern, nr: MessageNr) -> Vec<Instruction> {
        let mut instructions = vec![];

        for i in &self.info {
            let validate = |ins: &mut Vec<_>| match &i.validation {
                Validation::Trust => {}
                &Validation::CheckAgainstKnowledge { id } => ins.push(Instruction::Compare {
                    trusted: id,
                    new: i.id,
                }),
            };

            match &i.origin {
                TermOrigin::InitialKnowledge => {}
                &TermOrigin::Initiated { ty, msg_nr } => {
                    if nr == msg_nr {
                        instructions.push(Instruction::Generate(ty, i.id));
                    }
                }
                &TermOrigin::Received { msg_nr, index } => {
                    if nr == msg_nr {
                        instructions.push(Instruction::Retrieve { index, id: i.id });
                        validate(&mut instructions);
                    }
                }
                &TermOrigin::DecryptSymmetric {
                    term,
                    key,
                    after_msg,
                } => {
                    if after_msg == nr {
                        instructions.push(Instruction::DecryptSymmetric {
                            term,
                            key,
                            into: i.id,
                        });
                        validate(&mut instructions);
                    }
                }
                TermOrigin::DecryptAsymmetric {
                    term,
                    key,
                    after_msg,
                } => todo!(),
                &TermOrigin::Decompose {
                    from,
                    index,
                    after_msg,
                } => {
                    if nr == after_msg {
                        instructions.push(Instruction::Extract {
                            from,
                            index,
                            into: i.id,
                        });
                        validate(&mut instructions);
                    }
                }
                TermOrigin::CreateTuple { for_msg, from } => {
                    if nr == *for_msg {
                        instructions.push(Instruction::CreateTuple {
                            from: from.clone(),
                            into: i.id,
                        });
                    }
                }
                &TermOrigin::SymEnc { for_msg, body, key } => {
                    if nr == for_msg {
                        instructions.push(Instruction::SymEnc {
                            body,
                            key,
                            into: i.id,
                        });
                    }
                }
            }
        }

        instructions
    }
}

#[derive(Debug, Clone)]
enum Expansion {
    None,
    DontExpand,
    ExpandedAt {
        nr: MessageNr,
        depends_on: Vec<InfoId>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct InfoId(usize);
impl std::fmt::Display for InfoId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "i{}", self.0)
    }
}

#[derive(Debug, Clone)]
struct Info {
    id: InfoId,
    term: Term,
    origin: TermOrigin,
    expansion: Expansion,
    validation: Validation,
}

#[derive(Debug, Clone)]
enum TermOrigin {
    InitialKnowledge,
    Initiated {
        ty: GenerateTy,
        msg_nr: MessageNr,
    },
    Received {
        msg_nr: MessageNr,
        index: usize,
    },
    DecryptSymmetric {
        term: InfoId,
        key: InfoId,
        after_msg: MessageNr,
    },
    DecryptAsymmetric {
        term: InfoId,
        key: InfoId,
        after_msg: MessageNr,
    },
    Decompose {
        from: InfoId,
        index: usize,
        after_msg: MessageNr,
    },
    CreateTuple {
        for_msg: MessageNr,
        from: Vec<InfoId>,
    },
    SymEnc {
        for_msg: MessageNr,
        body: InfoId,
        key: InfoId,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum GenerateTy {
    Nonce,
    SymKey,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Retrieve {
        index: usize,
        id: InfoId,
    },
    Generate(GenerateTy, InfoId),
    DecryptSymmetric {
        term: InfoId,
        key: InfoId,
        into: InfoId,
    },
    Compare {
        trusted: InfoId,
        new: InfoId,
    },
    Extract {
        from: InfoId,
        index: usize,
        into: InfoId,
    },
    CreateTuple {
        from: Vec<InfoId>,
        into: InfoId,
    },
    SymEnc {
        body: InfoId,
        key: InfoId,
        into: InfoId,
    },
    ComputeInitialKnowledgeFrom {
        number_given: usize,
        into: Vec<InfoId>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessageId {
    Initialize,
    Nr(MessageNr),
}

impl std::fmt::Display for MessageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageId::Initialize => write!(f, "Initiate"),
            MessageId::Nr(nr) => write!(f, "{nr}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InitializeInitialKnowledge {
    pub from_agent: AgentName,
    pub from_types: Vec<Term>,
    pub from_msg: MessageNr,
}

#[derive(Debug, Clone)]
pub struct AgentModel {
    pub name: AgentName,
    pub registers: Vec<(InfoId, Term)>,
    pub stage_names: Vec<String>,
    pub listen_ports: Vec<AgentName>,
    pub initial_knowledge: Vec<(InfoId, Term)>,
    pub initialize_knowledge_from_first_message: Option<InitializeInitialKnowledge>,
    pub ingoing: Vec<(MessageId, Vec<Term>)>,
    pub outgoing: Vec<(MessageNr, Vec<Term>)>,
    pub messages: Vec<MessageInstructions>,
}

// #[derive(Debug, Clone)]
// pub enum MessageEnd {
//     Send {
//         nr: MessageNr,
//         body: Vec<InfoId>,
//         to: InfoId,
//     },
//     Terminate,
// }

#[derive(Debug, Clone)]
pub enum MessageThingy {
    Initiate(Vec<Term>),
    Pattern(MessagePattern),
}

#[derive(Debug, Clone)]
pub enum SendMessage {
    Nothing,
    Send {
        nr: MessageNr,
        body: Vec<InfoId>,
        connect: bool,
        to: (AgentName, InfoId),
    },
}

#[derive(Debug, Clone)]
pub enum NextAction {
    Terminate,
    ListenOn(AgentName, InfoId),
    RecvFrom(AgentName, InfoId),
}

#[derive(Debug, Clone)]
pub struct Response {
    pub save_connection_as: Option<AgentName>,
    pub send: SendMessage,
    pub action: NextAction,
}

#[derive(Debug, Clone)]
pub struct MessageInstructions {
    pub id: MessageId,
    pub match_on: MessageThingy,
    pub instructions: Vec<Instruction>,
    pub response: Response,
}

#[derive(Debug, Clone)]
pub struct CustomType {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct InstanceInfo {
    pub types: Vec<CustomType>,
}

#[derive(Debug, Clone)]
pub struct ProtocolModel {
    pub instance: InstanceInfo,
    pub messages: Vec<(MessageId, Vec<Term>)>,
    pub agents: Vec<AgentModel>,
}

pub fn prepate_model(src: &str, doc: Document<&str>) -> anyhow::Result<ProtocolModel> {
    let protocol =
        Protocol::new(src.to_string(), doc.clone()).map_err(|x| x.first().cloned().unwrap())?;

    let mut agents = vec![];

    for agent in &protocol.agents {
        let (initial, info) = agent
            .initial_knowledge
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, t)| {
                (
                    (i, InfoId(i)),
                    Info {
                        id: InfoId(i),
                        term: t,
                        origin: TermOrigin::InitialKnowledge,
                        expansion: Expansion::None,
                        validation: Validation::Trust,
                    },
                )
            })
            .unzip();
        let mut knowledge = Knowledge { initial, info };

        for msg in &agent.messages {
            match msg.direction {
                Direction::Ingoing => {
                    for (index, t) in msg.message.terms.iter().enumerate() {
                        knowledge.retrieve(
                            TermOrigin::Received {
                                msg_nr: msg.nr,
                                index,
                            },
                            t.clone(),
                        );
                    }
                }
                Direction::Outgoing => {
                    for t in &msg.initiates {
                        let ty = match t {
                            macor::protocol::Variable::Agent(a) => {
                                eprintln!("Wtf? {a:?} in {msg:?}");
                                continue;
                            }
                            macor::protocol::Variable::SymmetricKey(_) => GenerateTy::SymKey,
                            macor::protocol::Variable::Number(_) => GenerateTy::Nonce,
                        };
                        knowledge.retrieve(
                            TermOrigin::Initiated { ty, msg_nr: msg.nr },
                            Term::Variable(t.clone()),
                        );
                    }

                    for t in &msg.message.terms {
                        knowledge.construct(msg.nr, t);
                    }
                }
            }

            let mut count = knowledge.info.len();
            loop {
                knowledge.augment_knowledge(msg.nr);
                let new_count = knowledge.info.len();
                if new_count == count {
                    break;
                }
                count = new_count;
            }
        }

        let mut ingoing = agent
            .messages
            .iter()
            .filter(|msg| msg.is_ingoing())
            .map(|msg| (MessageId::Nr(msg.nr), msg.message.terms.clone()))
            .collect_vec();
        if agent.is_initiator() {
            ingoing.insert(
                0,
                (
                    MessageId::Initialize,
                    agent.initial_knowledge.iter().cloned().collect(),
                ),
            );
        }
        let outgoing = agent
            .messages
            .iter()
            .filter(|msg| msg.is_outgoing())
            .map(|msg| (msg.nr, msg.message.terms.clone()))
            .collect();

        let mut agent_info_ids: HashMap<AgentName, InfoId> = Default::default();

        for i in &knowledge.info {
            let mut set = |v: &AgentName, id: InfoId| {
                if !agent_info_ids.contains_key(v) {
                    agent_info_ids.insert(v.clone(), id);
                }
            };

            match &i.term {
                Term::Variable(v) => match v {
                    macor::protocol::Variable::Agent(s) => set(s, i.id),
                    _ => {}
                },
                Term::Constant(c) => match c {
                    macor::protocol::Constant::Agent(s) => set(s, i.id),
                    _ => {}
                },
                _ => {}
            }
        }

        let mut listen_ports: Vec<AgentName> = Default::default();
        let mut connections: HashSet<AgentName> = Default::default();
        let mut messages: Vec<MessageInstructions> = vec![];

        let mut save_connection_as = None;

        for (prev, msg, next) in agent.messages.iter().enumerate().map(|(idx, msg)| {
            (
                if idx >= 1 {
                    agent.messages.get(idx - 1)
                } else {
                    None
                },
                msg,
                if let Some(next) = agent.messages.get(idx + 1) {
                    Some(next)
                } else {
                    None
                },
            )
        }) {
            match msg.direction {
                Direction::Ingoing => {
                    if connections.insert(msg.from.clone()) {
                        save_connection_as = Some(msg.from.clone());
                        listen_ports.push(msg.from.clone());
                    }

                    match next {
                        Some(_) => continue,
                        None => {}
                    }

                    messages.push(MessageInstructions {
                        id: MessageId::Nr(msg.nr),
                        match_on: MessageThingy::Pattern(msg.clone()),
                        instructions: knowledge.generate_instructions(msg, msg.nr),
                        response: Response {
                            save_connection_as: None,
                            send: SendMessage::Nothing,
                            action: NextAction::Terminate,
                        },
                    });
                }
                Direction::Outgoing => {
                    let connect = connections.insert(msg.to.clone());

                    let next_action = match next {
                        Some(next) => {
                            if connections.contains(&next.from) {
                                NextAction::RecvFrom(
                                    next.from.clone(),
                                    *agent_info_ids.get(&next.from).unwrap(),
                                )
                            } else {
                                listen_ports.push(next.from.clone());
                                NextAction::ListenOn(
                                    next.from.clone(),
                                    *agent_info_ids.get(&next.from).unwrap(),
                                )
                            }
                        }
                        None => NextAction::Terminate,
                    };

                    match prev {
                        Some(prev) => {
                            let mut prev_insts = knowledge.generate_instructions(prev, prev.nr);
                            let insts = knowledge.generate_instructions(msg, msg.nr);

                            if prev.is_first_for_agent {
                                prev_insts.insert(
                                    0,
                                    Instruction::ComputeInitialKnowledgeFrom {
                                        number_given: prev.message.terms.len(),
                                        into: knowledge
                                            .initial
                                            .iter()
                                            .map(|(_, i)| i.clone())
                                            .collect(),
                                    },
                                );
                            }

                            messages.push(MessageInstructions {
                                id: MessageId::Nr(prev.nr),
                                match_on: MessageThingy::Pattern(prev.clone()),
                                instructions: prev_insts.into_iter().chain(insts).collect(),
                                response: Response {
                                    save_connection_as: save_connection_as.take(),
                                    send: SendMessage::Send {
                                        nr: msg.nr,
                                        body: msg
                                            .message
                                            .iter()
                                            .map(|t| knowledge.construct(msg.nr, t))
                                            .collect(),
                                        connect,
                                        to: (msg.to.clone(), *agent_info_ids.get(&msg.to).unwrap()),
                                    },
                                    action: next_action,
                                },
                            });
                        }
                        None => {
                            let body = msg
                                .message
                                .iter()
                                .map(|t| knowledge.construct(msg.nr, t))
                                .collect();

                            messages.push(MessageInstructions {
                                id: MessageId::Initialize,
                                match_on: MessageThingy::Initiate(
                                    agent.initial_knowledge.iter().cloned().collect(),
                                ),
                                instructions: knowledge
                                    .initial
                                    .iter()
                                    .map(|&(index, id)| Instruction::Retrieve { index, id })
                                    .chain(knowledge.generate_instructions(msg, msg.nr))
                                    .collect(),
                                response: Response {
                                    save_connection_as: save_connection_as.take(),
                                    send: SendMessage::Send {
                                        nr: msg.nr,
                                        body,
                                        connect,
                                        to: (msg.to.clone(), *agent_info_ids.get(&msg.to).unwrap()),
                                    },
                                    action: next_action,
                                },
                            });
                        }
                    }
                }
            }
        }

        // let init: Option<(MessageThingy, Vec<Instruction>)> = if agent.is_initiator() {
        //     Some((
        //         MessageThingy::Initiate(agent.initial_knowledge.iter().cloned().collect()),
        //         knowledge
        //             .initial
        //             .iter()
        //             .map(|&(index, id)| Instruction::Retrieve { index, id })
        //             .collect(),
        //     ))
        // } else {
        //     None
        // };

        // let (temp, mut messages) =
        //     agent
        //         .messages
        //         .iter()
        //         .fold((init, vec![]), |(prev, mut messages), msg| match prev {
        //             Some((p_msg, mut p)) => match msg.direction {
        //                 Direction::Ingoing => todo!(),
        //                 Direction::Outgoing => {
        //                     let insts = knowledge.generate_instructions(msg, msg.nr);
        //                     p.extend_from_slice(&insts);
        //                     messages.push(MessageInstructions {
        //                         match_on: p_msg,
        //                         instructions: p,
        //                         response: Response::Send {
        //                             nr: msg.nr,
        //                             body: msg
        //                                 .message
        //                                 .iter()
        //                                 .map(|t| knowledge.construct(msg.nr, t))
        //                                 .collect(),
        //                             // connect: ,
        //                             to: *agent_info_ids.get(&msg.to).unwrap(),
        //                         },
        //                         next_action: NextAction::ListenOn { agent: () },
        //                         // end_with: MessageEnd::Send {
        //                         //     nr: msg.nr,
        //                         //     body: msg
        //                         //         .message
        //                         //         .iter()
        //                         //         .map(|t| knowledge.construct(msg.nr, t))
        //                         //         .collect(),
        //                         //     to: *agent_info_ids.get(&msg.to).unwrap(),
        //                         // },
        //                     });
        //                     (None, messages)
        //                 }
        //             },
        //             None => match msg.direction {
        //                 Direction::Ingoing => {
        //                     let mut insts = knowledge.generate_instructions(msg, msg.nr);

        //                     if msg.is_first_for_agent {
        //                         insts.insert(
        //                             0,
        //                             Instruction::ComputeInitialKnowledgeFrom {
        //                                 number_given: msg.message.terms.len(),
        //                                 into: knowledge
        //                                     .initial
        //                                     .iter()
        //                                     .map(|(_, i)| i.clone())
        //                                     .collect(),
        //                             },
        //                         );
        //                     }

        //                     (Some((MessageThingy::Pattern(msg.clone()), insts)), messages)
        //                 }
        //                 Direction::Outgoing => todo!("{msg:?}"),
        //             },
        //         });
        // match temp {
        //     Some((mp, insts)) => {
        //         messages.push(MessageInstructions {
        //             match_on: mp,
        //             instructions: insts,
        //             response: Response::Nothing,
        //             next_action: NextAction::Terminate,
        //         });
        //     }
        //     None => {}
        // }

        let mut stage_names = ingoing.iter().map(|(id, _)| id.to_string()).collect_vec();
        stage_names.push("Terminated".to_string());

        let model = AgentModel {
            name: agent.name.clone(),
            registers: knowledge
                .info
                .iter()
                .map(|i| (i.id, i.term.clone()))
                .collect(),
            stage_names,
            initial_knowledge: knowledge
                .initial
                .iter()
                .map(|(_, i)| (*i, knowledge.info[i.0].term.clone()))
                .collect(),
            initialize_knowledge_from_first_message: if !agent.is_initiator() {
                let first_msg = agent.messages.first().unwrap();

                Some(InitializeInitialKnowledge {
                    from_agent: first_msg.from.clone(),
                    from_types: first_msg.message.terms.clone(),
                    from_msg: first_msg.nr,
                })
            } else {
                None
            },
            listen_ports,
            ingoing,
            outgoing,
            messages,
        };

        agents.push(model);
    }

    let mut custom_types = vec![];

    for (name, (_args, _ret)) in &protocol.functions {
        custom_types.push(CustomType {
            name: name.to_string(),
        });
    }

    Ok(ProtocolModel {
        instance: InstanceInfo {
            types: custom_types,
        },
        messages: agents
            .iter()
            .flat_map(|a| {
                a.ingoing.iter().cloned().chain(
                    a.outgoing
                        .iter()
                        .cloned()
                        .map(|(nr, m)| (MessageId::Nr(nr), m)),
                )
            })
            .sorted()
            .dedup()
            .collect(),
        agents,
    })
}
pub fn generate(src: &str, doc: Document<&str>) -> anyhow::Result<String> {
    let model = prepate_model(src, doc)?;
    model.rust()
}
