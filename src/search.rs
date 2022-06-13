use std::collections::HashMap;

use crate::protocol::{
    ActorKind, InstanceName, Knowledge, MessagePattern, Protocol, Recipient, SessionId,
};

#[derive(Debug)]
struct Strand {
    current_execution: usize,
    messages: Vec<MessagePattern>,
    knowledge: Knowledge,
}

#[derive(Debug)]
struct Intruder {
    knowledge: Knowledge,
}

#[derive(Debug)]
struct Execution {
    intruder: Option<Intruder>,
    sessions: Vec<Session>,
}

#[derive(Debug)]
struct Session {
    actors: HashMap<InstanceName, Actor>,
}

#[derive(Debug)]
struct Actor {
    name: InstanceName,
    strand: Strand,
}

#[derive(Debug)]
pub enum SearchResult {
    NoAttackFound,
    AttackFound,
}

#[derive(Debug)]
pub struct Searcher {
    protocol: Protocol,
}

#[derive(Debug)]
pub struct SearchOptions {
    pub num_sessions: u32,
}

impl Searcher {
    pub fn new(protocol: Protocol) -> Searcher {
        Searcher { protocol }
    }

    pub fn find_attack(&self, s: SearchOptions) -> SearchResult {
        let sessions = (0..s.num_sessions)
            .map(|n| Session {
                actors: self
                    .protocol
                    .actors
                    .iter()
                    .map(move |a| {
                        let session = SessionId(n);
                        let name = InstanceName::Actor(a.name.clone(), session);
                        (
                            name.clone(),
                            Actor {
                                name,
                                strand: Strand {
                                    current_execution: 0,
                                    messages: a
                                        .messages
                                        .iter()
                                        .map(|msg| msg.init_all_actors(session))
                                        .collect(),
                                    knowledge: a.initial_knowledge.init_all_actors(session),
                                },
                            },
                        )
                    })
                    .collect(),
            })
            .collect();

        let intruder_knowledge: Knowledge = self
            .protocol
            .actors
            .iter()
            .filter_map(|a| match a.kind {
                ActorKind::Variable => Some(
                    a.initial_knowledge
                        .substitute_actor_instance(&a.name, InstanceName::Intruder),
                ),
                ActorKind::Constant => None,
            })
            .collect();

        let exe = Execution {
            intruder: Some(Intruder {
                knowledge: intruder_knowledge,
            }),
            sessions,
        };

        todo!("{exe:#?}")
    }
}
impl Execution {
    pub fn next(&self) -> impl Iterator<Item = Execution> + '_ {
        self.sessions.iter().flat_map(|ses| {
            ses.actors.iter().flat_map(|(_, actor)| {
                match &actor.strand.messages[actor.strand.current_execution] {
                    MessagePattern::Ingoing(_) => vec![],
                    MessagePattern::Outgoing(target, msgs) => {
                        // TODO: Instantiate msgs with current actor. Should be
                        //       deterministic, as the protocol would otherwise
                        //       be ambiguous.

                        if msgs
                            .iter()
                            .any(|msg| !actor.strand.knowledge.can_construct(msg))
                        {
                            return vec![];
                        }

                        let target = match target {
                            Recipient::Variable(_) => unreachable!(),
                            Recipient::Instance(target) => target,
                        };

                        todo!()
                    }
                }
            })
        })
    }
}
impl MessagePattern {
    fn init_all_actors(&self, session_id: SessionId) -> MessagePattern {
        match self {
            MessagePattern::Ingoing(msgs) => MessagePattern::Ingoing(
                msgs.iter()
                    .map(|msg| msg.init_all_actors(session_id))
                    .collect(),
            ),
            MessagePattern::Outgoing(target, msgs) => MessagePattern::Outgoing(
                match target {
                    Recipient::Variable(v) => {
                        Recipient::Instance(InstanceName::Actor(v.clone(), session_id))
                    }
                    Recipient::Instance(i) => Recipient::Instance(i.clone()),
                },
                msgs.iter()
                    .map(|msg| msg.init_all_actors(session_id))
                    .collect(),
            ),
        }
    }
}
