use std::{
    collections::{HashMap, VecDeque},
    sync::Once,
};

use crate::{
    dolev_yao::Knowledge,
    protocol::{
        ActorKind, Constant, InstanceName, Message, Nonce, PacketPattern, Protocol, Recipient,
        SessionId, Variable,
    },
};

#[derive(Debug, Clone)]
struct Strand {
    current_execution: usize,
    messages: Vec<PacketPattern>,
    knowledge: Knowledge,
    substitutions: HashMap<Variable, Message>,
}

#[derive(Debug, Clone)]
struct Intruder {
    knowledge: Knowledge,
}

#[derive(Debug, Clone)]
struct Execution {
    intruder: Option<Intruder>,
    sessions: Vec<Session>,
}

#[derive(Debug, Clone)]
struct Session {
    actors: HashMap<InstanceName, Actor>,
}

#[derive(Debug, Clone)]
pub struct Packet {
    messages: Vec<Message>,
}

impl FromIterator<Message> for Packet {
    fn from_iter<T: IntoIterator<Item = Message>>(iter: T) -> Self {
        Packet {
            messages: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Packet {
    type Item = Message;

    type IntoIter = std::vec::IntoIter<Message>;

    fn into_iter(self) -> Self::IntoIter {
        self.messages.into_iter()
    }
}
impl<'a> IntoIterator for &'a Packet {
    type Item = &'a Message;

    type IntoIter = std::slice::Iter<'a, Message>;

    fn into_iter(self) -> Self::IntoIter {
        self.messages.iter()
    }
}
impl Packet {
    fn iter(&self) -> impl Iterator<Item = &Message> {
        self.into_iter()
    }
}

#[derive(Debug, Clone)]
struct Actor {
    name: InstanceName,
    strand: Strand,
    inbox: VecDeque<Packet>,
}

#[derive(Debug, Clone)]
pub enum SearchResult {
    NoAttackFound,
    AttackFound,
}

#[derive(Debug, Clone)]
pub struct Searcher {
    protocol: Protocol,
}

#[derive(Debug, Clone)]
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
                                    substitutions: Default::default(),
                                },
                                inbox: VecDeque::default(),
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

        let _ = exe.next(&mut Default::default());

        todo!("{exe:#?}")
    }
}
#[derive(Debug, Default)]
struct ExecutionContext {
    next_nonce: u32,
}
impl ExecutionContext {
    fn new_nonce(&mut self) -> Nonce {
        let nonce = Nonce(self.next_nonce);
        self.next_nonce += 1;
        nonce
    }
}
impl Execution {
    pub fn next(&self, ctx: &mut ExecutionContext) -> Vec<Execution> {
        let mut next = vec![];

        for (i_ses, ses) in self.sessions.iter().enumerate() {
            for (actor_key, actor) in &ses.actors {
                let mut new_execution = self.clone();
                let new_actor = new_execution.sessions[i_ses]
                    .actors
                    .get_mut(actor_key)
                    .unwrap();

                match &actor.strand.messages[actor.strand.current_execution] {
                    PacketPattern::Ingoing(_) => {
                        // TODO: Can intruder send?

                        match actor.inbox.pop_front() {
                            Some(packet) => {}
                            None => todo!(),
                        }
                        todo!("check that inbox can match ingoing message")
                    }
                    PacketPattern::Outgoing(target, packet) => {
                        let target = match target {
                            Recipient::Variable(_) => unreachable!(),
                            Recipient::Instance(target) => target,
                        };

                        if let Some(new_packet) = new_actor.strand.initiate(ctx, packet) {
                            if let Some(i) = new_execution.intruder.as_mut() {
                                for msg in new_packet.iter() {
                                    i.knowledge.augment_with(msg.clone());
                                }
                            }

                            // NOTE: A new execution where the recipient does
                            // not get the message.
                            next.push(new_execution.clone());

                            new_execution.sessions[i_ses]
                                .actors
                                .get_mut(target)
                                .unwrap()
                                .inbox
                                .push_back(new_packet);

                            next.push(new_execution);
                        } else {
                            todo!("{:#?} cannot initiate {:#?}", &new_actor.strand, packet)
                        }
                    }
                }
            }
        }

        next
    }
}
impl PacketPattern {
    fn init_all_actors(&self, session_id: SessionId) -> PacketPattern {
        match self {
            PacketPattern::Ingoing(packet) => PacketPattern::Ingoing(
                packet
                    .iter()
                    .map(|msg| msg.init_all_actors(session_id))
                    .collect(),
            ),
            PacketPattern::Outgoing(target, packet) => PacketPattern::Outgoing(
                match target {
                    Recipient::Variable(v) => {
                        Recipient::Instance(InstanceName::Actor(v.clone(), session_id))
                    }
                    Recipient::Instance(i) => Recipient::Instance(i.clone()),
                },
                packet
                    .iter()
                    .map(|msg| msg.init_all_actors(session_id))
                    .collect(),
            ),
        }
    }
}
impl Strand {
    fn initiate(&mut self, ctx: &mut ExecutionContext, packet: &Packet) -> Option<Packet> {
        packet
            .iter()
            .map(|msg| self.initiate_msg(ctx, msg))
            .collect()
    }
    fn initiate_msg(&mut self, ctx: &mut ExecutionContext, msg: &Message) -> Option<Message> {
        let new_msg = match msg {
            Message::Variable(var) => {
                self.substitutions
                    .entry(var.clone())
                    .or_insert_with_key(|var| match var {
                        Variable::Actor(_) => unreachable!("all actors have been initiated"),
                        Variable::SymmetricKey(_) | Variable::Number(_) => {
                            // TODO: Add marker for which type of variable
                            // the nonce came from
                            let new = Message::Constant(Constant::Nonce(ctx.new_nonce()));
                            self.knowledge.augment_with(new.clone());
                            new
                        }
                    })
                    .clone()
            }
            Message::Constant(_) => msg.clone(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|arg| self.initiate_msg(ctx, arg))
                    .collect::<Option<Vec<_>>>()?,
            },
            Message::SymEnc { message, key } => Message::SymEnc {
                message: box self.initiate_msg(ctx, message)?,
                key: box self.initiate_msg(ctx, key)?,
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: box self.initiate_msg(ctx, message)?,
                key: box self.initiate_msg(ctx, key)?,
            },
            Message::Tuple(ts) => Message::Tuple(
                ts.iter()
                    .map(|t| self.initiate_msg(ctx, t))
                    .collect::<Option<Vec<_>>>()?,
            ),
            Message::Inverse(x) => Message::Inverse(box self.initiate_msg(ctx, x)?),
            Message::Exp(x) => Message::Exp(box self.initiate_msg(ctx, x)?),
        };

        self.knowledge.can_construct(&new_msg).then(|| new_msg)
    }
}
