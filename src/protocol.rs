use std::{hash::Hash, rc::Rc};

use itertools::Itertools;
use smol_str::SmolStr;

use crate::{
    ast::{self, Ident},
    dolev_yao::{self, Knowledge},
    search::{Packet, SubstitutionTable},
    typing::{Stage, Type, TypedStage, TypingContext, TypingError, UntypedStage},
};

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct ActorName(pub(crate) Ident<SmolStr>);

impl std::fmt::Debug for ActorName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct SessionId(pub u32);

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum InstanceName {
    Actor(ActorName, SessionId),
    Constant(ActorName),
    Intruder,
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Func(pub Ident<SmolStr>);

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub enum Message<S: Stage = TypedStage> {
    Variable(S::Variable),
    Constant(S::Constant),
    Composition {
        func: Func,
        args: Vec<Message<S>>,
    },
    SymEnc {
        message: Rc<Message<S>>,
        key: Rc<Message<S>>,
    },
    AsymEnc {
        message: Rc<Message<S>>,
        key: Rc<Message<S>>,
    },
    Tuple(Vec<Message<S>>),
    Inverse(Rc<Message<S>>),
    Exp(Rc<Message<S>>),
}

impl<S: Stage> std::fmt::Debug for Message<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(var) => write!(f, "@{var:?}"),
            Self::Constant(c) => write!(f, "#{c:?}"),
            Self::Composition { func, args } => {
                write!(f, "{}({:?})", func.0, args.iter().format(", "))
            }
            Self::SymEnc { message, key } => write!(f, "{{| {:?} |}}{:?}", message, key),
            Self::AsymEnc { message, key } => write!(f, "{{ {:?} }}{:?}", message, key),
            Self::Tuple(ts) => write!(f, ":({:?})", ts.iter().format(", ")),
            Self::Inverse(arg0) => f.debug_tuple("Inverse").field(arg0).finish(),
            Self::Exp(arg0) => f.debug_tuple("Exp").field(arg0).finish(),
        }
    }
}

impl<'a> From<ast::Message<&'a str>> for Message<UntypedStage<'a>> {
    fn from(message: ast::Message<&'a str>) -> Self {
        match message {
            ast::Message::Var(var) => Self::Variable(var.clone()),
            ast::Message::Fun(name, args) => Self::Composition {
                func: Func(name.convert()),
                args: args.into_iter().map(Message::from).collect(),
            },
            ast::Message::SymEnc(messages, key) => Self::SymEnc {
                message: Rc::new(Message::Tuple(
                    messages.into_iter().map(Message::from).collect(),
                )),
                key: Rc::new(Message::from(*key)),
            },
            ast::Message::AsymEnc(messages, key) => Self::AsymEnc {
                message: Rc::new(Message::Tuple(
                    messages.into_iter().map(Message::from).collect(),
                )),
                key: Rc::new(Message::from(*key)),
            },
        }
    }
}

impl Knowledge<TypedStage> {
    pub fn substitute_actor_instance(
        &self,
        from: &ActorName,
        to: InstanceName,
    ) -> Knowledge<TypedStage> {
        let messages = self
            .iter()
            .map(|message| message.substitute_actor_instance(from, to.clone()))
            .collect();
        Knowledge::new(messages)
    }

    pub fn init_all_actors(&self, session_id: SessionId) -> Knowledge<TypedStage> {
        Knowledge::new(
            self.iter()
                .map(|msg| msg.init_all_actors(session_id))
                .collect(),
        )
    }

    pub fn can_verify(&self, msg: &Message) -> bool {
        self.can_construct(msg)
    }
    pub fn can_construct(&self, msg: &Message) -> bool {
        // TODO: Remove this when found to be not a problem :)
        let mut augmented = self.clone();
        dolev_yao::augment_knowledge(&mut augmented);
        assert_eq!(self, &augmented);

        // NOTE: Assume that augment_knowledge has already been called :)
        dolev_yao::composition_search(self, msg)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Variable {
    Actor(ActorName),
    SymmetricKey(Ident<SmolStr>),
    Number(Ident<SmolStr>),
}
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Constant {
    Actor(ActorName),
    Instance(InstanceName),
    Function(Func),
    Nonce(Nonce),
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Nonce(pub u32);

impl Message<TypedStage> {
    fn substitute_actor_instance(&self, from: &ActorName, to: InstanceName) -> Message<TypedStage> {
        match self {
            Message::Variable(Variable::Actor(a)) if a == from => {
                Message::Constant(Constant::Instance(to))
            }
            Message::Variable(_) | Message::Constant(_) => self.clone(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|arg| arg.substitute_actor_instance(from, to.clone()))
                    .collect(),
            },
            Message::SymEnc { message, key } => Message::SymEnc {
                message: Rc::new(message.substitute_actor_instance(from, to.clone())),
                key: Rc::new(key.substitute_actor_instance(from, to)),
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: Rc::new(message.substitute_actor_instance(from, to.clone())),
                key: Rc::new(key.substitute_actor_instance(from, to)),
            },
            Message::Tuple(ts) => Message::Tuple(
                ts.iter()
                    .map(|t| t.substitute_actor_instance(from, to.clone()))
                    .collect(),
            ),
            Message::Inverse(i) => Message::Inverse(Rc::new(i.substitute_actor_instance(from, to))),
            Message::Exp(x) => Message::Exp(Rc::new(x.substitute_actor_instance(from, to))),
        }
    }
    pub fn perform_substitutions(&self, subs: &SubstitutionTable) -> Message<TypedStage> {
        let res = match self {
            Message::Variable(var) => match subs.table.get(var) {
                Some(m) => m.clone(),
                None => self.clone(),
            },
            Message::Constant(_) => self.clone(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|msg| msg.perform_substitutions(subs))
                    .collect(),
            },
            Message::SymEnc { message, key } => Message::SymEnc {
                message: Rc::new(message.perform_substitutions(subs)),
                key: Rc::new(key.perform_substitutions(subs)),
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: Rc::new(message.perform_substitutions(subs)),
                key: Rc::new(key.perform_substitutions(subs)),
            },
            Message::Tuple(ts) => {
                Message::Tuple(ts.iter().map(|t| t.perform_substitutions(subs)).collect())
            }
            Message::Inverse(i) => Message::Inverse(Rc::new(i.perform_substitutions(subs))),
            Message::Exp(x) => Message::Exp(Rc::new(x.perform_substitutions(subs))),
        };

        res
    }
    pub fn init_all_actors(&self, session_id: SessionId) -> Message<TypedStage> {
        match self {
            Message::Variable(var) => match var {
                Variable::Actor(a) => {
                    if a.0.is_constant() {
                        Message::Constant(Constant::Instance(InstanceName::Constant(a.clone())))
                    } else {
                        Message::Constant(Constant::Instance(InstanceName::Actor(
                            a.clone(),
                            session_id,
                        )))
                    }
                }
                Variable::SymmetricKey(_) | Variable::Number(_) => self.clone(),
            },
            Message::Constant(_) => self.clone(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|arg| arg.init_all_actors(session_id))
                    .collect(),
            },
            Message::SymEnc { message, key } => Message::SymEnc {
                message: Rc::new(message.init_all_actors(session_id)),
                key: Rc::new(key.init_all_actors(session_id)),
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: Rc::new(message.init_all_actors(session_id)),
                key: Rc::new(key.init_all_actors(session_id)),
            },
            Message::Tuple(ts) => {
                Message::Tuple(ts.iter().map(|t| t.init_all_actors(session_id)).collect())
            }
            Message::Inverse(x) => Message::Inverse(Rc::new(x.init_all_actors(session_id))),
            Message::Exp(x) => Message::Exp(Rc::new(x.init_all_actors(session_id))),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Recipient {
    Variable(ActorName),
    Instance(InstanceName),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PacketPattern {
    Ingoing(Packet),
    Outgoing(Recipient, Packet),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ActorKind {
    Variable,
    Constant,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolActor {
    pub name: ActorName,
    pub kind: ActorKind,
    pub initial_knowledge: Knowledge,
    pub messages: Vec<PacketPattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Goal<A> {
    SecretBetween(A, A, Message),
    Authenticates(A, A, Message),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Protocol {
    pub actors: Vec<ProtocolActor>,
    pub goals: Vec<Goal<ActorName>>,
}

impl Protocol {
    fn new_protocol_actor(
        agent: &Ident<&str>,
        knowledge: &[ast::Message<&str>],
        ctx: &mut TypingContext,
        document: &ast::Document<&str>,
    ) -> ProtocolActor {
        let k: Knowledge<UntypedStage> = knowledge.into();

        ProtocolActor {
            name: ActorName(agent.convert()),
            kind: if agent.is_constant() {
                ActorKind::Constant
            } else {
                ActorKind::Variable
            },
            initial_knowledge: k.to_typed(ctx),
            messages: document
                .actions
                .iter()
                .filter_map(|a| {
                    if &a.from == agent {
                        Some(PacketPattern::Outgoing(
                            if a.to.is_constant() {
                                Recipient::Instance(InstanceName::Constant(ActorName(
                                    a.to.convert(),
                                )))
                            } else {
                                Recipient::Variable(ActorName(a.to.convert()))
                            },
                            a.msgs
                                .iter()
                                .map(|m| Message::<UntypedStage>::from(m.clone()).to_typed(ctx))
                                .collect(),
                        ))
                    } else if &a.to == agent {
                        Some(PacketPattern::Ingoing(
                            a.msgs
                                .iter()
                                .map(|m| Message::<UntypedStage>::from(m.clone()).to_typed(ctx))
                                .collect(),
                        ))
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }

    pub fn new(src: String, document: ast::Document<&str>) -> Result<Protocol, Vec<TypingError>> {
        let mut ctx = TypingContext {
            types: document
                .types
                .iter()
                .flat_map(|(key, xs)| {
                    let ty = match key {
                        ast::TypesKey::Agent => Type::Agent,
                        ast::TypesKey::Number => Type::Number,
                        ast::TypesKey::SymmetricKey => Type::SymmetricKey,
                        ast::TypesKey::PublicKey => todo!(),
                        ast::TypesKey::Function => Type::Function,
                    };

                    xs.iter().map(move |x| (x.to_string(), ty))
                })
                .collect(),
            errors: Vec::new(),
            src,
        };

        let actors = document
            .knowledge
            .agents
            .iter()
            .map(|(agent, knowledge)| {
                Self::new_protocol_actor(agent, knowledge, &mut ctx, &document)
            })
            .collect();

        if ctx.errors.is_empty() {
            Ok(Protocol {
                actors,
                goals: vec![],
            })
        } else {
            Err(ctx.errors)
        }
    }
}
impl Func {
    pub(crate) fn is_public(&self) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match self.0.as_str() {
            "exp" => true,
            _ => false,
        }
    }
}
impl ActorName {
    pub fn initiate_for_session(&self, session_id: SessionId) -> InstanceName {
        if self.0.is_constant() {
            InstanceName::Constant(self.clone())
        } else {
            InstanceName::Actor(self.clone(), session_id)
        }
    }
}

// struct FreeId(usize);

// struct MessageRef(usize);

// enum Message {
//     Free(FreeId),
//     Just(MessageInner),
// }

// enum MessageInner {
//     Composition {
//         func: Func,
//         args: Vec<MessageRef>,
//     },
// }
