use std::{collections::HashMap, hash::Hash};

use crate::{
    ast,
    dolev_yao::{self, Knowledge},
    search::Packet,
};

pub trait Stage: PartialEq + Eq + std::fmt::Debug + Clone + PartialOrd + Ord + Hash {
    type Constant: PartialEq + Eq + std::fmt::Debug + Clone + PartialOrd + Ord + Hash;
    type Variable: PartialEq + Eq + std::fmt::Debug + Clone + PartialOrd + Ord + Hash;
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct UntypedStage;

impl Stage for UntypedStage {
    type Constant = !;
    type Variable = String;
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct TypedStage;

impl Stage for TypedStage {
    type Constant = Constant;
    type Variable = Variable;
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct ActorName(String);

#[derive(PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct SessionId(pub u32);

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum InstanceName {
    Actor(ActorName, SessionId),
    Constant(ActorName),
    Intruder,
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Func(pub String);

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Message<S: Stage = TypedStage> {
    Variable(S::Variable),
    Constant(S::Constant),
    Composition {
        func: Func,
        args: Vec<Message<S>>,
    },
    SymEnc {
        message: Box<Message<S>>,
        key: Box<Message<S>>,
    },
    AsymEnc {
        message: Box<Message<S>>,
        key: Box<Message<S>>,
    },
    Tuple(Vec<Message<S>>),
    Inverse(Box<Message<S>>),
    Exp(Box<Message<S>>),
}

impl<'a> From<ast::Message<'a>> for Message<UntypedStage> {
    fn from(message: ast::Message<'a>) -> Self {
        match message {
            ast::Message::Var(var) => Self::Variable(var.to_string()),
            ast::Message::Fun(name, args) => Self::Composition {
                func: Func(name.0.to_string()),
                args: args.into_iter().map(Message::from).collect(),
            },
            ast::Message::SymEnc(messages, key) => Self::SymEnc {
                message: Box::new(Message::Tuple(
                    messages.into_iter().map(Message::from).collect(),
                )),
                key: Box::new(Message::from(*key)),
            },
            ast::Message::AsymEnc(messages, key) => Self::AsymEnc {
                message: Box::new(Message::Tuple(
                    messages.into_iter().map(Message::from).collect(),
                )),
                key: Box::new(Message::from(*key)),
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

    pub fn init_all_actors(&self, session_id: SessionId) -> Knowledge {
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
        // NOTE: Assume that augment_knowledge has already been called :)
        dolev_yao::composition_search(self, msg, &[])
    }
}

impl Knowledge<UntypedStage> {
    pub fn to_typed(&self, ctx: &TypingContext) -> Knowledge<TypedStage> {
        self.iter().map(|message| message.to_typed(ctx)).collect()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Variable {
    Actor(ActorName),
    SymmetricKey(String),
    Number(String),
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

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Agent,
    SymmetricKey,
    Number,
    Function,
}

#[derive(Debug, Clone)]
pub struct TypingContext {
    types: HashMap<String, Type>,
}
impl TypingContext {
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.types.get(name).cloned()
    }
}

impl Message<UntypedStage> {
    fn to_typed(&self, ctx: &TypingContext) -> Message<TypedStage> {
        match self {
            Message::Variable(name) => {
                let is_constant = name
                    .chars()
                    .next()
                    .expect("Empty variable not supported")
                    .is_lowercase();

                if let Some(ty) = ctx.lookup(name) {
                    match ty {
                        Type::Agent => {
                            if is_constant {
                                Message::Variable(Variable::Actor(ActorName(name.clone())))
                            } else {
                                Message::Constant(Constant::Actor(ActorName(name.clone())))
                            }
                        }
                        Type::SymmetricKey => {
                            assert!(!is_constant, "symmetric keys '{name}' cannot be constant");
                            Message::Variable(Variable::SymmetricKey(name.clone()))
                        }
                        Type::Number => {
                            assert!(!is_constant, "numbers '{name}' cannot be constant");
                            Message::Variable(Variable::Number(name.clone()))
                        }
                        Type::Function => {
                            assert!(is_constant, "functions '{name}' cannot be variable");
                            Message::Constant(Constant::Function(Func(name.clone())))
                        }
                    }
                } else {
                    panic!("name {name} not found in {ctx:?}")
                }
            }
            Message::Constant(_) => unreachable!("Constant in untyped contains ! type"),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args.iter().map(|x| x.to_typed(ctx)).collect(),
            },
            Message::SymEnc { message, key } => Message::SymEnc {
                message: box message.to_typed(ctx),
                key: box key.to_typed(ctx),
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: box message.to_typed(ctx),
                key: box key.to_typed(ctx),
            },
            Message::Tuple(xs) => Message::Tuple(xs.iter().map(|x| x.to_typed(ctx)).collect()),
            Message::Inverse(x) => Message::Inverse(box x.to_typed(ctx)),
            Message::Exp(x) => Message::Exp(box x.to_typed(ctx)),
        }
    }
}
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
                message: box message.substitute_actor_instance(from, to.clone()),
                key: box key.substitute_actor_instance(from, to),
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: box message.substitute_actor_instance(from, to.clone()),
                key: box key.substitute_actor_instance(from, to),
            },
            Message::Tuple(ts) => Message::Tuple(
                ts.iter()
                    .map(|t| t.substitute_actor_instance(from, to.clone()))
                    .collect(),
            ),
            Message::Inverse(i) => Message::Inverse(box i.substitute_actor_instance(from, to)),
            Message::Exp(x) => Message::Exp(box x.substitute_actor_instance(from, to)),
        }
    }
    pub fn init_all_actors(&self, session_id: SessionId) -> Message<TypedStage> {
        match self {
            Message::Variable(var) => match var {
                Variable::Actor(a) => Message::Constant(Constant::Instance(InstanceName::Actor(
                    a.clone(),
                    session_id,
                ))),
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
                message: box message.init_all_actors(session_id),
                key: box key.init_all_actors(session_id),
            },
            Message::AsymEnc { message, key } => Message::AsymEnc {
                message: box message.init_all_actors(session_id),
                key: box key.init_all_actors(session_id),
            },
            Message::Tuple(ts) => {
                Message::Tuple(ts.iter().map(|t| t.init_all_actors(session_id)).collect())
            }
            Message::Inverse(x) => Message::Inverse(box x.init_all_actors(session_id)),
            Message::Exp(x) => Message::Exp(box x.init_all_actors(session_id)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Recipient {
    Variable(ActorName),
    Instance(InstanceName),
}

#[derive(Debug, Clone)]
pub enum PacketPattern {
    Ingoing(Packet),
    Outgoing(Recipient, Packet),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ActorKind {
    Variable,
    Constant,
}

#[derive(Debug, Clone)]
pub struct ProtocolActor {
    pub name: ActorName,
    pub kind: ActorKind,
    pub initial_knowledge: Knowledge,
    pub messages: Vec<PacketPattern>,
}

#[derive(Debug, Clone)]
pub enum Goal {
    SecretBetween(Message),
    Authenticates(Message),
}

#[derive(Debug, Clone)]
pub struct Protocol {
    pub actors: Vec<ProtocolActor>,
    pub goals: Vec<Goal>,
}

impl Protocol {
    pub fn new(document: ast::Document) -> Protocol {
        let ctx = TypingContext {
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
        };

        let actors = document
            .knowledge
            .agents
            .iter()
            .map(|(agent, knowledge)| -> ProtocolActor {
                let k: Knowledge<UntypedStage> = knowledge.clone().into();

                ProtocolActor {
                    name: ActorName(agent.to_string()),
                    kind: if agent.starts_with(|c: char| c.is_lowercase()) {
                        ActorKind::Constant
                    } else {
                        ActorKind::Variable
                    },
                    initial_knowledge: k.to_typed(&ctx),
                    messages: document
                        .actions
                        .iter()
                        .filter_map(|a| {
                            if &a.from == agent {
                                Some(PacketPattern::Outgoing(
                                    if a.to.starts_with(|c: char| c.is_lowercase()) {
                                        Recipient::Instance(InstanceName::Constant(ActorName(
                                            a.to.to_string(),
                                        )))
                                    } else {
                                        Recipient::Variable(ActorName(a.to.to_string()))
                                    },
                                    a.msgs
                                        .iter()
                                        .map(|m| {
                                            Message::<UntypedStage>::from(m.clone()).to_typed(&ctx)
                                        })
                                        .collect(),
                                ))
                            } else if &a.to == agent {
                                Some(PacketPattern::Ingoing(
                                    a.msgs
                                        .iter()
                                        .map(|m| {
                                            Message::<UntypedStage>::from(m.clone()).to_typed(&ctx)
                                        })
                                        .collect(),
                                ))
                            } else {
                                None
                            }
                        })
                        .collect(),
                }
            })
            .collect();

        Protocol {
            actors,
            goals: vec![],
        }
    }
}