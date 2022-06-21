use std::hash::Hash;

use indexmap::{indexset, IndexMap, IndexSet};
use itertools::Itertools;
use macor_parse::ast::{self, Ident};
use smol_str::SmolStr;

use crate::{
    dolev_yao_old::Knowledge,
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
pub enum Func<U = Ident<SmolStr>> {
    SymEnc,
    AsymEnc,
    Exp,
    Inv,
    User(U),
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct LazyId(pub usize);

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub enum Message<S: Stage = TypedStage> {
    Variable(S::Variable),
    Constant(S::Constant),
    Composition { func: Func, args: Vec<Message<S>> },
    Tuple(Vec<Message<S>>),
}

impl<S: Stage> std::fmt::Debug for Message<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(var) => write!(f, "@{var:?}"),
            Self::Constant(c) => write!(f, "#{c:?}"),
            Self::Composition { func, args } => match func {
                Func::SymEnc => write!(f, "{{| {:?} |}}{:?}", args[0], args[1]),
                Func::AsymEnc => write!(f, "{{ {:?} }}{:?}", args[0], args[1]),
                Func::Inv => f.debug_tuple("Inverse").field(&args[0]).finish(),
                Func::Exp => f.debug_tuple("Exp").field(&args[0]).finish(),
                Func::User(name) => write!(f, "{}({:?})", name, args.iter().format(", ")),
            },
            Self::Tuple(ts) => write!(f, ":({:?})", ts.iter().format(", ")),
        }
    }
}

impl<'a> From<ast::Message<&'a str>> for Message<UntypedStage<'a>> {
    fn from(message: ast::Message<&'a str>) -> Self {
        match message {
            ast::Message::Var(var) => Self::Variable(var.clone()),
            ast::Message::Fun(name, args) => match name.as_str() {
                "inv" => Self::Composition {
                    func: Func::Inv,
                    args: args.into_iter().map_into().collect(),
                },
                "exp" => Self::Composition {
                    func: Func::Exp,
                    args: args.into_iter().map_into().collect(),
                },
                _ => Self::Composition {
                    func: Func::User(name.convert()),
                    args: args.into_iter().map_into().collect(),
                },
            },
            ast::Message::SymEnc(messages, key) => Self::Composition {
                func: Func::SymEnc,
                args: vec![
                    Message::Tuple(messages.into_iter().map_into().collect()),
                    Message::from(*key),
                ],
            },
            ast::Message::AsymEnc(messages, key) => Self::Composition {
                func: Func::AsymEnc,
                args: vec![
                    Message::Tuple(messages.into_iter().map_into().collect()),
                    Message::from(*key),
                ],
            },
        }
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
    Intruder,
    Actor(ActorName),
    Function(Func),
    Nonce(Nonce),
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Nonce(pub u32);

#[derive(Debug, Clone, PartialEq)]
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
    pub fn iter(&self) -> impl Iterator<Item = &Message> {
        self.into_iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Direction {
    Ingoing,
    Outgoing,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PacketPattern {
    pub from: ActorName,
    pub to: ActorName,
    pub direction: Direction,
    pub packet: Packet,
    pub initiates: IndexSet<Variable>,
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
    SecretBetween(Vec<A>, Vec<Message>),
    Authenticates(A, A, Vec<Message>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Protocol {
    pub actors: Vec<ProtocolActor>,
    pub initiations: IndexMap<Variable, ActorName>,
    pub goals: Vec<Goal<ActorName>>,
}

struct ProtocolAction {
    from: Ident<SmolStr>,
    to: Ident<SmolStr>,
    messages: Vec<Message>,
    initiates: IndexSet<Variable>,
}

impl Protocol {
    fn new_protocol_actor(
        agent: &Ident<SmolStr>,
        knowledge: &[ast::Message<&str>],
        ctx: &mut TypingContext,
        actions: &[ProtocolAction],
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
            messages: actions
                .iter()
                .filter_map(|a| {
                    let direction = if &a.from == agent {
                        Direction::Outgoing
                    } else if &a.to == agent {
                        Direction::Ingoing
                    } else {
                        return None;
                    };

                    Some(PacketPattern {
                        from: ActorName(a.from.convert()),
                        to: ActorName(a.to.convert()),
                        direction,
                        packet: a.messages.iter().cloned().collect(),
                        initiates: a.initiates.clone(),
                    })
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

        let mut actions = document
            .actions
            .iter()
            .map(|action| ProtocolAction {
                from: action.from.convert(),
                to: action.to.convert(),
                messages: action
                    .msgs
                    .iter()
                    .map(|msg| Message::<UntypedStage>::from(msg.clone()).to_typed(&mut ctx))
                    .collect(),
                initiates: Default::default(),
            })
            .collect_vec();

        let mut initiations = IndexMap::default();
        let mut seen = IndexSet::<Variable>::default();

        for action in &mut actions {
            let in_this: IndexSet<Variable> = action
                .messages
                .iter()
                .flat_map(|msg| msg.extract_variables())
                .collect();

            action.initiates = in_this.difference(&seen).cloned().collect();
            for var in &action.initiates {
                initiations.insert(var.clone(), ActorName(action.from.clone()));
            }
            seen.extend(in_this.into_iter());
        }

        let actors = document
            .knowledge
            .agents
            .iter()
            .map(|(agent, knowledge)| {
                Self::new_protocol_actor(&agent.convert(), knowledge, &mut ctx, &actions)
            })
            .collect();

        let goals = document
            .goals
            .iter()
            .map(|goal| match goal {
                ast::Goal::Authenticates {
                    a,
                    b,
                    msgs,
                    weakly: _,
                } => Goal::Authenticates(
                    ActorName(a.convert()),
                    ActorName(b.convert()),
                    msgs.iter()
                        .map(|msg| Message::<UntypedStage>::from(msg.clone()).to_typed(&mut ctx))
                        .collect(),
                ),
                ast::Goal::SecretBetween {
                    msgs,
                    agents,
                    guessable: _,
                } => Goal::SecretBetween(
                    agents
                        .iter()
                        .map(|agent| ActorName(agent.convert()))
                        .collect(),
                    msgs.iter()
                        .map(|msg| Message::<UntypedStage>::from(msg.clone()).to_typed(&mut ctx))
                        .collect(),
                ),
            })
            .collect();

        if ctx.errors.is_empty() {
            Ok(Protocol {
                actors,
                initiations,
                goals,
            })
        } else {
            Err(ctx.errors)
        }
    }
}
impl<T> Func<T> {
    pub(crate) fn is_public(&self) -> bool {
        match self {
            Func::SymEnc | Func::AsymEnc | Func::Exp => true,
            Func::Inv => false,
            Func::User(_) => false,
        }
    }
}
impl Message {
    fn extract_variables(&self) -> IndexSet<Variable> {
        match self {
            Message::Variable(v) => indexset! { v.clone() },
            Message::Constant(_) => indexset! {},
            Message::Composition { func: _, args } => args
                .iter()
                .flat_map(|arg| arg.extract_variables())
                .collect(),
            Message::Tuple(ts) => ts.iter().flat_map(|t| t.extract_variables()).collect(),
        }
    }

    pub(crate) fn replace_agent_with_intruder(&self, current_agent: &ActorName) -> Self {
        match self {
            Message::Variable(var) => match var {
                Variable::Actor(a) if a == current_agent => Message::Constant(Constant::Intruder),
                Variable::SymmetricKey(_) | Variable::Number(_) | Variable::Actor(_) => {
                    Message::Variable(var.clone())
                }
            },
            Message::Constant(_) => self.clone(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|msg| msg.replace_agent_with_intruder(current_agent))
                    .collect(),
            },
            Message::Tuple(args) => Message::Tuple(
                args.iter()
                    .map(|msg| msg.replace_agent_with_intruder(current_agent))
                    .collect(),
            ),
        }
    }
}
