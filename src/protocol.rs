use std::hash::Hash;

use indexmap::{indexset, IndexMap, IndexSet};
use itertools::Itertools;
use macor_parse::ast::{self, Ident};
use smol_str::SmolStr;

use crate::typing::{Stage, Type, TypedStage, TypingContext, TypingError, UntypedStage};

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct AgentName(pub Ident<SmolStr>);

impl std::fmt::Debug for AgentName {
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
pub enum Term<S: Stage = TypedStage> {
    Variable(S::Variable),
    Constant(S::Constant),
    Composition { func: Func, args: Vec<Term<S>> },
    Tuple(Vec<Term<S>>),
}

impl<S: Stage> std::fmt::Debug for Term<S> {
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

impl<'a> From<ast::Term<&'a str>> for Term<UntypedStage<'a>> {
    fn from(term: ast::Term<&'a str>) -> Self {
        match term {
            ast::Term::Var(var) => Self::Variable(var.clone()),
            ast::Term::Fun(name, args) => match name.as_str() {
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
            ast::Term::SymEnc(terms, key) => Self::Composition {
                func: Func::SymEnc,
                args: vec![
                    Term::Tuple(terms.into_iter().map_into().collect()),
                    Term::from(*key),
                ],
            },
            ast::Term::AsymEnc(terms, key) => Self::Composition {
                func: Func::AsymEnc,
                args: vec![
                    Term::Tuple(terms.into_iter().map_into().collect()),
                    Term::from(*key),
                ],
            },
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Variable {
    Agent(AgentName),
    SymmetricKey(Ident<SmolStr>),
    Number(Ident<SmolStr>),
}
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Constant {
    Intruder,
    Agent(AgentName),
    Function(Func),
    Nonce(Nonce),
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Nonce(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
    pub terms: Vec<Term>,
}

impl FromIterator<Term> for Message {
    fn from_iter<T: IntoIterator<Item = Term>>(iter: T) -> Self {
        Message {
            terms: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Message {
    type Item = Term;

    type IntoIter = std::vec::IntoIter<Term>;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter()
    }
}
impl<'a> IntoIterator for &'a Message {
    type Item = &'a Term;

    type IntoIter = std::slice::Iter<'a, Term>;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.iter()
    }
}
impl Message {
    pub fn iter(&self) -> impl Iterator<Item = &Term> {
        self.into_iter()
    }
}

#[derive(Eq, Debug, Clone, PartialOrd, Ord, Hash, PartialEq)]
pub struct Knowledge<S: Stage = TypedStage>(Vec<Term<S>>);

impl Knowledge<TypedStage> {
    fn new(terms: Vec<Term<TypedStage>>) -> Self {
        Knowledge(terms)
    }
    pub fn iter(&self) -> impl Iterator<Item = &Term<TypedStage>> {
        self.0.iter()
    }
}

impl<'a> From<&[ast::Term<&'a str>]> for Knowledge<UntypedStage<'a>> {
    fn from(knowledge: &[ast::Term<&'a str>]) -> Self {
        Self(knowledge.iter().cloned().map(Term::from).collect())
    }
}

impl Knowledge<UntypedStage<'_>> {
    pub fn to_typed(&self, ctx: &mut TypingContext) -> Knowledge<TypedStage> {
        let terms: Vec<Term<TypedStage>> = self.0.iter().map(|term| term.to_typed(ctx)).collect();

        Knowledge::<TypedStage>::new(terms)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Ingoing,
    Outgoing,
}
impl Direction {
    pub fn is_outgoing(&self) -> bool {
        matches!(self, Direction::Outgoing)
    }
    pub fn is_ingoing(&self) -> bool {
        matches!(self, Direction::Ingoing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessagePattern {
    pub from: AgentName,
    pub to: AgentName,
    pub direction: Direction,
    pub message: Message,
    pub initiates: IndexSet<Variable>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AgentKind {
    Variable,
    Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolAgent {
    pub name: AgentName,
    pub kind: AgentKind,
    pub initial_knowledge: Knowledge,
    pub messages: Vec<MessagePattern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Goal<A> {
    SecretBetween(Vec<A>, Vec<Term>),
    Authenticates(A, A, Vec<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Protocol {
    pub agents: Vec<ProtocolAgent>,
    pub initiations: IndexMap<Variable, AgentName>,
    pub goals: Vec<Goal<AgentName>>,
}

struct ProtocolAction {
    from: Ident<SmolStr>,
    to: Ident<SmolStr>,
    terms: Vec<Term>,
    initiates: IndexSet<Variable>,
}

impl Protocol {
    fn new_protocol_agent(
        agent: &Ident<SmolStr>,
        knowledge: &[ast::Term<&str>],
        ctx: &mut TypingContext,
        actions: &[ProtocolAction],
    ) -> ProtocolAgent {
        let k: Knowledge<UntypedStage> = knowledge.into();

        ProtocolAgent {
            name: AgentName(agent.convert()),
            kind: if agent.is_constant() {
                AgentKind::Constant
            } else {
                AgentKind::Variable
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

                    Some(MessagePattern {
                        from: AgentName(a.from.convert()),
                        to: AgentName(a.to.convert()),
                        direction,
                        message: a.terms.iter().cloned().collect(),
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
                terms: action
                    .terms
                    .iter()
                    .map(|term| Term::<UntypedStage>::from(term.clone()).to_typed(&mut ctx))
                    .collect(),
                initiates: Default::default(),
            })
            .collect_vec();

        let mut initiations = IndexMap::default();
        let mut seen = IndexSet::<Variable>::default();

        for action in &mut actions {
            let in_this: IndexSet<Variable> = action
                .terms
                .iter()
                .flat_map(|term| term.extract_variables())
                .collect();

            action.initiates = in_this.difference(&seen).cloned().collect();
            for var in &action.initiates {
                initiations.insert(var.clone(), AgentName(action.from.clone()));
            }
            seen.extend(in_this.into_iter());
        }

        let agents = document
            .knowledge
            .agents
            .iter()
            .map(|(agent, knowledge)| {
                Self::new_protocol_agent(&agent.convert(), knowledge, &mut ctx, &actions)
            })
            .collect();

        let goals = document
            .goals
            .iter()
            .map(|goal| match goal {
                ast::Goal::Authenticates {
                    a,
                    b,
                    terms,
                    weakly: _,
                } => Goal::Authenticates(
                    AgentName(a.convert()),
                    AgentName(b.convert()),
                    terms
                        .iter()
                        .map(|term| Term::<UntypedStage>::from(term.clone()).to_typed(&mut ctx))
                        .collect(),
                ),
                ast::Goal::SecretBetween {
                    terms,
                    agents,
                    guessable: _,
                } => Goal::SecretBetween(
                    agents
                        .iter()
                        .map(|agent| AgentName(agent.convert()))
                        .collect(),
                    terms
                        .iter()
                        .map(|term| Term::<UntypedStage>::from(term.clone()).to_typed(&mut ctx))
                        .collect(),
                ),
            })
            .collect();

        if ctx.errors.is_empty() {
            Ok(Protocol {
                agents,
                initiations,
                goals,
            })
        } else {
            Err(ctx.errors)
        }
    }
}
impl<T> Func<T> {
    pub fn map<S>(&self, f: impl FnOnce(&T) -> S) -> Func<S> {
        match self {
            Func::SymEnc => Func::SymEnc,
            Func::AsymEnc => Func::AsymEnc,
            Func::Exp => Func::Exp,
            Func::Inv => Func::Inv,
            Func::User(c) => Func::User(f(c)),
        }
    }
}
impl Term {
    fn extract_variables(&self) -> IndexSet<Variable> {
        match self {
            Term::Variable(v) => indexset! { v.clone() },
            Term::Constant(_) => indexset! {},
            Term::Composition { func: _, args } => args
                .iter()
                .flat_map(|arg| arg.extract_variables())
                .collect(),
            Term::Tuple(ts) => ts.iter().flat_map(|t| t.extract_variables()).collect(),
        }
    }

    pub(crate) fn replace_agent_with_intruder(&self, current_agent: &AgentName) -> Self {
        match self {
            Term::Variable(var) => match var {
                Variable::Agent(a) if a == current_agent => Term::Constant(Constant::Intruder),
                Variable::SymmetricKey(_) | Variable::Number(_) | Variable::Agent(_) => {
                    Term::Variable(var.clone())
                }
            },
            Term::Constant(_) => self.clone(),
            Term::Composition { func, args } => Term::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|term| term.replace_agent_with_intruder(current_agent))
                    .collect(),
            },
            Term::Tuple(args) => Term::Tuple(
                args.iter()
                    .map(|term| term.replace_agent_with_intruder(current_agent))
                    .collect(),
            ),
        }
    }

    pub fn span(&self) -> Option<miette::SourceSpan> {
        match self {
            Term::Variable(var) => match var {
                Variable::Agent(a) => Some(a.span()),
                Variable::SymmetricKey(n) | Variable::Number(n) => Some(n.span()),
            },
            Term::Constant(c) => match c {
                Constant::Intruder => None,
                Constant::Agent(a) => Some(a.span()),
                Constant::Function(f) => match f {
                    Func::SymEnc | Func::AsymEnc | Func::Exp | Func::Inv => None,
                    Func::User(f) => Some(f.span()),
                },
                Constant::Nonce(_) => None,
            },
            Term::Composition { func, args } => args.iter().filter_map(|arg| arg.span()).fold(
                match func {
                    Func::SymEnc => None,
                    Func::AsymEnc => None,
                    Func::Exp => None,
                    Func::Inv => None,
                    Func::User(u) => Some(u.span()),
                },
                |a, b| match a {
                    Some(a) => Some(join_span(a, b)),
                    None => Some(b),
                },
            ),
            Term::Tuple(_) => todo!(),
        }
    }
}

fn join_span(a: miette::SourceSpan, b: miette::SourceSpan) -> miette::SourceSpan {
    let start = a.offset().min(b.offset());
    let end = (a.offset() + a.len()).max(b.offset() + b.len());

    (start, end).into()
}
impl AgentName {
    pub fn span(&self) -> miette::SourceSpan {
        self.0 .1
    }
    pub fn is_constant(&self) -> bool {
        self.0.is_constant()
    }
}
