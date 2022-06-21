use std::collections::VecDeque;

use crate::dolev_yao::{augment_knowledge, can_derive};
use crate::protocol::{self, Direction, Func, Protocol, SessionId};

use ena::unify::{InPlaceUnificationTable, UnifyKey, UnifyValue};
use indexmap::IndexMap;
use itertools::Itertools;
use macor_parse::ast::Ident;
use smol_str::SmolStr;

type Rc<T> = std::sync::Arc<T>;

#[derive(Clone, Copy, Eq)]
pub struct ConstantId(u32, pub Option<&'static str>);

impl std::hash::Hash for ConstantId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialOrd for ConstantId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for ConstantId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialEq for ConstantId {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::fmt::Debug for ConstantId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.1 {
            write!(f, "!{}[{}]", name, self.0)
        } else {
            write!(f, "![{}]", self.0)
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct MessageId(u32);

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Actor {
    Actor(Option<SmolStr>),
    Intruder,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Message<M> {
    Variable(Option<SmolStr>),
    Agent(Actor),
    Constant(ConstantId),
    Composition(Func<ConstantId>, Vec<M>),
    Tuple(Vec<M>),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FullMessage(pub Message<FullMessage>);

impl<M: std::fmt::Debug> std::fmt::Debug for Message<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(arg0) => {
                if let Some(k) = arg0 {
                    write!(f, "Var({})", k)
                } else {
                    write!(f, "Var")
                }
            }
            Self::Agent(Actor::Intruder) => {
                write!(f, "Agent(i)")
            }
            Self::Agent(Actor::Actor(arg0)) => {
                if let Some(k) = arg0 {
                    write!(f, "Agent({})", k)
                } else {
                    write!(f, "Agent")
                }
            }
            Self::Constant(c) => c.fmt(f),
            Self::Composition(fun, args) => {
                if let Func::User(x) = fun {
                    write!(f, "{:?}({:?})", x, args.iter().format(", "))
                } else {
                    write!(f, "{:?}({:?})", fun, args.iter().format(", "))
                }
            }
            Self::Tuple(arg0) => write!(f, ":({:?})", arg0.iter().format(", ")),
        }
    }
}

impl std::fmt::Debug for FullMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl UnifyValue for Message<MessageId> {
    type Error = ();

    fn unify_values(l: &Self, r: &Self) -> Result<Self, Self::Error> {
        use self::Message::*;

        Ok(match (l, r) {
            (Variable(l), Variable(r)) => Variable(l.clone().or_else(|| r.clone())),
            (Variable(_), Agent(c)) | (Agent(c), Variable(_)) => Agent(c.clone()),
            (Variable(_), Constant(c)) | (Constant(c), Variable(_)) => Constant(*c),
            (Variable(_), Composition(func, args)) | (Composition(func, args), Variable(_)) => {
                Composition(func.clone(), args.clone())
            }
            (Variable(_), Tuple(ts)) | (Tuple(ts), Variable(_)) => Tuple(ts.clone()),
            (Agent(Actor::Intruder), Agent(a)) | (Agent(a), Agent(Actor::Intruder)) => {
                Agent(Actor::Intruder)
            }
            (Agent(Actor::Actor(l)), Agent(Actor::Actor(r))) => {
                Agent(Actor::Actor(l.clone().or_else(|| r.clone())))
            }
            (Constant(l), Constant(r)) => {
                if l == r {
                    Constant(*l)
                } else {
                    return Err(());
                }
            }
            (Composition(l_func, l_args), Composition(r_func, r_args)) => {
                if l_func != r_func && l_args.len() == r_args.len() {
                    return Err(());
                } else {
                    Composition(l_func.clone(), l_args.clone())
                }
            }
            (Tuple(ls), Tuple(rs)) => {
                if ls.len() != rs.len() {
                    return Err(());
                } else {
                    Tuple(ls.clone())
                }
            }
            _ => return Err(()),
        })
    }
}

impl UnifyKey for MessageId {
    type Value = Message<MessageId>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        MessageId(u)
    }

    fn tag() -> &'static str {
        "MessageId"
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct VariableKey {
    session_id: SessionId,
    actor: SmolStr,
    variable: SmolStr,
}

#[derive(Debug, Clone)]
pub struct Unifier {
    intruder: MessageId,
    table: InPlaceUnificationTable<MessageId>,
}

impl Default for Unifier {
    fn default() -> Self {
        let mut table = InPlaceUnificationTable::default();
        let intruder = table.new_key(Message::Agent(Actor::Intruder));
        Self { intruder, table }
    }
}

impl Unifier {
    pub fn intruder(&self) -> MessageId {
        self.intruder
    }
    /// Recursively unifies the two messages and returns either of the passed
    /// messages if they indeed do unify (since they are now equivalent), or
    /// else produces and error.
    pub fn unify(&mut self, l: MessageId, r: MessageId) -> Result<MessageId, ()> {
        use self::Message::*;

        Ok(
            match (self.table.probe_value(l), self.table.probe_value(r)) {
                (Variable(_), _) | (_, Variable(_)) => {
                    self.table.unify_var_var(l, r)?;
                    l
                }
                (Agent(_), Agent(_)) => {
                    self.table.unify_var_var(l, r)?;
                    l
                }
                (Constant(x), Constant(y)) => {
                    if x == y {
                        l
                    } else {
                        return Err(());
                    }
                }
                (Composition(l_func, l_args), Composition(r_func, r_args)) => {
                    if l_func != r_func || l_args.len() != r_args.len() {
                        // eprintln!(
                        //     "Attempted to unify {:?} with {:?}",
                        //     self.resolve_full(l),
                        //     self.resolve_full(r)
                        // );
                        return Err(());
                    } else {
                        for (l_arg, r_arg) in l_args.into_iter().zip_eq(r_args) {
                            self.unify(l_arg, r_arg)?;
                        }
                        self.table.unify_var_var(l, r)?;
                        l
                    }
                }
                (Tuple(ls), Tuple(rs)) => {
                    if ls.len() != rs.len() {
                        return Err(());
                    } else {
                        for (l_arg, r_arg) in ls.into_iter().zip_eq(rs) {
                            self.unify(l_arg, r_arg)?;
                        }
                        self.table.unify_var_var(l, r)?;
                        l
                    }
                }
                _ => return Err(()),
            },
        )
    }
    pub fn are_unified(&mut self, a: MessageId, b: MessageId) -> bool {
        // TODO: Should this be recursive?
        if self.table.unioned(a, b) {
            return true;
        }

        // println!(
        //     "Are unified: {:?} with {:?}",
        //     self.resolve_full(a),
        //     self.resolve_full(b)
        // );

        false
    }
    // pub fn try_unify(&mut self, a: MessageId, b: MessageId) -> Result<(), ()> {
    //     // TODO: Should this be recursive?
    //     if self.table.unioned(a, b) {
    //         return Ok(());
    //     }

    //     use self::Message::*;

    //     match (self.probe_value(a), self.probe_value(b)) {
    //         (Message::Agent(_))
    //         (a, b) => todo!("{:?} {:?}", a, b),
    //     }

    //     // println!(
    //     //     "Are unified: {:?} with {:?}",
    //     //     self.resolve_full(a),
    //     //     self.resolve_full(b)
    //     // );

    //     false
    // }

    pub fn probe_value(&mut self, id: MessageId) -> Message<MessageId> {
        self.table.probe_value(id)
    }

    pub fn resolve_full(&mut self, id: MessageId) -> FullMessage {
        match self.table.probe_value(id) {
            Message::Variable(v) => FullMessage(Message::Variable(v)),
            Message::Agent(v) => FullMessage(Message::Agent(v)),
            Message::Constant(c) => FullMessage(Message::Constant(c)),
            Message::Composition(func, args) => FullMessage(Message::Composition(
                func,
                args.into_iter().map(|arg| self.resolve_full(arg)).collect(),
            )),
            Message::Tuple(ts) => FullMessage(Message::Tuple(
                ts.into_iter().map(|t| self.resolve_full(t)).collect(),
            )),
        }
    }
}

#[test]
fn very_basic_unification() -> Result<(), ()> {
    let mut unifier = Unifier::default();

    let a = unifier
        .table
        .new_key(Message::Constant(ConstantId(0, None)));
    let b = unifier.table.new_key(Message::Variable(None));

    unifier.unify(a, b)?;

    assert_eq!(unifier.table.probe_value(a), unifier.table.probe_value(b));
    assert_eq!(
        unifier.table.probe_value(b),
        Message::Constant(ConstantId(0, None))
    );

    Ok(())
}

#[test]
fn less_basic_unification() -> Result<(), ()> {
    let mut unifier = Unifier::default();

    let x = unifier
        .table
        .new_key(Message::Constant(ConstantId(0, None)));
    let y = unifier
        .table
        .new_key(Message::Constant(ConstantId(1, None)));

    let y_free = unifier.table.new_key(Message::Variable(None));
    let x_free = unifier.table.new_key(Message::Variable(None));

    let a = unifier.table.new_key(Message::Tuple(vec![x, y_free]));
    let b = unifier.table.new_key(Message::Tuple(vec![x_free, y]));

    unifier.unify(a, b)?;

    assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
    assert_eq!(
        unifier.resolve_full(a),
        FullMessage(Message::Tuple(vec![
            FullMessage(Message::Constant(ConstantId(0, None))),
            FullMessage(Message::Constant(ConstantId(1, None)))
        ]))
    );

    Ok(())
}

#[test]
fn unify_simple_composition() -> Result<(), ()> {
    let mut unifier = Unifier::default();

    let x = unifier
        .table
        .new_key(Message::Constant(ConstantId(0, None)));
    let y = unifier
        .table
        .new_key(Message::Constant(ConstantId(1, None)));

    let x_free = unifier.table.new_key(Message::Variable(None));
    let y_free = unifier.table.new_key(Message::Variable(None));

    let a = unifier
        .table
        .new_key(Message::Composition(Func::Exp, vec![x, y_free]));
    let b = unifier
        .table
        .new_key(Message::Composition(Func::Exp, vec![x_free, y]));

    unifier.unify(a, b)?;

    assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
    assert_eq!(
        unifier.resolve_full(a),
        FullMessage(Message::Composition(
            Func::Exp,
            vec![
                FullMessage(Message::Constant(ConstantId(0, None))),
                FullMessage(Message::Constant(ConstantId(1, None)))
            ]
        ))
    );

    Ok(())
}

#[test]
fn non_unification() {
    let mut unifier = Unifier::default();

    let x = unifier
        .table
        .new_key(Message::Constant(ConstantId(0, None)));
    let y = unifier
        .table
        .new_key(Message::Constant(ConstantId(1, None)));

    let free = unifier.table.new_key(Message::Variable(None));

    let a = unifier.table.new_key(Message::Tuple(vec![y, free]));
    let b = unifier.table.new_key(Message::Tuple(vec![x, y]));

    assert_eq!(unifier.unify(a, b), Err(()));
}

#[test]
fn branching_unification() -> Result<(), ()> {
    let mut unifier = Unifier::default();

    let x = unifier
        .table
        .new_key(Message::Constant(ConstantId(1, None)));
    let y = unifier
        .table
        .new_key(Message::Constant(ConstantId(2, None)));

    let a = unifier.table.new_key(Message::Variable(None));

    let mut world_1 = unifier.clone();
    let mut world_2 = unifier.clone();

    world_1.unify(a, x)?;
    world_2.unify(a, y)?;

    assert_eq!(
        world_1.resolve_full(a),
        FullMessage(Message::Constant(ConstantId(1, None)))
    );
    assert_eq!(
        world_2.resolve_full(a),
        FullMessage(Message::Constant(ConstantId(2, None)))
    );

    Ok(())
}

// Numbers: NA
//
// A->s: NA
//              A fixes the value of NA to be whatever
//                  .... s does not know what NA is yet ....
//              s receives its first occurrence of NA, and fixes it to that value
// s->B: NA
//              s sends its fixed value of NA to B
//              B fixes the value to what ever it was told
// B->A: NA
//              B sends the fixed value of NA to A
//              A already knows the value of NA, and thus can check its validity
//
// A->s: n1
//              s needs to unify the expected message with the received:
//              unify(n1, NA)

// A->: NA_a (1)
// ->A: NA_a (3)

// ->s: NA_s (1)
// s->: NA_s (2)

// ->B: NA_b (2)
// B->: NA_b (3)

#[derive(Debug)]
pub struct Converter<'a> {
    unifier: &'a mut Unifier,
    mappings: &'a mut Mappings,
}

#[derive(Debug, Clone)]
pub struct Mappings {
    next_constant: u32,
    actor_table: IndexMap<(Option<SessionId>, SmallStr), MessageId>,
    func_table: IndexMap<Func, MessageId>,
    global_constant_table: IndexMap<SmallStr, MessageId>,
    constant_table: IndexMap<VariableKey, MessageId>,
    variable_table: IndexMap<VariableKey, MessageId>,
}

impl Default for Mappings {
    fn default() -> Self {
        Self {
            next_constant: 1,
            actor_table: Default::default(),
            func_table: Default::default(),
            global_constant_table: Default::default(),
            constant_table: Default::default(),
            variable_table: Default::default(),
        }
    }
}

impl<'a> Converter<'a> {
    pub fn new(unifier: &'a mut Unifier, mappings: &'a mut Mappings) -> Self {
        Self { unifier, mappings }
    }

    pub fn get_actor(
        &mut self,
        session_id: Option<SessionId>,
        agent: &protocol::ActorName,
    ) -> MessageId {
        // TODO: ???
        // let mid = self.register_global_constant(agent);
        // if let Message::Constant(c) = self.unifier.table.probe_value(mid) {
        //     c
        // } else {
        //     unreachable!()
        // }
        // TODO: ???
        *self
            .mappings
            .actor_table
            .entry((session_id, agent.0.clone().into()))
            .or_insert_with(|| match session_id {
                _ if agent.0.is_constant() => {
                    let cid = ConstantId(
                        self.mappings.next_constant,
                        Some(leak_str(agent.0.as_str())),
                    );
                    self.mappings.next_constant += 1;
                    self.unifier.table.new_key(Message::Constant(cid))
                }
                Some(session_id) => self.unifier.table.new_key(Message::Agent(Actor::Actor(Some(
                    format!("{}_{}", agent.0, session_id.0).into(),
                )))),
                _ => self.unifier.table.new_key(Message::Agent(Actor::Actor(Some(
                    format!("{}", agent.0).into(),
                )))),
            })
    }
    pub fn get_function_constant(&mut self, func: Func) -> MessageId {
        *self
            .mappings
            .func_table
            .entry(func.clone())
            .or_insert_with(|| {
                let cid = ConstantId(
                    self.mappings.next_constant,
                    Some(leak_str(&format!("{:?}", func))),
                );
                self.mappings.next_constant += 1;
                self.unifier.table.new_key(Message::Constant(cid))
            })
    }

    pub fn register_global_constant(&mut self, constant_name: &str) -> ConstantId {
        let msg = self.register_global_constant_msg(constant_name);
        match self.unifier.table.probe_value(msg) {
            Message::Constant(c) => c,
            _ => unreachable!(),
        }
    }
    pub fn register_global_constant_msg(&mut self, constant_name: &str) -> MessageId {
        *self
            .mappings
            .global_constant_table
            .entry(constant_name.into())
            .or_insert_with(|| {
                let cid = ConstantId(self.mappings.next_constant, Some(leak_str(constant_name)));
                self.mappings.next_constant += 1;
                self.unifier.table.new_key(Message::Constant(cid))
            })
    }
    pub fn register_constant(
        &mut self,
        agent: &Ident<SmallStr>,
        session_id: SessionId,
        constant_name: &Ident<SmallStr>,
    ) -> MessageId {
        *self
            .mappings
            .constant_table
            .entry(VariableKey {
                session_id,
                actor: agent.into(),
                variable: constant_name.into(),
            })
            .or_insert_with(|| {
                let cid = ConstantId(
                    self.mappings.next_constant,
                    Some(leak_str(&format!(
                        "{}@{}:{}",
                        agent, session_id.0, constant_name
                    ))),
                );
                self.mappings.next_constant += 1;
                self.unifier.table.new_key(Message::Constant(cid))
            })
    }
    pub fn register_variable(
        &mut self,
        agent: &Ident<SmallStr>,
        session_id: SessionId,
        variable_name: &Ident<SmallStr>,
    ) -> MessageId {
        *self
            .mappings
            .variable_table
            .entry(VariableKey {
                session_id,
                actor: agent.into(),
                variable: variable_name.into(),
            })
            .or_insert_with(|| {
                self.unifier.table.new_key(Message::Variable(Some(
                    format!("{}@{}:{}", agent, session_id.0, variable_name).into(),
                )))
            })
    }

    fn initiate_typed_variable(
        &mut self,
        agent: Option<&Ident<SmallStr>>,
        session_id: SessionId,
        initators: &IndexMap<protocol::Variable, protocol::ActorName>,
        var: &protocol::Variable,
    ) -> MessageId {
        match var {
            protocol::Variable::Actor(a) => {
                // TODO: Is it fine to register agents like this??
                // self.register_variable(agent, session_id, a.0.as_str())
                // TODO: For now, just fix them so that A will always be A, and B always B
                self.get_actor(Some(session_id), a)
            }
            protocol::Variable::SymmetricKey(n) | protocol::Variable::Number(n) => {
                match (initators.get(var), agent) {
                    (Some(initator), Some(agent)) if &initator.0 == agent => {
                        self.register_constant(agent, session_id, &n.convert())
                    }
                    (Some(_), Some(agent)) => {
                        self.register_variable(agent, session_id, &n.convert())
                    }
                    (Some(initator), None) => {
                        self.register_constant(&initator.0, session_id, &n.convert())
                    }
                    _ => todo!("Variable {:?} ({:?})", var, initators),
                    // _ => self.register_variable(agent, session_id, &n.convert()),
                }
            }
        }
    }
    fn register_typed_message(
        &mut self,
        agent: Option<&Ident<SmallStr>>,
        session_id: SessionId,
        initiations: &IndexMap<protocol::Variable, protocol::ActorName>,
        msg: protocol::Message,
    ) -> MessageId {
        match msg {
            protocol::Message::Variable(var) => {
                self.initiate_typed_variable(agent, session_id, initiations, &var)
            }
            protocol::Message::Constant(c) => match c {
                protocol::Constant::Actor(a) => self.get_actor(Some(session_id), &a),
                protocol::Constant::Function(f) => self.get_function_constant(f),
                protocol::Constant::Intruder => self.unifier.intruder(),
                protocol::Constant::Nonce(_) => todo!(),
            },
            protocol::Message::Composition { func, args } => {
                let msg = Message::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant(u.as_str())),
                    },
                    args.into_iter()
                        .map(|arg| self.register_typed_message(agent, session_id, initiations, arg))
                        .collect(),
                );
                self.unifier.table.new_key(msg)
            }
            protocol::Message::Tuple(ts) => {
                let msg = Message::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_typed_message(agent, session_id, initiations, t))
                        .collect(),
                );
                self.unifier.table.new_key(msg)
            }
        }
    }
    pub fn register_ast_message(
        &mut self,
        msg: protocol::Message<crate::typing::UntypedStage>,
    ) -> MessageId {
        match msg {
            protocol::Message::Variable(v) => self.register_global_constant_msg(v.as_str()),
            protocol::Message::Constant(_) => unreachable!(),
            protocol::Message::Composition { func, args } => {
                let msg = Message::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant(u.as_str())),
                    },
                    args.into_iter()
                        .map(|t| self.register_ast_message(t))
                        .collect(),
                );

                self.unifier.table.new_key(msg)
            }
            protocol::Message::Tuple(ts) => {
                let msg = Message::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_ast_message(t))
                        .collect(),
                );
                self.unifier.table.new_key(msg)
            }
        }
    }
}

fn leak_str(s: &str) -> &'static str {
    Box::leak(s.to_string().into_boxed_str())
}

#[derive(Debug, Clone, Default)]
pub struct Knowledge(pub Vec<MessageId>);

type SmallStr = SmolStr;

#[derive(Debug, Clone)]
struct Transaction {
    sender: MessageId,
    receiver: MessageId,
    direction: Direction,
    messages: Vec<MessageId>,
}

#[derive(Debug, Clone)]
struct SessionActor {
    name: Ident<SmallStr>,
    actor_id: MessageId,
    initial_knowledge: Knowledge,
    strand: Vec<Transaction>,
}

#[derive(Debug, Clone)]
struct Secret {
    between_actors: Vec<MessageId>,
    msg: MessageId,
}

#[derive(Debug, Clone)]
pub struct Session {
    session_id: SessionId,
    actors: Vec<SessionActor>,
    secrets: Vec<Secret>,
}

impl Session {
    pub fn new(protocol: &Protocol, session_id: SessionId, converter: &mut Converter) -> Session {
        let actors = protocol
            .actors
            .iter()
            .map(|actor| {
                let mut initial_knowledge: Vec<_> = actor
                    .initial_knowledge
                    .iter()
                    .map(|msg| {
                        converter.register_typed_message(
                            Some(&actor.name.0),
                            session_id,
                            &protocol.initiations,
                            msg.clone(),
                        )
                    })
                    .collect();

                let initiates = actor.messages.iter().flat_map(|pattern| {
                    if pattern.direction == Direction::Outgoing {
                        pattern.initiates.clone()
                    } else {
                        Default::default()
                    }
                });

                initial_knowledge.extend(initiates.map(|var| {
                    converter.initiate_typed_variable(
                        Some(&actor.name.0),
                        session_id,
                        &protocol.initiations,
                        &var,
                    )
                }));

                initial_knowledge.sort_unstable_by_key(|msg| converter.unifier.resolve_full(*msg));
                initial_knowledge.dedup();

                SessionActor {
                    name: actor.name.0.clone(),
                    actor_id: converter.get_actor(Some(session_id), &actor.name),
                    initial_knowledge: Knowledge(initial_knowledge),
                    strand: actor
                        .messages
                        .iter()
                        .map(|pattern| Transaction {
                            sender: converter.get_actor(Some(session_id), &pattern.from),
                            receiver: converter.get_actor(Some(session_id), &pattern.to),
                            direction: pattern.direction,
                            messages: pattern
                                .packet
                                .iter()
                                .map(|msg| {
                                    converter.register_typed_message(
                                        Some(&actor.name.0),
                                        session_id,
                                        &protocol.initiations,
                                        msg.clone(),
                                    )
                                })
                                .collect_vec(),
                        })
                        .collect(),
                }
            })
            .collect_vec();

        let secrets = protocol
            .goals
            .iter()
            .flat_map(|goal| match goal {
                protocol::Goal::SecretBetween(agents, msgs) => msgs
                    .iter()
                    .map(|msg| Secret {
                        between_actors: agents
                            .iter()
                            .map(|agent| converter.get_actor(Some(session_id), agent))
                            .collect(),
                        msg: converter.register_typed_message(
                            None,
                            session_id,
                            &protocol.initiations,
                            msg.clone(),
                        ),
                    })
                    .collect_vec(),
                protocol::Goal::Authenticates(_, _, _) => {
                    eprintln!("⚠️ no authentication goals");
                    vec![]
                }
            })
            .collect();

        Session {
            session_id,
            actors,
            secrets,
        }
    }
    pub fn print(&self, unifier: &mut Unifier) {
        println!();
        println!("#############");
        println!("# SESSION {} #", self.session_id.0);
        println!("#############");
        for actor in &self.actors {
            println!();
            println!("> {} ({:?})", actor.name, actor.actor_id);
            println!(
                "> IK: {:?}",
                actor
                    .initial_knowledge
                    .0
                    .iter()
                    .map(|&msg| unifier.resolve_full(msg))
                    .collect_vec()
            );
            for t in &actor.strand {
                println!(
                    ">> {:?}->{:?}: {:?}",
                    unifier.resolve_full(t.sender),
                    unifier.resolve_full(t.receiver),
                    t.messages
                        .iter()
                        .map(|&msg| unifier.resolve_full(msg))
                        .collect_vec()
                );
            }
        }
        println!();
        println!("=== SECRETS ===");
        for secret in &self.secrets {
            println!("{:?}", unifier.resolve_full(secret.msg));
        }
    }
}

impl Knowledge {
    fn can_construct(&self, unifier: &mut Unifier, msg: MessageId) -> bool {
        // eprintln!(
        //     "Can construct {:?} with knowledge {:?}",
        //     unifier.resolve_full(msg),
        //     self.0
        //         .iter()
        //         .map(|&msg| unifier.resolve_full(msg))
        //         .format(", ")
        // );
        if self.0.iter().any(|&k| unifier.table.unioned(k, msg)) {
            return true;
        }

        if can_derive(self, msg, unifier) {
            return true;
        }

        false
    }
}

#[derive(Debug, Clone)]
struct ExecutionActorState {
    actor_id: MessageId,
    current_execution: usize,
    inbox: VecDeque<Vec<MessageId>>,
}

#[derive(Debug, Clone)]
struct ExecutionSessionState {
    actors: Vec<ExecutionActorState>,
}

#[derive(Debug, Clone)]
pub struct TraceEntry {
    session: SessionId,
    pub sender: Option<(SmolStr, MessageId)>,
    pub receiver: Option<(SmolStr, MessageId)>,
    pub messages: Vec<MessageId>,
}

#[derive(Debug, Clone, Default)]
struct Intruder {
    knowledge: Knowledge,
    constraints: Vec<Rc<(Knowledge, Vec<MessageId>)>>,
}

impl Intruder {
    fn has_achieved_goal(&mut self, sessions: &[Session], unifier: &mut Unifier) -> bool {
        sessions.iter().any(|sess| {
            sess.secrets.iter().any(|secret| {
                let mut knowledge = self.knowledge.clone();
                let mut new_unifier = unifier.clone();

                let intruder_id = unifier.intruder();
                augment_knowledge(&mut knowledge, &mut new_unifier);

                println!(
                    "Trying to construct {:?} with knowledge [{:?}]\n",
                    new_unifier.resolve_full(secret.msg),
                    knowledge
                        .0
                        .iter()
                        .map(|msg| new_unifier.resolve_full(*msg))
                        .format(", ")
                );

                for &k in &knowledge.0 {
                    if new_unifier.unify(secret.msg, k).is_ok()
                    // if self.knowledge.can_construct(unifier, secret)
                        && self.conforms_to_constraints_without_augment(&mut new_unifier) && !secret
                        .between_actors
                        .iter()
                        .any(|agent| new_unifier.are_unified(intruder_id, *agent))
                    {
                        eprintln!(
                            "LEAKED {:?} ({:?})",
                            new_unifier.resolve_full(secret.msg),
                            unifier.resolve_full(k)
                        );
                        *unifier = new_unifier;
                        return true;
                    }
                }

                // eprintln!("check with dolev-yao if secret is derivable");

                false
            })
        })
    }

    fn conforms_to_constraints_without_augment(&self, unifier: &mut Unifier) -> bool {
        self.constraints
            .iter()
            .map(|r| r.as_ref())
            .all(|(k, msgs)| msgs.iter().all(|&msg| k.can_construct(unifier, msg)))
    }

    fn conforms_to_constraints(&mut self, unifier: &mut Unifier) -> bool {
        augment_knowledge(&mut self.knowledge, unifier);
        self.conforms_to_constraints_without_augment(unifier)
    }
}

#[derive(Debug, Clone)]
pub struct Execution {
    pub unifier: Unifier,
    intruder: Option<Intruder>,
    states: Vec<ExecutionSessionState>,
    sessions: Rc<Vec<Session>>,
    pub trace: Vec<Rc<TraceEntry>>,
}

impl Execution {
    pub fn new(
        protocol: &Protocol,
        mappings: &mut Mappings,
        mut unifier: Unifier,
        sessions: Rc<Vec<Session>>,
    ) -> Self {
        let states = sessions
            .iter()
            .map(|session| ExecutionSessionState {
                actors: session
                    .actors
                    .iter()
                    .map(|actor| ExecutionActorState {
                        actor_id: actor.actor_id,
                        current_execution: 0,
                        inbox: Default::default(),
                    })
                    .collect(),
            })
            .collect();

        let mut converter = Converter::new(&mut unifier, mappings);
        let mut intruder = Intruder::default();
        for session in sessions.iter() {
            for actor in &protocol.actors {
                if actor.name.0.is_constant() {
                    continue;
                }

                for msg in &actor.initial_knowledge.0 {
                    let msg = msg.replace_agent_with_intruder(&actor.name);
                    let registered_msg = converter.register_typed_message(
                        // TODO: ???
                        None,
                        session.session_id,
                        &protocol.initiations,
                        msg.clone(),
                    );

                    if intruder
                        .knowledge
                        .0
                        .iter()
                        .all(|&msg| !converter.unifier.are_unified(msg, registered_msg))
                    {
                        intruder.knowledge.0.push(registered_msg);
                    }
                }
            }
        }

        Execution {
            unifier,
            intruder: Some(intruder),
            sessions,
            states,
            trace: vec![],
        }
    }

    pub fn possible_next(&self) -> impl Iterator<Item = Execution> + '_ {
        // NOTE: Under the assumption that all sessions are initially
        // equivalent, never progress sessions which come after sessions which
        // have not been started yet.
        let num_used_sessions = self
            .trace
            .iter()
            .map(|trace| trace.session)
            .sorted()
            .dedup()
            .count();

        self.sessions
            .iter()
            .zip_eq(self.states.iter())
            .enumerate()
            .take(num_used_sessions + 1)
            .flat_map(move |(session_i, (session, session_state))| {
                session
                    .actors
                    .iter()
                    .zip_eq(session_state.actors.iter())
                    .enumerate()
                    .flat_map(move |(actor_i, (actor, state))| {
                        if let Some(transaction) = actor.strand.get(state.current_execution) {
                            match transaction.direction {
                                Direction::Outgoing => {
                                    let mut new = self.clone();

                                    if let Some(receiver_i) = new.states[session_i]
                                        .actors
                                        .iter_mut()
                                        .position(|a| a.actor_id == transaction.receiver)
                                    {
                                        // shouldn't, we also have an execution where receiver doesn't get the message in their inbox?
                                        let mut next_executions = Vec::new();
                                        if let Some(intruder) = &mut new.intruder {
                                            intruder
                                                .knowledge
                                                .0
                                                .extend(transaction.messages.clone());
                                        }

                                        new.states[session_i].actors[actor_i].current_execution +=
                                            1;

                                        // TODO: Don't clone if not nessesary
                                        let mut intruder_intercept = new.clone();

                                        new.trace.push(
                                            TraceEntry {
                                                session: session.session_id,
                                                sender: Some((
                                                    actor.name.as_str().into(),
                                                    actor.actor_id,
                                                )),
                                                receiver:
                                                    Some(
                                                        (
                                                            self.sessions[session_i].actors
                                                                [receiver_i]
                                                                .name
                                                                .as_str()
                                                                .into(),
                                                            self.sessions[session_i].actors
                                                                [receiver_i]
                                                                .actor_id,
                                                        ),
                                                    ),
                                                messages: transaction.messages.clone(),
                                            }
                                            .into(),
                                        );

                                        let r = &mut new.states[session_i].actors[receiver_i];
                                        r.inbox.push_back(transaction.messages.clone());

                                        next_executions.push(new);

                                        if self.intruder.is_some() {
                                            intruder_intercept.trace.push(
                                                TraceEntry {
                                                    session: session.session_id,
                                                    sender: Some((
                                                        actor.name.as_str().into(),
                                                        actor.actor_id,
                                                    )),
                                                    receiver: None,
                                                    messages: transaction.messages.clone(),
                                                }
                                                .into(),
                                            );

                                            next_executions.push(intruder_intercept);
                                        }

                                        next_executions
                                    } else {
                                        unreachable!("cannot send message to unknown actor")
                                    }
                                }
                                Direction::Ingoing => {
                                    if !state.inbox.is_empty() {
                                        let mut new = self.clone();

                                        let incoming = new.states[session_i].actors[actor_i]
                                            .inbox
                                            .pop_back()
                                            .unwrap();

                                        for (&a, &b) in
                                            transaction.messages.iter().zip_eq(incoming.iter())
                                        {
                                            // if new.unifier.resolve_full(a)
                                            //     != new.unifier.resolve_full(b)
                                            // {
                                            //     println!(
                                            //         "Time to unify {:?} with {:?}",
                                            //         new.unifier.resolve_full(a),
                                            //         new.unifier.resolve_full(b)
                                            //     );
                                            // }
                                            match new.unifier.unify(a, b) {
                                                Ok(_) => {}
                                                Err(_) => {
                                                    return vec![];
                                                }
                                            }
                                        }

                                        new.states[session_i].actors[actor_i].current_execution +=
                                            1;

                                        vec![new]
                                    } else {
                                        let mut new = self.clone();

                                        if let Some(intruder) = &mut new.intruder {
                                            intruder.constraints.push(Rc::new((
                                                intruder.knowledge.clone(),
                                                transaction.messages.clone(),
                                            )));

                                            intruder
                                                .knowledge
                                                .0
                                                .extend(transaction.messages.clone());

                                            new.states[session_i].actors[actor_i]
                                                .current_execution += 1;

                                            new.trace.push(
                                                TraceEntry {
                                                    session: SessionId(session_i as _),
                                                    sender: None,
                                                    receiver:
                                                        Some(
                                                            (
                                                                new.sessions[session_i].actors
                                                                    [actor_i]
                                                                    .name
                                                                    .clone()
                                                                    .into(),
                                                                new.states[session_i].actors
                                                                    [actor_i]
                                                                    .actor_id,
                                                            ),
                                                        ),
                                                    messages: transaction.messages.clone(),
                                                }
                                                .into(),
                                            );

                                            return vec![new];
                                        }

                                        vec![]
                                    }
                                }
                            }
                        } else {
                            vec![]
                        }
                    })
            })
            .filter_map(|mut exe| {
                if let Some(intruder) = &mut exe.intruder {
                    intruder
                        .conforms_to_constraints(&mut exe.unifier)
                        .then(|| exe)
                } else {
                    Some(exe)
                }
            })
    }

    pub fn has_compromised_secrets(&mut self) -> bool {
        match &mut self.intruder {
            Some(intruder) => intruder.has_achieved_goal(&self.sessions, &mut self.unifier),
            None => false,
        }
    }

    pub fn print_sessions(&mut self) {
        println!("---------------------------");
        if let Some(intruder) = &self.intruder {
            println!(
                "# Intruder knowledge:\n  {:?}",
                intruder
                    .knowledge
                    .0
                    .iter()
                    .map(|msg| self.unifier.resolve_full(*msg))
                    .format(", ")
            );
        }

        println!();
        println!("Sessions");
        for ses in self.sessions.iter() {
            ses.print(&mut self.unifier);
        }
    }
}

/*


A->B: A,B

A1->
    ->B1
        A2->
        B2->
    A2->
        ->B1
        ->B2
A2->
    ->B2
        A1->
        B1->
    A1->
        ->B1
        ->B2


EX1: A1: Put A,B in inbox for B1
    - Take out of inbox B1
    - Put A,B in inbox for B2



*/

/*
problem with gloabl unification:
A -> s: A, B
s->A: {| KAB |}sk(A,s), {| KAB |}sk(B,s)
A->B: A,{| KAB |}sk(B,s)


if s thinks B~i but talks to i initially
i -> s: A, i
s->i: {| KAB |}sk(A,s), {| KAB |}sk(i,s)
i->B: A,{| KAB |}sk(B,s)


i then gets KAB but s thinks it is secure between (A,i,s) and B thinks it is secure between (i,B,s).
here i plays the role of A and B when needed
*/
