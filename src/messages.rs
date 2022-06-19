use std::collections::VecDeque;

use crate::protocol::{self, Direction, Func, Protocol, SessionId};

use ena::unify::{InPlaceUnificationTable, UnifyKey, UnifyValue};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use macor_parse::ast::Ident;
use smol_str::SmolStr;

type Rc<T> = std::sync::Arc<T>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantId(u32, Option<&'static str>);

impl std::fmt::Debug for ConstantId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.1 {
            write!(f, "!{}[{}]", name, self.0)
        } else {
            write!(f, "![{}]", self.0)
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MessageId(u32);

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Message<M> {
    Variable(Option<String>),
    Lazy,
    Constant(ConstantId),
    Composition(Func, Vec<M>),
    Tuple(Vec<M>),
}

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
            Self::Lazy => write!(f, "Lazy"),
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct FullMessage(Message<FullMessage>);

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
            (Lazy, _) | (_, Lazy) => todo!(),
            (Variable(l), Variable(r)) => Variable(l.clone().or_else(|| r.clone())),
            (Variable(_), Constant(c)) | (Constant(c), Variable(_)) => Constant(*c),
            (Variable(_), Composition(func, args)) | (Composition(func, args), Variable(_)) => {
                Composition(func.clone(), args.clone())
            }
            (Variable(_), Tuple(ts)) | (Tuple(ts), Variable(_)) => Tuple(ts.clone()),
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
            (Constant(_), Composition(_, _))
            | (Constant(_), Tuple(_))
            | (Composition(_, _), Constant(_))
            | (Composition(_, _), Tuple(_))
            | (Tuple(_), Constant(_))
            | (Tuple(_), Composition(_, _)) => return Err(()),
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

#[derive(Debug, Default, Clone)]
pub struct Unifier {
    next_constant: u32,
    actor_table: Rc<IndexMap<SmallStr, ConstantId>>,
    func_table: Rc<IndexMap<Func, MessageId>>,
    global_constant_table: Rc<IndexMap<SmallStr, MessageId>>,
    constant_table: Rc<IndexMap<VariableKey, MessageId>>,
    variable_table: Rc<IndexMap<VariableKey, MessageId>>,
    table: InPlaceUnificationTable<MessageId>,
}

impl Unifier {
    /// Recursively unifies the two messages and returns either of the passed
    /// messages if they indeed do unify (since they are now equivalent), or
    /// else produces and error.
    fn unify(&mut self, l: MessageId, r: MessageId) -> Result<MessageId, ()> {
        use self::Message::*;

        Ok(
            match (self.table.probe_value(l), self.table.probe_value(r)) {
                (Lazy, _) | (_, Lazy) => todo!(),
                (Variable(_), _) | (_, Variable(_)) => {
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
                        todo!()
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
                (Constant(_), Composition(_, _))
                | (Constant(_), Tuple(_))
                | (Composition(_, _), Constant(_))
                | (Composition(_, _), Tuple(_))
                | (Tuple(_), Constant(_))
                | (Tuple(_), Composition(_, _)) => return Err(()),
            },
        )
    }
    fn resolve_full(&mut self, id: MessageId) -> FullMessage {
        match self.table.probe_value(id) {
            Message::Variable(v) => FullMessage(Message::Variable(v)),
            Message::Lazy => FullMessage(Message::Lazy),
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

    pub fn get_actor(&mut self, agent: &str) -> ConstantId {
        // TODO: ???
        let mid = self.register_global_constant(agent);
        if let Message::Constant(c) = self.table.probe_value(mid) {
            c
        } else {
            unreachable!()
        }
        // TODO: ???
        // *self.actor_table.entry(agent.into()).or_insert_with(|| {
        //     let cid = ConstantId(self.next_constant, Some(leak_str(agent)));
        //     self.next_constant += 1;
        //     cid
        // })
    }
    pub fn get_function_constant(&mut self, func: Func) -> MessageId {
        *Rc::make_mut(&mut self.func_table)
            .entry(func.clone())
            .or_insert_with(|| {
                let cid = ConstantId(self.next_constant, Some(leak_str(&format!("{:?}", func))));
                self.next_constant += 1;
                self.table.new_key(Message::Constant(cid))
            })
    }

    pub fn register_global_constant(&mut self, constant_name: &str) -> MessageId {
        *Rc::make_mut(&mut self.global_constant_table)
            .entry(constant_name.into())
            .or_insert_with(|| {
                let cid = ConstantId(self.next_constant, Some(leak_str(constant_name)));
                self.next_constant += 1;
                self.table.new_key(Message::Constant(cid))
            })
    }
    pub fn register_constant(
        &mut self,
        agent: &Ident<SmallStr>,
        session_id: SessionId,
        constant_name: &Ident<SmallStr>,
    ) -> MessageId {
        *Rc::make_mut(&mut self.constant_table)
            .entry(VariableKey {
                session_id,
                actor: agent.into(),
                variable: constant_name.into(),
            })
            .or_insert_with(|| {
                let cid = ConstantId(
                    self.next_constant,
                    Some(leak_str(&format!(
                        "{}@{}:{}",
                        agent, session_id.0, constant_name
                    ))),
                );
                self.next_constant += 1;
                self.table.new_key(Message::Constant(cid))
            })
    }
    pub fn register_variable(
        &mut self,
        agent: &Ident<SmallStr>,
        session_id: SessionId,
        variable_name: &Ident<SmallStr>,
    ) -> MessageId {
        *Rc::make_mut(&mut self.variable_table)
            .entry(VariableKey {
                session_id,
                actor: agent.into(),
                variable: variable_name.into(),
            })
            .or_insert_with(|| {
                self.table.new_key(Message::Variable(Some(format!(
                    "{}@{}:{}",
                    agent, session_id.0, variable_name
                ))))
            })
    }

    fn initiate_typed_variable(
        &mut self,
        agent: &Ident<SmallStr>,
        session_id: SessionId,
        initiate: &IndexSet<protocol::Variable>,
        var: &protocol::Variable,
    ) -> MessageId {
        match var {
            protocol::Variable::Actor(a) => {
                // TODO: Is it fine to register agents like this??
                // self.register_variable(agent, session_id, a.0.as_str())
                // TODO: For now, just fix them so that A will always be A, and B always B
                self.register_global_constant(a.0.as_str())
            }
            protocol::Variable::SymmetricKey(n) | protocol::Variable::Number(n) => {
                if initiate.contains(var) {
                    self.register_constant(agent, session_id, &n.convert())
                } else {
                    self.register_variable(agent, session_id, &n.convert())
                }
            }
        }
    }
    fn register_typed_message(
        &mut self,
        agent: &Ident<SmallStr>,
        session_id: SessionId,
        initiate: &IndexSet<protocol::Variable>,
        msg: protocol::Message,
    ) -> MessageId {
        match msg {
            protocol::Message::Variable(var) => {
                self.initiate_typed_variable(agent, session_id, initiate, &var)
            }
            protocol::Message::Constant(c) => match c {
                protocol::Constant::Actor(a) => self.register_global_constant(a.0.as_str()),
                protocol::Constant::Function(f) => self.get_function_constant(f),
                protocol::Constant::Nonce(_) => todo!(),
            },
            protocol::Message::Composition { func, args } => {
                let msg = Message::Composition(
                    func,
                    args.into_iter()
                        .map(|arg| self.register_typed_message(agent, session_id, initiate, arg))
                        .collect(),
                );
                self.table.new_key(msg)
            }
            protocol::Message::Tuple(ts) => {
                let msg = Message::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_typed_message(agent, session_id, initiate, t))
                        .collect(),
                );
                self.table.new_key(msg)
            }
        }
    }
}

fn leak_str(s: &str) -> &'static str {
    Box::leak(s.to_string().into_boxed_str())
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
        unifier.table.probe_value(a),
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

// TODO
struct Converter {}

#[derive(Debug, Clone)]
struct Knowledge(Vec<MessageId>);

type SmallStr = SmolStr;

#[derive(Debug, Clone)]
struct Transaction {
    sender: ConstantId,
    receiver: ConstantId,
    direction: Direction,
    messages: Vec<MessageId>,
}

#[derive(Debug, Clone)]
struct SessionActor {
    name: Ident<SmallStr>,
    constant: ConstantId,
    initial_knowledge: Knowledge,
    strand: Vec<Transaction>,
}

#[derive(Debug, Clone)]
pub struct Session {
    session_id: SessionId,
    actors: Vec<SessionActor>,
}

impl Session {
    pub fn new(protocol: &Protocol, session_id: SessionId, unifier: &mut Unifier) -> Session {
        let actors = protocol
            .actors
            .iter()
            .map(|actor| {
                let initiates = actor
                    .messages
                    .iter()
                    .flat_map(|pattern| {
                        if pattern.direction == Direction::Outgoing {
                            pattern.initiates.clone()
                        } else {
                            Default::default()
                        }
                    })
                    .collect();

                let mut initial_knowledge: Vec<_> = actor
                    .initial_knowledge
                    .iter()
                    .map(|msg| {
                        unifier.register_typed_message(
                            &actor.name.0,
                            session_id,
                            &initiates,
                            msg.clone(),
                        )
                    })
                    .collect();

                initial_knowledge.extend(initiates.iter().map(|var| {
                    unifier.initiate_typed_variable(&actor.name.0, session_id, &initiates, var)
                }));

                initial_knowledge.sort_unstable_by_key(|msg| unifier.resolve_full(*msg));
                initial_knowledge.dedup();

                SessionActor {
                    name: actor.name.0.clone(),
                    constant: unifier.get_actor(actor.name.0.as_str()),
                    initial_knowledge: Knowledge(initial_knowledge),
                    strand: actor
                        .messages
                        .iter()
                        .map(|pattern| Transaction {
                            sender: unifier.get_actor(pattern.from.0.as_str()),
                            receiver: unifier.get_actor(pattern.to.0.as_str()),
                            direction: pattern.direction,
                            messages: pattern
                                .packet
                                .iter()
                                .map(|msg| {
                                    unifier.register_typed_message(
                                        &actor.name.0,
                                        session_id,
                                        &initiates,
                                        msg.clone(),
                                    )
                                })
                                .collect_vec(),
                        })
                        .collect(),
                }
            })
            .collect_vec();

        Session { session_id, actors }
    }
    pub fn print(&self, unifier: &mut Unifier) {
        println!();
        println!("#############");
        println!("# SESSION {} #", self.session_id.0);
        println!("#############");
        for actor in &self.actors {
            println!();
            println!("> {} ({:?})", actor.name, actor.constant);
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
                    t.sender,
                    t.receiver,
                    t.messages
                        .iter()
                        .map(|&msg| unifier.resolve_full(msg))
                        .collect_vec()
                );
            }
        }
    }
}

#[derive(Debug, Clone)]
struct ExecutionActorState {
    id: ConstantId,
    current_execution: usize,
    inbox: VecDeque<Vec<MessageId>>,
}

#[derive(Debug, Clone)]
struct ExecutionSessionState {
    actors: Vec<ExecutionActorState>,
}

#[derive(Debug, Clone)]
struct TraceEntry {
    session: SessionId,
    sender: SmolStr,
    receiver: SmolStr,
    messages: Vec<MessageId>,
}

#[derive(Debug, Clone)]
pub struct Execution {
    unifier: Unifier,
    intruder: Option<Vec<MessageId>>,
    sessions: Rc<Vec<Session>>,
    states: Vec<ExecutionSessionState>,
    trace: Vec<Rc<TraceEntry>>,
}

impl Execution {
    pub fn new(unifier: Unifier, sessions: Rc<Vec<Session>>) -> Self {
        let states = sessions
            .iter()
            .map(|session| ExecutionSessionState {
                actors: session
                    .actors
                    .iter()
                    .map(|actor| ExecutionActorState {
                        id: actor.constant,
                        current_execution: 0,
                        inbox: Default::default(),
                    })
                    .collect(),
            })
            .collect();

        Execution {
            unifier,
            intruder: None,
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
                    .filter_map(move |(actor_i, (actor, state))| {
                        let transaction = actor.strand.get(state.current_execution)?;

                        match transaction.direction {
                            Direction::Outgoing => {
                                if transaction.direction == Direction::Ingoing {
                                    return None;
                                }

                                let mut new = self.clone();

                                if let Some(receiver_i) = new.states[session_i]
                                    .actors
                                    .iter_mut()
                                    .position(|a| a.id == transaction.receiver)
                                {
                                    let r = &mut new.states[session_i].actors[receiver_i];
                                    r.inbox.push_back(transaction.messages.clone());

                                    new.states[session_i].actors[actor_i].current_execution += 1;

                                    new.trace.push(
                                        TraceEntry {
                                            session: session.session_id,
                                            sender: actor.name.as_str().into(),
                                            receiver: self.sessions[session_i].actors[receiver_i]
                                                .name
                                                .0
                                                .as_str()
                                                .into(),
                                            // messages: transaction
                                            //     .messages
                                            //     .iter()
                                            //     .map(|&msg| new.unifier.resolve_full(msg))
                                            //     .collect(),
                                            messages: transaction.messages.clone(),
                                        }
                                        .into(),
                                    );

                                    Some(new)
                                } else {
                                    todo!()
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
                                        new.unifier.unify(a, b).unwrap();
                                    }

                                    new.states[session_i].actors[actor_i].current_execution += 1;

                                    return Some(new);
                                }

                                // TODO: Intruder
                                None
                            }
                        }
                    })
            })
    }
    pub fn print_trace(&mut self) {
        println!("---------------------------");
        for t in &self.trace {
            println!(
                "[{}] {}->{}: {:?}",
                t.session.0,
                t.sender,
                t.receiver,
                t.messages
                    .iter()
                    .map(|&msg| self.unifier.resolve_full(msg))
                    .format(", ")
            );
        }
    }
    pub fn print_sessions(&mut self) {
        println!("---------------------------");
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
