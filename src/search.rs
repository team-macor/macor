use std::{collections::VecDeque, rc::Rc};

use indexmap::{map::Entry, IndexMap};
use itertools::Itertools;

use crate::{
    dolev_yao::Knowledge,
    protocol::{
        ActorKind, Constant, Goal, InstanceName, LazyId, Message, Nonce, PacketPattern, Protocol,
        Recipient, SessionId, Variable,
    },
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SubstitutionTable {
    pub variables: IndexMap<Variable, Message>,
}

impl From<IndexMap<Variable, Message>> for SubstitutionTable {
    fn from(variables: IndexMap<Variable, Message>) -> Self {
        Self { variables }
    }
}

impl IntoIterator for SubstitutionTable {
    type Item = (Variable, Message);

    type IntoIter = indexmap::map::IntoIter<Variable, Message>;

    fn into_iter(self) -> Self::IntoIter {
        self.variables.into_iter()
    }
}

impl SubstitutionTable {
    fn is_consistent(&self, other: &SubstitutionTable) -> bool {
        for (var, msg) in &other.variables {
            if let Some(our_msg) = self.variables.get(var) {
                if our_msg != msg {
                    return false;
                }
            }
        }

        true
    }

    fn extend(&mut self, other: SubstitutionTable) {
        self.variables.extend(other.variables)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Strand {
    current_execution: usize,
    messages: Rc<Vec<PacketPattern>>,
    knowledge: Rc<Knowledge>,
    substitutions: SubstitutionTable,
}

#[derive(Debug, Clone, PartialEq)]
enum Obligation {
    CanDerive {
        with_knowledge: Rc<Knowledge>,
        lazy: LazyId,
    },
}

#[derive(Debug, Clone, PartialEq)]
struct Intruder {
    knowledge: Rc<Knowledge>,
    obligations: Vec<Obligation>,
}

#[derive(Debug, Clone, PartialEq)]
struct Execution {
    intruder: Option<Intruder>,
    trace: Vec<(SessionId, InstanceName, InstanceName, Packet)>,
    sessions: Vec<Session>,
}

#[derive(Debug, Clone, PartialEq)]
struct Session {
    actors: Rc<IndexMap<InstanceName, Actor>>,
    goals: Vec<Goal<InstanceName>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Packet {
    messages: Vec<Message>,
}

fn most_general_unifier(a: &Message, b: &Message) -> Result<SubstitutionTable, ()> {
    if a == b {
        return Ok(Default::default());
    }

    match (a, b) {
        (Message::Variable(var), other) | (other, Message::Variable(var)) => {
            let mut substitutions = IndexMap::new();

            substitutions.insert(var.clone(), other.clone());

            Ok(substitutions.into())
        }
        (Message::Tuple(xs), Message::Tuple(ys)) => {
            if xs.len() != ys.len() {
                return Err(());
            }

            let substitutions = combine_substitutions(xs, ys)?;

            Ok(substitutions.into())
        }
        (
            Message::Composition {
                func: func1,
                args: args1,
            },
            Message::Composition {
                func: func2,
                args: args2,
            },
        ) => {
            if args1.len() != args2.len() {
                return Err(());
            }

            if func1 != func2 {
                return Err(());
            }

            let substitutions = combine_substitutions(args1, args2)?;

            Ok(substitutions.into())
        }
        (a, b) => todo!("unify {:?} with {:?}", a, b),
    }
}

fn combine_substitutions(a: &[Message], b: &[Message]) -> Result<IndexMap<Variable, Message>, ()> {
    let mut substitutions = IndexMap::new();
    for (a, b) in a.iter().zip(b.iter()) {
        let argument_unification = most_general_unifier(a, b)?;

        for (var, unification) in argument_unification {
            match substitutions.entry(var) {
                Entry::Occupied(entry) => {
                    if entry.get() != &unification {
                        return Err(());
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(unification);
                }
            }
        }
    }
    Ok(substitutions)
}

impl Packet {
    fn most_general_unifier(&self, other: &Packet) -> Result<SubstitutionTable, ()> {
        // println!("Most general unifier: {:?} {:?}", self, other);

        if self.messages.len() != other.messages.len() {
            return Err(());
        }

        let mut result = IndexMap::new();

        for unification in self
            .messages
            .iter()
            .zip_eq(other.messages.iter())
            .map(|(a, b)| most_general_unifier(a, b))
        {
            for (variable, unification) in unification? {
                match result.entry(variable) {
                    Entry::Occupied(existing) => {
                        if existing.get() != &unification {
                            return Err(());
                        }
                    }
                    Entry::Vacant(v) => {
                        v.insert(unification);
                    }
                }
            }
        }

        Ok(result.into())
    }

    fn substitute_unification(self, unification: &SubstitutionTable) -> Packet {
        let messages: Vec<Message> = self
            .messages
            .into_iter()
            .map(|msg| msg.unify(unification))
            .collect();

        Packet { messages }
    }
}

impl Message {
    fn unify(&self, unification: &SubstitutionTable) -> Message {
        match self {
            Message::Variable(var) => unification.variables[var].clone(),
            Message::Constant(_) => self.clone(),
            Message::Lazy(_) => todo!(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args.iter().map(|msg| msg.unify(unification)).collect(),
            },
            Message::Tuple(args) => {
                Message::Tuple(args.iter().map(|arg| arg.unify(unification)).collect())
            }
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
struct Actor {
    name: InstanceName,
    strand: Strand,
    inbox: VecDeque<Packet>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SearchResult {
    NoAttackFound,
    AttackFound,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Searcher {
    protocol: Protocol,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SearchOptions {
    pub include_intruder: bool,
    pub num_sessions: u32,
}

impl Searcher {
    pub fn new(protocol: Protocol) -> Searcher {
        Searcher { protocol }
    }

    fn initiate_session(&self, session_id: SessionId) -> Session {
        let actors = self
            .protocol
            .actors
            .iter()
            .map(move |a| {
                let name = a.name.initiate_for_session(session_id);

                (
                    name.clone(),
                    Actor {
                        name,
                        strand: Strand {
                            current_execution: 0,
                            messages: a
                                .messages
                                .iter()
                                .map(|msg| msg.init_all_actors(session_id))
                                .collect_vec()
                                .into(),
                            knowledge: a.initial_knowledge.init_all_actors(session_id).into(),
                            substitutions: Default::default(),
                        },
                        inbox: VecDeque::default(),
                    },
                )
            })
            .collect::<IndexMap<_, _>>()
            .into();

        let goals = self
            .protocol
            .goals
            .iter()
            .map(|goal| match goal {
                Goal::SecretBetween(a, b, msg) => Goal::SecretBetween(
                    a.initiate_for_session(session_id),
                    b.initiate_for_session(session_id),
                    msg.init_all_actors(session_id),
                ),
                Goal::Authenticates(a, b, msg) => Goal::Authenticates(
                    a.initiate_for_session(session_id),
                    b.initiate_for_session(session_id),
                    msg.init_all_actors(session_id),
                ),
            })
            .collect_vec();

        Session { actors, goals }
    }

    pub fn find_attack(&self, s: SearchOptions) -> SearchResult {
        let mut execution_queue = VecDeque::new();
        let sessions = (0..s.num_sessions)
            .map(|n| self.initiate_session(SessionId(n)))
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
            intruder: s.include_intruder.then(|| Intruder {
                knowledge: intruder_knowledge.into(),
                obligations: vec![],
            }),
            trace: vec![],
            sessions,
        };

        execution_queue.push_back(exe);

        let start = std::time::Instant::now();
        let mut executions_searched = 0;

        while let Some(exe) = execution_queue.pop_back() {
            executions_searched += 1;

            println!(
                "solcreme i mit øje 😎👌 {:?}",
                start.elapsed() / executions_searched,
            );

            if exe.has_attack() {
                return SearchResult::AttackFound;
            }

            let next_executions = exe.next(&mut Default::default());

            for execution in next_executions {
                if execution == exe {
                    unreachable!("logic error: next execution produced non-progressive execution");
                }

                execution_queue.push_back(execution);
            }
        }

        SearchResult::NoAttackFound
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
    pub fn has_attack(&self) -> bool {
        // TODO: check if intruder has access to any goals
        match &self.intruder {
            Some(intruder) => self.sessions.iter().any(|sess| {
                sess.goals.iter().any(|goal| match goal {
                    Goal::Authenticates(_, _, _) => todo!("only secrecy goals are checked"),
                    Goal::SecretBetween(_, _, msg) => intruder.knowledge.can_construct(msg),
                })
            }),
            None => false,
        }
    }

    pub fn print_trace(&self) {
        for (ses, from, target, packet) in &self.trace {
            println!("[{ses:?}] {from:?}->{target:?}: {packet:?}");
        }
    }

    /// Adds the message to the inbox of `target` and increments the execution of the sender
    pub fn send_packet(
        &mut self,
        session_id: SessionId,
        sender: &InstanceName,
        receiver: &InstanceName,
        packet: Packet,
    ) {
        self.trace
            .push((session_id, sender.clone(), receiver.clone(), packet.clone()));

        Rc::make_mut(&mut self.sessions[session_id.0 as usize].actors)
            .get_mut(receiver)
            .unwrap()
            .inbox
            .push_back(packet);

        Rc::make_mut(&mut self.sessions[session_id.0 as usize].actors)
            .get_mut(sender)
            .unwrap()
            .strand
            .current_execution += 1;
    }

    pub fn receive_packet(
        &mut self,
        session_id: SessionId,
        sender: Option<&InstanceName>,
        receiver: &InstanceName,
        packet_pattern: &Packet,
        packet: Packet,
    ) {
        let receiver = Rc::make_mut(&mut self.sessions[session_id.0 as usize].actors)
            .get_mut(receiver)
            .unwrap();

        // NOTE: check if packet matches previous Knowledge
        match packet_pattern.most_general_unifier(&packet) {
            Ok(unification) => {
                if !receiver.strand.substitutions.is_consistent(&unification) {
                    println!("\n{:?}\n{:?}", receiver.strand.substitutions, unification);
                    self.print_trace();
                    todo!("actor received some packets which are not consistent with current knowledge");
                }

                for msg in packet.substitute_unification(&unification) {
                    Rc::make_mut(&mut receiver.strand.knowledge).augment_with(msg)
                }
                receiver.strand.current_execution += 1;
                receiver.strand.substitutions.extend(unification);
            }
            Err(_) => todo!("unification error"),
        }
    }

    pub fn next(&self, ctx: &mut ExecutionContext) -> Vec<Execution> {
        let mut next = vec![];

        for (i_ses, ses) in self.sessions.iter().enumerate() {
            for (actor_key, actor) in ses.actors.iter() {
                let mut new_execution = self.clone();
                let new_actor = Rc::make_mut(&mut new_execution.sessions[i_ses].actors)
                    .get_mut(actor_key)
                    .unwrap();

                if new_actor
                    .strand
                    .messages
                    .get(new_actor.strand.current_execution)
                    .is_none()
                {
                    continue;
                }

                match &actor.strand.messages[actor.strand.current_execution] {
                    PacketPattern::Ingoing(packet_pattern) => {
                        if let Some(packet) = new_actor.inbox.pop_front() {
                            // TODO: Don't modify new_execution
                            new_execution.receive_packet(
                                SessionId(i_ses as _),
                                None,
                                &actor.name,
                                packet_pattern,
                                packet,
                            );

                            if &new_execution != self {
                                next.push(new_execution.clone());
                            }
                        }

                        // TODO: Can intruder send?
                        if let Some(intruder) = new_execution.intruder.as_mut() {
                            let intruder_packet = packet_pattern
                                .iter()
                                .map(|_| {
                                    // TODO: Don't use lazy 0
                                    let lid = LazyId(0);
                                    intruder.obligations.push(Obligation::CanDerive {
                                        with_knowledge: intruder.knowledge.clone(),
                                        lazy: lid.clone(),
                                    });
                                    Message::Lazy(lid)
                                })
                                .collect();

                            new_execution.receive_packet(
                                SessionId(i_ses as _),
                                Some(&InstanceName::Intruder),
                                &actor.name,
                                packet_pattern,
                                intruder_packet,
                            );

                            if &new_execution != self {
                                todo!();
                                next.push(new_execution.clone());
                            }
                        }
                    }
                    PacketPattern::Outgoing(target, packet) => {
                        let target = match target {
                            Recipient::Variable(_) => unreachable!(),
                            Recipient::Instance(target) => target,
                        };

                        if let Some(new_packet) = new_actor.strand.initiate(ctx, packet) {
                            // if let Some(i) = new_execution.intruder.as_mut() {
                            //     for msg in new_packet.iter() {
                            //         Rc::make_mut(&mut i.knowledge).augment_with(msg.clone());
                            //     }

                            //     // TODO: This should not result in infinite
                            //     //       loops. The case is when i's knowledge
                            //     //       does not get extended, and thus is the
                            //     //       same as the previous knowledge.
                            //     if self != &new_execution {
                            //         // NOTE: A new execution where the recipient does
                            //         // not get the message.
                            //         next.push(new_execution.clone());
                            //         todo!("take me home");
                            //     }
                            // }

                            // TODO: Why does this target not have a session!
                            if !new_execution.sessions[i_ses].actors.contains_key(target) {
                                todo!(
                                    "\nactors: {:?}\ntarget: {:?}",
                                    new_execution.sessions[i_ses].actors.keys().collect_vec(),
                                    target
                                )
                            }

                            new_execution.send_packet(
                                SessionId(i_ses as _),
                                &actor.name,
                                target,
                                new_packet,
                            );

                            if &new_execution != self {
                                next.push(new_execution);
                            }
                        } else {
                            self.print_trace();
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
                        if v.0.is_constant() {
                            Recipient::Instance(InstanceName::Constant(v.clone()))
                        } else {
                            Recipient::Instance(InstanceName::Actor(v.clone(), session_id))
                        }
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
        let msg = &msg.perform_substitutions(&self.substitutions);

        if self.knowledge.can_construct(msg) {
            return Some(msg.clone());
        }

        let new_msg = match msg {
            Message::Variable(var) => {
                self.substitutions
                    .variables
                    .entry(var.clone())
                    .or_insert_with_key(|var| match var {
                        Variable::Actor(_) => unreachable!("all actors have been initiated"),
                        Variable::SymmetricKey(_) | Variable::Number(_) => {
                            // TODO: Add marker for which type of variable
                            // the nonce came from
                            let new = Message::Constant(Constant::Nonce(ctx.new_nonce()));
                            Rc::make_mut(&mut self.knowledge).augment_with(new.clone());
                            new
                        }
                    })
                    .clone()
            }
            Message::Constant(_) => msg.clone(),
            Message::Lazy(_) => todo!(),
            Message::Composition { func, args } => Message::Composition {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|arg| self.initiate_msg(ctx, arg))
                    .collect::<Option<Vec<_>>>()?,
            },
            Message::Tuple(ts) => Message::Tuple(
                ts.iter()
                    .map(|t| self.initiate_msg(ctx, t))
                    .collect::<Option<Vec<_>>>()?,
            ),
        };

        self.knowledge
            .can_construct(&new_msg)
            .then(|| new_msg.clone())
            .or_else(|| {
                dbg!(&self.knowledge);
                dbg!(new_msg);
                None
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::protocol::ActorName;

    use super::*;

    #[test]
    fn simple_unifications() {
        let unification = most_general_unifier(
            &Message::Variable(Variable::Actor(ActorName("A".into()))),
            &Message::Constant(Constant::Actor(ActorName("a".into()))),
        )
        .unwrap();

        let mut expected = IndexMap::new();
        expected.insert(
            Variable::Actor(ActorName("A".into())),
            Message::Constant(Constant::Actor(ActorName("a".into()))),
        );

        assert_eq!(unification, expected.into());

        let unification = most_general_unifier(
            &Message::Constant(Constant::Actor(ActorName("a".into()))),
            &Message::Variable(Variable::Actor(ActorName("A".into()))),
        )
        .unwrap();

        let mut expected = IndexMap::new();
        expected.insert(
            Variable::Actor(ActorName("A".into())),
            Message::Constant(Constant::Actor(ActorName("a".into()))),
        );

        assert_eq!(unification, expected.into());

        let unification = most_general_unifier(
            &Message::Variable(Variable::Number("NA".into())),
            &Message::Constant(Constant::Nonce(Nonce(0))),
        )
        .unwrap();

        let mut expected = IndexMap::new();
        expected.insert(
            Variable::Number("NA".into()),
            Message::Constant(Constant::Nonce(Nonce(0))),
        );

        assert_eq!(unification, expected.into());

        // assert!(most_general_unifier(
        //     &Message::SymEnc {
        //         message: box Message::Constant(Constant::Actor(ActorName("a".into()))),
        //         key: box Message::Constant(Constant::Nonce(Nonce(0))),
        //     },
        //     &Message::Constant(Constant::Nonce(Nonce(0))),
        // )
        // .is_err());
    }

    #[test]
    fn can_verify_all_protocols() -> Result<(), Box<dyn std::error::Error>> {
        for p in std::fs::read_dir("./example_programs")? {
            let src = std::fs::read_to_string(p?.path())?;
            let parsed = crate::parse_document(&src)?;
            let protocol =
                Protocol::new(src.clone(), parsed).map_err(|x| x.first().cloned().unwrap())?;
            let searcher = Searcher::new(protocol);

            let attack = searcher.find_attack(SearchOptions {
                num_sessions: 1,
                include_intruder: false,
            });

            assert_eq!(attack, SearchResult::NoAttackFound);
        }

        Ok(())
    }
}
