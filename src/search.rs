use std::{collections::VecDeque, sync::Once};

use indexmap::{map::Entry, IndexMap};
use itertools::Itertools;

use crate::{
    dolev_yao::Knowledge,
    protocol::{
        ActorKind, Constant, InstanceName, Message, Nonce, PacketPattern, Protocol, Recipient,
        SessionId, Variable,
    },
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SubstitutionTable {
    pub table: IndexMap<Variable, Message>,
}

impl From<IndexMap<Variable, Message>> for SubstitutionTable {
    fn from(table: IndexMap<Variable, Message>) -> Self {
        Self { table }
    }
}

impl IntoIterator for SubstitutionTable {
    type Item = (Variable, Message);

    type IntoIter = indexmap::map::IntoIter<Variable, Message>;

    fn into_iter(self) -> Self::IntoIter {
        self.table.into_iter()
    }
}

impl SubstitutionTable {
    fn is_consistent(&self, other: &SubstitutionTable) -> bool {
        for (var, msg) in &other.table {
            if let Some(our_msg) = self.table.get(var) {
                if our_msg != msg {
                    return false;
                }
            }
        }

        true
    }

    fn extend(&mut self, other: SubstitutionTable) {
        self.table.extend(other.table)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Strand {
    current_execution: usize,
    messages: Vec<PacketPattern>,
    knowledge: Knowledge,
    substitutions: SubstitutionTable,
}

#[derive(Debug, Clone, PartialEq)]
struct Intruder {
    knowledge: Knowledge,
}

#[derive(Debug, Clone, PartialEq)]
struct Execution {
    intruder: Option<Intruder>,
    trace: Vec<(SessionId, InstanceName, InstanceName, Packet)>,
    sessions: Vec<Session>,
}

#[derive(Debug, Clone, PartialEq)]
struct Session {
    actors: IndexMap<InstanceName, Actor>,
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

            let substitutions = ayo(xs, ys)?;

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

            if func1.0 != func2.0 {
                return Err(());
            }

            let substitutions = ayo(args1, args2)?;

            Ok(substitutions.into())
        }
        (Message::Composition { func, args }, Message::SymEnc { message, key })
        | (Message::SymEnc { message, key }, Message::Composition { func, args })
        | (Message::Composition { func, args }, Message::AsymEnc { message, key })
        | (Message::AsymEnc { message, key }, Message::Composition { func, args }) => {
            Err(()) // is this correct?
        }
        (
            Message::SymEnc {
                message: message1,
                key: key1,
            },
            Message::SymEnc {
                message: message2,
                key: key2,
            },
        )
        | (
            Message::AsymEnc {
                message: message1,
                key: key1,
            },
            Message::AsymEnc {
                message: message2,
                key: key2,
            },
        ) => {
            // can we unify the encrypted messages even though we do not know the keys?

            let mut substitutions = most_general_unifier(message1, message2)?;
            substitutions.extend(most_general_unifier(key1, key2)?);

            Ok(substitutions)
        }
        (a, b) => todo!("unify {:?} with {:?}", a, b),
    }
}

fn ayo(args1: &[Message], args2: &[Message]) -> Result<IndexMap<Variable, Message>, ()> {
    let mut substitutions = IndexMap::new();
    for (a, b) in args1.iter().zip(args2.iter()) {
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
        println!("Most general unifier: {:?} {:?}", self, other);

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
            .map(|msg| unify(&msg, unification))
            .collect();

        Packet { messages }
    }
}

fn unify(msg: &Message, unification: &SubstitutionTable) -> Message {
    match msg {
        Message::Variable(var) => unification.table[var].clone(),
        Message::Constant(_) => msg.clone(),
        Message::Composition { func, args } => Message::Composition {
            func: func.clone(),
            args: args.iter().map(|msg| unify(msg, unification)).collect(),
        },
        Message::SymEnc { message, key } => Message::SymEnc {
            message: box unify(message, unification),
            key: box unify(key, unification),
        },
        Message::AsymEnc { message, key } => Message::AsymEnc {
            message: box unify(message, unification),
            key: box unify(key, unification),
        },
        Message::Tuple(args) => {
            Message::Tuple(args.iter().map(|arg| unify(arg, unification)).collect())
        }
        Message::Inverse(key) => Message::Inverse(box unify(key, unification)),
        Message::Exp(arg) => Message::Exp(box unify(arg, unification)),
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
    pub num_sessions: u32,
}

impl Searcher {
    pub fn new(protocol: Protocol) -> Searcher {
        Searcher { protocol }
    }

    pub fn find_attack(&self, s: SearchOptions) -> SearchResult {
        let mut execution_queue = VecDeque::new();
        let sessions = (0..s.num_sessions)
            .map(|n| Session {
                actors: self
                    .protocol
                    .actors
                    .iter()
                    .map(move |a| {
                        let session = SessionId(n);
                        let name = if a.name.0.is_constant() {
                            InstanceName::Constant(a.name.clone())
                        } else {
                            InstanceName::Actor(a.name.clone(), session)
                        };
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
            // intruder: Some(Intruder {
            //     knowledge: intruder_knowledge,
            // }),
            intruder: None,
            trace: vec![],
            sessions,
        };

        execution_queue.push_back(exe);

        while let Some(exe) = execution_queue.pop_back() {
            println!("solcreme i mit øje 😎👌");

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
        false
    }

    pub fn print_trace(&self) {
        for (ses, from, target, packet) in &self.trace {
            println!("[{ses:?}] {from:?}->{target:?}: {packet:?}");
        }
    }

    pub fn send_packet(
        &mut self,
        session_id: SessionId,
        from: &InstanceName,
        target: &InstanceName,
        packet: Packet,
    ) {
        self.trace
            .push((session_id, from.clone(), target.clone(), packet.clone()));

        self.sessions[session_id.0 as usize]
            .actors
            .get_mut(target)
            .unwrap()
            .inbox
            .push_back(packet);
    }

    pub fn next(&self, ctx: &mut ExecutionContext) -> Vec<Execution> {
        let mut next = vec![];

        for (i_ses, ses) in self.sessions.iter().enumerate() {
            for (actor_key, actor) in ses.actors.iter() {
                let mut new_execution = self.clone();
                let new_actor = new_execution.sessions[i_ses]
                    .actors
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
                        // TODO: Can intruder send?

                        if let Some(packet) = new_actor.inbox.pop_front() {
                            // check if packet matches previous Knowledge

                            match packet_pattern.most_general_unifier(&packet) {
                                Ok(unification) => {
                                    if !new_actor.strand.substitutions.is_consistent(&unification) {
                                        println!(
                                            "\n{:?}\n{:?}",
                                            new_actor.strand.substitutions, unification
                                        );
                                        self.print_trace();
                                        todo!("actor received some packets which are not consistent with current knowledge");
                                        // eprintln!("actor received some packets which are not consistent with current knowledge");
                                        continue;
                                    }

                                    for msg in packet.substitute_unification(&unification) {
                                        new_actor.strand.knowledge.augment_with(msg)
                                    }
                                    new_actor.strand.current_execution += 1;
                                    new_actor.strand.substitutions.extend(unification);

                                    if &new_execution != self {
                                        next.push(new_execution);
                                    }
                                }
                                Err(_) => todo!("unification error"),
                            }
                        }
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

                                // TODO: This should not result in infinite
                                //       loops. The case is when i's knowledge
                                //       does not get extended, and thus is the
                                //       same as the previous knowledge.
                                if self != &new_execution {
                                    // NOTE: A new execution where the recipient does
                                    // not get the message.
                                    next.push(new_execution.clone());
                                    todo!("take me home");
                                }
                            }

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

                            new_execution.sessions[i_ses]
                                .actors
                                .get_mut(&actor.name)
                                .unwrap()
                                .strand
                                .current_execution += 1;

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
                    .table
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
}
