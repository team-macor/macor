use std::collections::VecDeque;

use itertools::Itertools;
use smol_str::SmolStr;

use crate::{
    dolev_yao::{Knowledge, WithUnification},
    protocol::{Direction, SessionId},
    sessions::Session,
    terms::{TermId, Unifier},
};

type Rc<T> = std::sync::Arc<T>;

#[derive(Debug, Clone)]
struct ExecutionAgentState {
    agent_id: TermId,
    current_execution: usize,
    inbox: VecDeque<Vec<TermId>>,
}

#[derive(Debug, Clone)]
struct ExecutionSessionState {
    agents: Vec<ExecutionAgentState>,
}

#[derive(Debug, Clone)]
pub struct TraceEntry<P, M> {
    session: SessionId,
    pub sender: P,
    pub receiver: P,
    pub terms: Vec<M>,
}

impl<P, M> TraceEntry<P, M> {
    pub fn map_participant<T>(&self, mut f: impl FnMut(&P) -> T) -> TraceEntry<T, M>
    where
        M: Clone,
    {
        TraceEntry {
            session: self.session,
            sender: f(&self.sender),
            receiver: f(&self.receiver),
            terms: self.terms.clone(),
        }
    }
    pub fn map_term<T>(&self, f: impl FnMut(&M) -> T) -> TraceEntry<P, T>
    where
        P: Clone,
    {
        TraceEntry {
            session: self.session,
            sender: self.sender.clone(),
            receiver: self.receiver.clone(),
            terms: self.terms.iter().map(f).collect(),
        }
    }
}

impl<P: std::fmt::Display, M: std::fmt::Display> std::fmt::Display for TraceEntry<P, M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}: {}",
            self.sender,
            self.receiver,
            self.terms.iter().format(", ")
        )
    }
}

#[derive(Debug, Clone, Default)]
struct Intruder {
    knowledge: Knowledge,
    constraints: Vec<Rc<(Knowledge, Vec<TermId>)>>,
}

impl Intruder {
    fn has_achieved_goal(&mut self, sessions: &[Session], unifier: &mut Unifier) -> bool {
        sessions.iter().any(|sess| {
            sess.secrets.iter().any(|secret| {
                let mut knowledge = self.knowledge.clone();
                let mut new_unifier = unifier.clone();

                let intruder_id = unifier.intruder();
                knowledge.augment_knowledge(&mut new_unifier);

                // println!(
                //     "Trying to construct {:?} with knowledge [{:?}]\n",
                //     new_unifier.resolve_full(secret.term),
                //     knowledge
                //         .0
                //         .iter()
                //         .map(|term| new_unifier.resolve_full(*term))
                //         .format(", ")
                // );

                for k in knowledge {
                    if new_unifier.unify(secret.term, k).is_ok()
                    // if self.knowledge.can_construct(unifier, secret)
                        && self.conforms_to_constraints_without_augment(&mut new_unifier) && !secret
                        .between_agents
                        .iter()
                        .any(|agent| new_unifier.are_equal(intruder_id, *agent))
                    {
                        eprintln!(
                            "LEAKED {:?} ({:?})",
                            new_unifier.resolve_full(secret.term),
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
            .all(|(k, terms)| {
                terms
                    .iter()
                    .all(|&term| k.can_derive(unifier, term, WithUnification::Yes))
            })
    }

    fn conforms_to_constraints(&mut self, unifier: &mut Unifier) -> bool {
        self.knowledge.augment_knowledge(unifier);
        self.conforms_to_constraints_without_augment(unifier)
    }
}

pub type ExecutionTraceEntry = TraceEntry<Option<(SmolStr, TermId)>, TermId>;

#[derive(Debug, Clone)]
pub struct Execution {
    pub unifier: Unifier,
    intruder: Option<Intruder>,
    states: Vec<ExecutionSessionState>,
    sessions: Rc<Vec<Session>>,
    pub trace: Vec<Rc<ExecutionTraceEntry>>,
}

impl Execution {
    pub fn new(unifier: Unifier, sessions: Rc<Vec<Session>>) -> Self {
        let states = sessions
            .iter()
            .map(|session| ExecutionSessionState {
                agents: session
                    .roles
                    .iter()
                    .map(|role| ExecutionAgentState {
                        agent_id: role.agent_id,
                        current_execution: 0,
                        inbox: Default::default(),
                    })
                    .collect(),
            })
            .collect();

        let mut intruder = Intruder::default();

        intruder.knowledge.extend(
            sessions
                .iter()
                .flat_map(|session| session.intruder_knowledge.iter()),
        );

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
                    .roles
                    .iter()
                    .zip_eq(session_state.agents.iter())
                    .enumerate()
                    .flat_map(move |(agent_i, (role, state))| {
                        if let Some(transaction) = role.strand.get(state.current_execution) {
                            match transaction.direction {
                                Direction::Outgoing => {
                                    let mut new = self.clone();

                                    if let Some(intruder) = &mut new.intruder {
                                        new.states[session_i].agents[agent_i].current_execution +=
                                            1;

                                        intruder
                                            .knowledge
                                            .extend(transaction.terms.iter().copied());

                                        new.trace.push(
                                            TraceEntry {
                                                session: session.session_id,
                                                sender: Some((
                                                    role.name.as_str().into(),
                                                    role.agent_id,
                                                )),
                                                receiver: None,
                                                terms: transaction.terms.clone(),
                                            }
                                            .into(),
                                        );
                                    } else if let Some(receiver_i) = new.states[session_i]
                                        .agents
                                        .iter_mut()
                                        .position(|a| a.agent_id == transaction.receiver)
                                    {
                                        new.states[session_i].agents[agent_i].current_execution +=
                                            1;

                                        new.trace.push(
                                            TraceEntry {
                                                session: session.session_id,
                                                sender: Some((
                                                    role.name.as_str().into(),
                                                    role.agent_id,
                                                )),
                                                receiver: Some((
                                                    self.sessions[session_i].roles[receiver_i]
                                                        .name
                                                        .as_str()
                                                        .into(),
                                                    self.sessions[session_i].roles[receiver_i]
                                                        .agent_id,
                                                )),
                                                terms: transaction.terms.clone(),
                                            }
                                            .into(),
                                        );

                                        let r = &mut new.states[session_i].agents[receiver_i];
                                        r.inbox.push_back(transaction.terms.clone());
                                    } else {
                                        unreachable!("cannot send term to unknown agent")
                                    }

                                    vec![new]
                                }
                                Direction::Ingoing => {
                                    if !state.inbox.is_empty() {
                                        assert!(self.intruder.is_none());

                                        let mut new = self.clone();

                                        let incoming = new.states[session_i].agents[agent_i]
                                            .inbox
                                            .pop_back()
                                            .unwrap();

                                        for (&a, &b) in
                                            transaction.terms.iter().zip_eq(incoming.iter())
                                        {
                                            if new.unifier.unify(a, b).is_err() {
                                                return vec![];
                                            }
                                        }

                                        new.states[session_i].agents[agent_i].current_execution +=
                                            1;

                                        vec![new]
                                    } else {
                                        let mut new = self.clone();

                                        if let Some(intruder) = &mut new.intruder {
                                            intruder.constraints.push(Rc::new((
                                                intruder.knowledge.clone(),
                                                transaction.terms.clone(),
                                            )));

                                            new.states[session_i].agents[agent_i]
                                                .current_execution += 1;

                                            new.trace.push(
                                                TraceEntry {
                                                    session: SessionId(session_i as _),
                                                    sender: None,
                                                    receiver: Some((
                                                        new.sessions[session_i].roles[agent_i]
                                                            .name
                                                            .clone()
                                                            .into(),
                                                        new.states[session_i].agents[agent_i]
                                                            .agent_id,
                                                    )),
                                                    terms: transaction.terms.clone(),
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
                    .iter()
                    .map(|term| self.unifier.resolve_full(term))
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
