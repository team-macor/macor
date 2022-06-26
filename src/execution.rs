use std::collections::VecDeque;

use itertools::Itertools;
use smol_str::SmolStr;

use crate::{
    dolev_yao,
    messages::{Converter, Knowledge, Mappings, Session, TermId, Unifier},
    protocol::{Direction, Protocol, SessionId},
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
pub struct TraceEntry {
    session: SessionId,
    pub sender: Option<(SmolStr, TermId)>,
    pub receiver: Option<(SmolStr, TermId)>,
    pub terms: Vec<TermId>,
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
                dolev_yao::augment_knowledge(&mut knowledge, &mut new_unifier);

                // println!(
                //     "Trying to construct {:?} with knowledge [{:?}]\n",
                //     new_unifier.resolve_full(secret.term),
                //     knowledge
                //         .0
                //         .iter()
                //         .map(|term| new_unifier.resolve_full(*term))
                //         .format(", ")
                // );

                for &k in &knowledge.0 {
                    if new_unifier.unify(secret.term, k).is_ok()
                    // if self.knowledge.can_construct(unifier, secret)
                        && self.conforms_to_constraints_without_augment(&mut new_unifier) && !secret
                        .between_agents
                        .iter()
                        .any(|agent| new_unifier.are_unified(intruder_id, *agent))
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
            .all(|(k, terms)| terms.iter().all(|&term| k.can_construct(unifier, term)))
    }

    fn conforms_to_constraints(&mut self, unifier: &mut Unifier) -> bool {
        dolev_yao::augment_knowledge(&mut self.knowledge, unifier);
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
                agents: session
                    .agents
                    .iter()
                    .map(|agent| ExecutionAgentState {
                        agent_id: agent.agent_id,
                        current_execution: 0,
                        inbox: Default::default(),
                    })
                    .collect(),
            })
            .collect();

        let mut converter = Converter::new(&mut unifier, mappings);
        let mut intruder = Intruder::default();

        for session in sessions.iter() {
            for agent in &session.agents {
                intruder.knowledge.0.push(agent.agent_id);
            }
            for agent in &protocol.agents {
                if agent.name.0.is_constant() {
                    continue;
                }

                for term in &agent.initial_knowledge.0 {
                    let term = term.replace_agent_with_intruder(&agent.name);
                    let registered_term = converter.register_typed_term(
                        &crate::messages::ForWho::Intruder(session.session_id),
                        &protocol.initiations,
                        term.clone(),
                    );

                    if intruder
                        .knowledge
                        .0
                        .iter()
                        .all(|&term| !converter.unifier.are_unified(term, registered_term))
                    {
                        intruder.knowledge.0.push(registered_term);
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
                    .agents
                    .iter()
                    .zip_eq(session_state.agents.iter())
                    .enumerate()
                    .flat_map(move |(agent_i, (agent, state))| {
                        if let Some(transaction) = agent.strand.get(state.current_execution) {
                            match transaction.direction {
                                Direction::Outgoing => {
                                    let mut new = self.clone();

                                    if let Some(intruder) = &mut new.intruder {
                                        new.states[session_i].agents[agent_i].current_execution +=
                                            1;

                                        intruder.knowledge.0.extend(transaction.terms.clone());

                                        new.trace.push(
                                            TraceEntry {
                                                session: session.session_id,
                                                sender: Some((
                                                    agent.name.as_str().into(),
                                                    agent.agent_id,
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
                                                    agent.name.as_str().into(),
                                                    agent.agent_id,
                                                )),
                                                receiver: Some(
                                                    (
                                                        self.sessions[session_i].agents[receiver_i]
                                                            .name
                                                            .as_str()
                                                            .into(),
                                                        self.sessions[session_i].agents[receiver_i]
                                                            .agent_id,
                                                    ),
                                                ),
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

                                            intruder.knowledge.0.extend(transaction.terms.clone());

                                            new.states[session_i].agents[agent_i]
                                                .current_execution += 1;

                                            new.trace.push(
                                                TraceEntry {
                                                    session: SessionId(session_i as _),
                                                    sender: None,
                                                    receiver: Some((
                                                        new.sessions[session_i].agents[agent_i]
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
                    .0
                    .iter()
                    .map(|term| self.unifier.resolve_full(*term))
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
