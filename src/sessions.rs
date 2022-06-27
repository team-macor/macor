use itertools::Itertools;
use macor_parse::ast::Ident;
use smol_str::SmolStr;

use crate::{
    lower::{Converter, ForWho},
    messages::{Knowledge, TermId, Unifier},
    protocol::{self, Direction, Protocol, ProtocolAgent, SessionId},
};

#[derive(Debug, Clone)]
pub struct Transaction {
    pub ast_node: protocol::PacketPattern,
    pub sender: TermId,
    pub receiver: TermId,
    pub direction: Direction,
    pub terms: Vec<TermId>,
}

#[derive(Debug, Clone)]
pub struct SessionAgent {
    pub name: Ident<SmolStr>,
    pub agent_id: TermId,
    pub initial_knowledge: Knowledge,
    pub strand: Vec<Transaction>,
}

#[derive(Debug, Clone)]
pub struct Secret {
    pub between_agents: Vec<TermId>,
    pub term: TermId,
}

#[derive(Debug, Clone)]
pub struct Session {
    pub session_id: SessionId,
    pub agents: Vec<SessionAgent>,
    pub intruder_knowledge: Knowledge,
    pub secrets: Vec<Secret>,
}

impl Session {
    pub fn new(protocol: &Protocol, session_id: SessionId, converter: &mut Converter) -> Session {
        let agents = protocol
            .agents
            .iter()
            .map(|agent| SessionAgent::new(protocol, session_id, converter, agent))
            .collect_vec();

        let secrets = protocol
            .goals
            .iter()
            .flat_map(|goal| match goal {
                protocol::Goal::SecretBetween(agents, terms) => terms
                    .iter()
                    .map(|term| Secret {
                        between_agents: agents
                            .iter()
                            .map(|agent| converter.get_agent(&ForWho::Intruder(session_id), agent))
                            .collect(),
                        term: converter.register_typed_term(
                            &ForWho::Intruder(session_id),
                            &protocol.initiations,
                            term.clone(),
                        ),
                    })
                    .collect_vec(),
                protocol::Goal::Authenticates(_, _, _) => {
                    eprintln!("⚠️ no authentication goals");
                    vec![]
                }
            })
            .collect();

        let mut intruder_knowledge = Knowledge::default();

        for agent in &agents {
            intruder_knowledge.0.push(agent.agent_id);
        }
        for agent in &protocol.agents {
            if agent.name.0.is_constant() {
                continue;
            }

            for term in &agent.initial_knowledge.0 {
                let term = term.replace_agent_with_intruder(&agent.name);
                let registered_term = converter.register_typed_term(
                    &ForWho::Intruder(session_id),
                    &protocol.initiations,
                    term.clone(),
                );

                if intruder_knowledge
                    .0
                    .iter()
                    .all(|&term| !converter.unifier.are_unified(term, registered_term))
                {
                    intruder_knowledge.0.push(registered_term);
                }
            }
        }

        Session {
            session_id,
            agents,
            intruder_knowledge,
            secrets,
        }
    }
    pub fn print(&self, unifier: &mut Unifier) {
        println!();
        println!("#############");
        println!("# SESSION {} #", self.session_id.0);
        println!("#############");
        for agent in &self.agents {
            println!();
            println!("> {} ({:?})", agent.name, agent.agent_id);
            println!(
                "> IK: {:?}",
                agent
                    .initial_knowledge
                    .0
                    .iter()
                    .map(|&term| unifier.resolve_full(term))
                    .collect_vec()
            );
            for t in &agent.strand {
                println!(
                    ">> {:?}->{:?}: {:?}",
                    unifier.resolve_full(t.sender),
                    unifier.resolve_full(t.receiver),
                    t.terms
                        .iter()
                        .map(|&term| unifier.resolve_full(term))
                        .collect_vec()
                );
            }
        }
        println!();
        println!("=== SECRETS ===");
        for secret in &self.secrets {
            println!("{:?}", unifier.resolve_full(secret.term));
        }
    }
}

impl SessionAgent {
    pub fn new(
        protocol: &Protocol,
        session_id: SessionId,
        converter: &mut Converter,
        agent: &ProtocolAgent,
    ) -> SessionAgent {
        let for_who = ForWho::Agent(session_id, agent.name.clone());

        let mut initial_knowledge: Vec<_> = agent
            .initial_knowledge
            .iter()
            .map(|term| {
                converter.register_typed_term(&for_who, &protocol.initiations, term.clone())
            })
            .collect();

        let initiates = agent.terms.iter().flat_map(|pattern| {
            if pattern.direction == Direction::Outgoing {
                pattern.initiates.clone()
            } else {
                Default::default()
            }
        });

        initial_knowledge.extend(
            initiates.map(|var| {
                converter.initiate_typed_variable(&for_who, &protocol.initiations, &var)
            }),
        );

        initial_knowledge.sort_unstable_by_key(|term| converter.unifier.resolve_full(*term));
        initial_knowledge.dedup();

        let strand = agent
            .terms
            .iter()
            .map(|pattern| Transaction {
                ast_node: pattern.clone(),
                sender: converter.get_agent(&for_who, &pattern.from),
                receiver: converter.get_agent(&for_who, &pattern.to),
                direction: pattern.direction,
                terms: pattern
                    .packet
                    .iter()
                    .map(|term| {
                        converter.register_typed_term(&for_who, &protocol.initiations, term.clone())
                    })
                    .collect(),
            })
            .collect();

        SessionAgent {
            name: agent.name.0.clone(),
            agent_id: converter.get_agent(&for_who, &agent.name),
            initial_knowledge: Knowledge(initial_knowledge),
            strand,
        }
    }
}
