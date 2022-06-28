use itertools::Itertools;
use macor_parse::ast::Ident;
use smol_str::SmolStr;

use crate::{
    dolev_yao::Knowledge,
    lower::{ForWho, LoweringContext},
    protocol::{self, Direction, Protocol, ProtocolAgent, SessionId},
    terms::{TermId, Unifier},
};

#[derive(Debug, Clone)]
pub struct Transaction {
    pub ast_node: protocol::MessagePattern,
    pub sender: TermId,
    pub receiver: TermId,
    pub post_knowledge: Knowledge,
    pub direction: Direction,
    pub terms: Vec<TermId>,
}

#[derive(Debug, Clone)]
pub struct Role {
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
    pub roles: Vec<Role>,
    pub intruder_knowledge: Knowledge,
    pub secrets: Vec<Secret>,
}

impl Session {
    pub fn new(protocol: &Protocol, session_id: SessionId, ctx: &mut LoweringContext) -> Session {
        let roles = protocol
            .agents
            .iter()
            .map(|agent| Role::new(protocol, session_id, ctx, agent))
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
                            .map(|agent| ctx.get_agent(&ForWho::Intruder(session_id), agent))
                            .collect(),
                        term: ctx.lower_term(
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

        for role in &roles {
            intruder_knowledge.add(role.agent_id);
        }
        for agent in &protocol.agents {
            if agent.name.is_constant() {
                continue;
            }

            for term in agent.initial_knowledge.iter() {
                let term = term.replace_agent_with_intruder(&agent.name);
                let registered_term = ctx.lower_term(
                    &ForWho::Intruder(session_id),
                    &protocol.initiations,
                    term.clone(),
                );

                if intruder_knowledge
                    .iter()
                    .all(|term| !ctx.unifier.are_unified(term, registered_term))
                {
                    intruder_knowledge.add(registered_term);
                }
            }
        }

        Session {
            session_id,
            roles,
            intruder_knowledge,
            secrets,
        }
    }
    pub fn print(&self, unifier: &mut Unifier) {
        println!();
        println!("#############");
        println!("# SESSION {} #", self.session_id.0);
        println!("#############");
        for role in &self.roles {
            println!();
            println!("> {} ({:?})", role.name, role.agent_id);
            println!(
                "> IK: {:?}",
                role.initial_knowledge
                    .iter()
                    .map(|term| unifier.resolve_full(term))
                    .collect_vec()
            );
            for t in &role.strand {
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

impl Role {
    pub fn new(
        protocol: &Protocol,
        session_id: SessionId,
        ctx: &mut LoweringContext,
        agent: &ProtocolAgent,
    ) -> Role {
        let for_who = ForWho::Agent(session_id, agent.name.clone());

        let mut initial_knowledge: Vec<_> = agent
            .initial_knowledge
            .iter()
            .map(|t| ctx.lower_term(&for_who, &protocol.initiations, t.clone()))
            .collect();

        let initiates = agent
            .messages
            .iter()
            .filter_map(|p| p.direction.is_outgoing().then(|| &p.initiates))
            .flatten();

        initial_knowledge
            .extend(initiates.map(|var| ctx.lower_variable(&for_who, &protocol.initiations, var)));

        initial_knowledge.sort_unstable_by_key(|&term| (ctx.unifier.resolve_full(term), term));
        initial_knowledge.dedup();

        let mut initial_knowledge = Knowledge::new(initial_knowledge);
        initial_knowledge.augment_knowledge(ctx.unifier);

        let mut current_knowledge = initial_knowledge.clone();

        let strand = agent
            .messages
            .iter()
            .map(|pattern| {
                let terms = pattern
                    .message
                    .iter()
                    .map(|t| ctx.lower_term(&for_who, &protocol.initiations, t.clone()))
                    .collect_vec();
                current_knowledge.extend(terms.iter().copied());
                current_knowledge.augment_knowledge(ctx.unifier);
                Transaction {
                    ast_node: pattern.clone(),
                    sender: ctx.get_agent(&for_who, &pattern.from),
                    receiver: ctx.get_agent(&for_who, &pattern.to),
                    direction: pattern.direction,
                    post_knowledge: current_knowledge.clone(),
                    terms,
                }
            })
            .collect();

        Role {
            name: agent.name.0.clone(),
            agent_id: ctx.get_agent(&for_who, &agent.name),
            initial_knowledge,
            strand,
        }
    }
}
