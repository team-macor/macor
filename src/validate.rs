use itertools::Itertools;
use macor_parse::ast::Ident;
use miette::SourceSpan;
use smol_str::SmolStr;

use crate::{
    dolev_yao::Knowledge,
    lower::LoweringContext,
    protocol::{Direction, Protocol, SessionId},
    sessions::Session,
    terms::{FullTerm, TermId, Unifier},
};

struct AgentState {
    id: TermId,
    knowledge: Knowledge,
    step: usize,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
pub enum ValidateError {
    #[error("An agent cannot send terms to themselves")]
    CannotSendSelf {
        #[source_code]
        src: String,
        #[label("Here, the agent '{agent:?}' tries to send a term to themselves")]
        err_span: SourceSpan,
        agent: Ident<SmolStr>,
    },
    #[error("Agent does not exist")]
    AgentDoesNotExist {
        #[source_code]
        src: String,
        #[label("This agent '{agent:?}' does not exist in the protocol")]
        err_span: SourceSpan,
        agent: Ident<SmolStr>,
    },
    #[error("Term not constructable")]
    TermIsNotConstructable {
        #[source_code]
        src: String,
        #[label("At this point in the protocol the agent '{agent:?}' cannot construct:\n  {term:?}\nTheir current knowledge is:\n  {:?}", knowledge.iter().format(", "))]
        err_span: SourceSpan,
        agent: Ident<SmolStr>,
        term: FullTerm,
        knowledge: Vec<FullTerm>,
    },
}

impl Protocol {
    pub fn validate(&self, src: &str) -> Vec<ValidateError> {
        let mut unifier = Default::default();
        let mut mapper = Default::default();
        let mut ctx = LoweringContext::new(&mut unifier, &mut mapper);

        let session = Session::new(self, SessionId(0), &mut ctx);

        validate(src, &mut unifier, &session)
    }
}

pub fn validate(src: &str, unifier: &mut Unifier, session: &Session) -> Vec<ValidateError> {
    let mut errors = vec![];

    let mut agent_states = session
        .roles
        .iter()
        .map(|role| AgentState {
            id: role.agent_id,
            knowledge: role.initial_knowledge.clone(),
            step: 0,
        })
        .collect_vec();

    loop {
        let mut did_progress = false;

        for (sender_i, sender) in session.roles.iter().enumerate() {
            let sender_state = &agent_states[sender_i];

            if sender.strand.len() == sender_state.step {
                continue;
            }

            let sender_term = &sender.strand[sender_state.step];

            if sender_term.sender == sender_term.receiver {
                errors.push(ValidateError::CannotSendSelf {
                    src: src.to_string(),
                    err_span: sender_term.ast_node.to.span(),
                    agent: sender_term.ast_node.to.0.clone(),
                });
                continue;
            }

            if sender_term.direction == Direction::Outgoing {
                if let Some(recipient_i) = agent_states
                    .iter()
                    .position(|a| a.id == sender_term.receiver)
                {
                    let recipient = &session.roles[recipient_i];
                    let recipient_state = &agent_states[recipient_i];

                    if recipient.strand.len() == recipient_state.step {
                        todo!()
                    }

                    let recipient_term = &recipient.strand[recipient_state.step];

                    if recipient_term.direction == Direction::Outgoing {
                        todo!()
                    }

                    if sender_term.terms.len() != recipient_term.terms.len() {
                        todo!()
                    }

                    for (term_i, (a, b)) in sender_term
                        .terms
                        .iter()
                        .zip_eq(recipient_term.terms.iter())
                        .enumerate()
                    {
                        if unifier.unify(*a, *b).is_err() {
                            todo!()
                        }
                    }

                    for (term_i, term) in sender_term.terms.iter().enumerate() {
                        if !sender_state.knowledge.can_derive(unifier, *term) {
                            errors.push(ValidateError::TermIsNotConstructable {
                                src: src.to_string(),
                                err_span: sender_term.ast_node.message.terms[term_i]
                                    .span()
                                    .unwrap(),
                                agent: sender.name.clone(),
                                term: unifier.resolve_full(sender_term.terms[term_i]),
                                knowledge: sender_state
                                    .knowledge
                                    .iter()
                                    .map(|id| unifier.resolve_full(id))
                                    .collect(),
                            })
                        }
                    }

                    agent_states[sender_i].step += 1;
                    agent_states[recipient_i].step += 1;

                    agent_states[recipient_i]
                        .knowledge
                        .extend(sender_term.terms.iter().cloned());
                    agent_states[recipient_i]
                        .knowledge
                        .augment_knowledge(unifier);

                    did_progress = true;
                } else {
                    errors.push(ValidateError::AgentDoesNotExist {
                        src: src.to_string(),
                        err_span: sender_term.ast_node.to.span(),
                        agent: sender_term.ast_node.to.0.clone(),
                    });
                }
            }
        }

        if !did_progress {
            break;
        }
    }

    errors
}
