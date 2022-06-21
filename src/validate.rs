use itertools::Itertools;
use macor_parse::ast::Ident;
use miette::SourceSpan;
use smol_str::SmolStr;

use crate::{
    dolev_yao,
    messages::{Converter, FullMessage, Knowledge, MessageId, Session, Unifier},
    protocol::{Direction, Protocol, SessionId},
};

struct ActorState {
    id: MessageId,
    knowledge: Knowledge,
    step: usize,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
pub enum ValidateError {
    #[error("An agent cannot send messages to themselves")]
    CannotSendSelf {
        #[source_code]
        src: String,
        #[label("Here, the actor '{actor:?}' tries to send a message to themselves")]
        err_span: SourceSpan,
        actor: Ident<SmolStr>,
    },
    #[error("Actor does not exist")]
    ActorDoesNotExist {
        #[source_code]
        src: String,
        #[label("This actor '{actor:?}' does not exist in the protocol")]
        err_span: SourceSpan,
        actor: Ident<SmolStr>,
    },
    #[error("Message not constructable")]
    MessageIsNotConstructable {
        #[source_code]
        src: String,
        #[label("At this point in the protocol the actor '{actor:?}' cannot construct:\n  {msg:?}\nTheir current knowledge is:\n  {:?}", knowledge.iter().format(", "))]
        err_span: SourceSpan,
        actor: Ident<SmolStr>,
        msg: FullMessage,
        knowledge: Vec<FullMessage>,
    },
}

impl Protocol {
    pub fn validate(&self, src: &str) -> Vec<ValidateError> {
        let mut unifier = Default::default();
        let mut mapper = Default::default();
        let mut converter = Converter::new(&mut unifier, &mut mapper);

        let session = Session::new(self, SessionId(0), &mut converter);

        validate(src, &mut unifier, &session)
    }
}

pub fn validate(src: &str, unifier: &mut Unifier, session: &Session) -> Vec<ValidateError> {
    let mut errors = vec![];

    let mut actor_states = session
        .actors
        .iter()
        .map(|actor| ActorState {
            id: actor.actor_id,
            knowledge: actor.initial_knowledge.clone(),
            step: 0,
        })
        .collect_vec();

    loop {
        let mut did_progress = false;

        for (sender_i, sender) in session.actors.iter().enumerate() {
            let sender_state = &actor_states[sender_i];

            if sender.strand.len() == sender_state.step {
                continue;
            }

            let sender_msg = &sender.strand[sender_state.step];

            if sender_msg.sender == sender_msg.receiver {
                errors.push(ValidateError::CannotSendSelf {
                    src: src.to_string(),
                    err_span: sender_msg.ast_node.to.span(),
                    actor: sender_msg.ast_node.to.0.clone(),
                });
                continue;
            }

            if sender_msg.direction == Direction::Outgoing {
                if let Some(recipient_i) = actor_states
                    .iter()
                    .position(|a| a.id == sender_msg.receiver)
                {
                    let recipient = &session.actors[recipient_i];
                    let recipient_state = &actor_states[recipient_i];

                    if recipient.strand.len() == recipient_state.step {
                        todo!()
                    }

                    let recipient_msg = &recipient.strand[recipient_state.step];

                    if recipient_msg.direction == Direction::Outgoing {
                        todo!()
                    }

                    if sender_msg.messages.len() != recipient_msg.messages.len() {
                        todo!()
                    }

                    for (msg_i, (a, b)) in sender_msg
                        .messages
                        .iter()
                        .zip_eq(recipient_msg.messages.iter())
                        .enumerate()
                    {
                        if unifier.unify(*a, *b).is_err() {
                            todo!()
                        }
                    }

                    for (msg_i, msg) in sender_msg.messages.iter().enumerate() {
                        if !sender_state.knowledge.can_construct(unifier, *msg) {
                            errors.push(ValidateError::MessageIsNotConstructable {
                                src: src.to_string(),
                                err_span: sender_msg.ast_node.packet.messages[msg_i]
                                    .span()
                                    .unwrap(),
                                actor: sender.name.clone(),
                                msg: unifier.resolve_full(sender_msg.messages[msg_i]),
                                knowledge: sender_state
                                    .knowledge
                                    .0
                                    .iter()
                                    .map(|&id| unifier.resolve_full(id))
                                    .collect(),
                            })
                        }
                    }

                    actor_states[sender_i].step += 1;
                    actor_states[recipient_i].step += 1;

                    actor_states[recipient_i]
                        .knowledge
                        .0
                        .extend(sender_msg.messages.iter().cloned());
                    dolev_yao::augment_knowledge(&mut actor_states[recipient_i].knowledge, unifier);

                    did_progress = true;
                } else {
                    errors.push(ValidateError::ActorDoesNotExist {
                        src: src.to_string(),
                        err_span: sender_msg.ast_node.to.span(),
                        actor: sender_msg.ast_node.to.0.clone(),
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
