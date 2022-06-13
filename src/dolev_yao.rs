use crate::protocol::{Func, Knowledge, Message, Stage};

pub fn composition_search<S: Stage>(
    knowledge: &Knowledge<S>,
    goal: &Message<S>,
    public_functions: &[Func],
) -> bool {
    match goal {
        Message::Composition { func, args } => {
            public_functions.contains(func)
                && args
                    .iter()
                    .all(|arg| composition_search(knowledge, arg, public_functions))
        }
        _ => knowledge.0.contains(goal),
    }
}

pub fn augment_knowledge<S: Stage>(knowledge: &mut Knowledge<S>, public_functions: &[Func]) {
    loop {
        let mut new_messages = Vec::new();

        for message in &knowledge.0 {
            match message {
                Message::SymEnc { message, key } => {
                    if composition_search(knowledge, key, public_functions) {
                        new_messages.push(*message.clone());
                    }
                }
                Message::AsymEnc { message, key } => {
                    if let Message::Inverse(_key) = key.as_ref() {
                        new_messages.push(*message.clone());
                    } else if composition_search(
                        knowledge,
                        &Message::Inverse((*key).clone()),
                        public_functions,
                    ) {
                        new_messages.push(*message.clone());
                    }
                }
                Message::Tuple(messages) => {
                    for m in messages {
                        new_messages.push(m.clone());
                    }
                }
                _ => {}
            }
        }

        new_messages.retain(|message| !knowledge.0.contains(message));

        if new_messages.is_empty() {
            return;
        }

        knowledge.0.append(&mut new_messages);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::protocol::UntypedStage;

    #[test]
    fn simple_composition_slides_example() {
        /*
           init knowledge: {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           goal: h(k_1, k_2)


           axiom      axiom
           --------  ---------
           M |- k_1  M |- k_2
           -------------------
           M |- h(k_1, k_2)
        */

        let goal: Message<UntypedStage> = Message::Composition {
            func: Func("h".to_string()),
            args: vec![
                Message::Variable("k1".to_string()),
                Message::Variable("k2".to_string()),
            ],
        }; // h(k_1, k_2)

        let knowledge = Knowledge(vec![
            Message::Variable("k1".to_string()),
            Message::Variable("k2".to_string()),
            Message::SymEnc {
                message: Box::new(Message::Tuple(vec![
                    Message::Variable("n1".to_string()),
                    Message::Variable("k3".to_string()),
                ])),
                key: Box::new(Message::Composition {
                    func: Func("h".to_string()),
                    args: vec![
                        Message::Variable("k1".to_string()),
                        Message::Variable("k2".to_string()),
                    ],
                }),
            },
            Message::SymEnc {
                message: Box::new(Message::Variable("n2".to_string())),
                key: Box::new(Message::Variable("k3".to_string())),
            },
        ]); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        assert!(composition_search(
            &knowledge,
            &goal,
            &[Func("h".to_string())]
        ));
    }

    #[test]
    fn non_provable_composition() {
        /*
           init knowledge: {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           goal: h(k_1, k_3)
        */

        let goal: Message<UntypedStage> = Message::Composition {
            func: Func("h".to_string()),
            args: vec![
                Message::Variable("k1".to_string()),
                Message::Variable("k3".to_string()),
            ],
        }; // h(k_1, k_2)

        let knowledge = Knowledge(vec![
            Message::Variable("k1".to_string()),
            Message::Variable("k2".to_string()),
            Message::SymEnc {
                message: Box::new(Message::Tuple(vec![
                    Message::Variable("n1".to_string()),
                    Message::Variable("k3".to_string()),
                ])),
                key: Box::new(Message::Composition {
                    func: Func("h".to_string()),
                    args: vec![
                        Message::Variable("k1".to_string()),
                        Message::Variable("k2".to_string()),
                    ],
                }),
            },
            Message::SymEnc {
                message: Box::new(Message::Variable("n2".to_string())),
                key: Box::new(Message::Variable("k3".to_string())),
            },
        ]); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        assert!(!composition_search(
            &knowledge,
            &goal,
            &[Func("h".to_string())]
        ));
    }

    #[test]
    fn non_public_function() {
        /*
           init knowledge: {k1, k_2 }
           pub: h
           goal: f(k_1, k_2)
        */

        let goal: Message<UntypedStage> = Message::Composition {
            func: Func("f".to_string()),
            args: vec![
                Message::Variable("k1".to_string()),
                Message::Variable("k2".to_string()),
            ],
        }; // h(k_1, k_3)

        let knowledge = Knowledge(vec![
            Message::Variable("k1".to_string()),
            Message::Variable("k2".to_string()),
        ]); // {k1, k2}

        assert!(!composition_search(
            &knowledge,
            &goal,
            &[Func("h".to_string())]
        ));
    }

    #[test]
    fn augment_knowledge_slides_example() {
        /*
           init knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           final knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3, <n1, k3>, n1, k3, n2 }
        */
        let mut knowledge = Knowledge::<UntypedStage>(vec![
            Message::Variable("k1".to_string()),
            Message::Variable("k2".to_string()),
            Message::SymEnc {
                message: Box::new(Message::Tuple(vec![
                    Message::Variable("n1".to_string()),
                    Message::Variable("k3".to_string()),
                ])),
                key: Box::new(Message::Composition {
                    func: Func("h".to_string()),
                    args: vec![
                        Message::Variable("k1".to_string()),
                        Message::Variable("k2".to_string()),
                    ],
                }),
            },
            Message::SymEnc {
                message: Box::new(Message::Variable("n2".to_string())),
                key: Box::new(Message::Variable("k3".to_string())),
            },
        ]); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        augment_knowledge(&mut knowledge, &[Func("h".to_string())]);

        assert_eq!(
            knowledge,
            Knowledge(vec![
                Message::Variable("k1".to_string()),
                Message::Variable("k2".to_string()),
                Message::SymEnc {
                    message: Box::new(Message::Tuple(vec![
                        Message::Variable("n1".to_string()),
                        Message::Variable("k3".to_string()),
                    ])),
                    key: Box::new(Message::Composition {
                        func: Func("h".to_string()),
                        args: vec![
                            Message::Variable("k1".to_string()),
                            Message::Variable("k2".to_string()),
                        ],
                    }),
                },
                Message::SymEnc {
                    message: Box::new(Message::Variable("n2".to_string())),
                    key: Box::new(Message::Variable("k3".to_string())),
                },
                Message::Tuple(vec![
                    Message::Variable("n1".to_string()),
                    Message::Variable("k3".to_string()),
                ]),
                Message::Variable("n1".to_string()),
                Message::Variable("k3".to_string()),
                Message::Variable("n2".to_string()),
            ])
        );
    }
}
