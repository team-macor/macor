#![allow(dead_code)]

//! Stuff that might be improved:
//! - [ ] Don't use `String` everywhere.
//! - [ ] Can the `clone`s be omitted?
//! - [ ] Algorithmic improvements to avoid some of the nested loops (look at composition search).
//! - [ ] Box/dereferences are ugly.
//!     - Someone can try converting to [`box_syntax`](https://doc.rust-lang.org/beta/unstable-book/language-features/box-syntax.html).
//! - [ ] Macro for tests.
//! - [ ] Should the `new_messages` variable be some kind of set and then check disjoint.

#[derive(PartialEq, Eq, Debug, Clone)]
struct Func(String);

#[derive(PartialEq, Eq, Debug, Clone)]
enum Message {
    Variable(String),
    Constant(String),
    Composition {
        func: Func,
        args: Vec<Message>,
    },
    SymEnc {
        message: Box<Message>,
        key: Box<Message>,
    },
    AsymEnc {
        message: Box<Message>,
        key: Box<Message>,
    },
    Tuple(Box<Message>, Box<Message>),
    Inverse(Box<Message>),
    Exp(Box<Message>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Knowledge(Vec<Message>);

fn composition_search(knowledge: &Knowledge, goal: &Message, public_functions: &[Func]) -> bool {
    match goal {
        Message::Composition { func, args } => {
            public_functions.contains(&func)
                && args
                    .iter()
                    .all(|arg| composition_search(knowledge, arg, public_functions))
        }
        _ => knowledge.0.contains(&goal),
    }
}

fn augment_knowledge(knowledge: &mut Knowledge, public_functions: &[Func]) {
    loop {
        let mut new_messages = Vec::new();

        for message in &knowledge.0 {
            match message {
                Message::SymEnc { message, key } => {
                    if composition_search(knowledge, *&key, public_functions) {
                        new_messages.push((**message).clone());
                    }
                }
                Message::AsymEnc { message, key } => {
                    if let Message::Inverse(_key) = &**key {
                        new_messages.push((**message).clone());
                    } else if composition_search(
                        knowledge,
                        &Message::Inverse((*key).clone()),
                        public_functions,
                    ) {
                        new_messages.push((**message).clone());
                    }
                }
                Message::Tuple(m1, m2) => {
                    new_messages.push((**m1).clone());
                    new_messages.push((**m2).clone());
                }
                _ => {}
            }
        }

        new_messages.retain(|message| !knowledge.0.contains(message));

        if new_messages.len() == 0 {
            return;
        }

        knowledge.0.append(&mut new_messages);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let goal = Message::Composition {
            func: Func("h".to_string()),
            args: vec![
                Message::Constant("k1".to_string()),
                Message::Constant("k2".to_string()),
            ],
        }; // h(k_1, k_2)

        let knowledge = Knowledge(vec![
            Message::Constant("k1".to_string()),
            Message::Constant("k2".to_string()),
            Message::SymEnc {
                message: Box::new(Message::Tuple(
                    Box::new(Message::Constant("n1".to_string())),
                    Box::new(Message::Constant("k3".to_string())),
                )),
                key: Box::new(Message::Composition {
                    func: Func("h".to_string()),
                    args: vec![
                        Message::Constant("k1".to_string()),
                        Message::Constant("k2".to_string()),
                    ],
                }),
            },
            Message::SymEnc {
                message: Box::new(Message::Constant("n2".to_string())),
                key: Box::new(Message::Constant("k3".to_string())),
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

        let goal = Message::Composition {
            func: Func("h".to_string()),
            args: vec![
                Message::Constant("k1".to_string()),
                Message::Constant("k3".to_string()),
            ],
        }; // h(k_1, k_2)

        let knowledge = Knowledge(vec![
            Message::Constant("k1".to_string()),
            Message::Constant("k2".to_string()),
            Message::SymEnc {
                message: Box::new(Message::Tuple(
                    Box::new(Message::Constant("n1".to_string())),
                    Box::new(Message::Constant("k3".to_string())),
                )),
                key: Box::new(Message::Composition {
                    func: Func("h".to_string()),
                    args: vec![
                        Message::Constant("k1".to_string()),
                        Message::Constant("k2".to_string()),
                    ],
                }),
            },
            Message::SymEnc {
                message: Box::new(Message::Constant("n2".to_string())),
                key: Box::new(Message::Constant("k3".to_string())),
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

        let goal = Message::Composition {
            func: Func("f".to_string()),
            args: vec![
                Message::Constant("k1".to_string()),
                Message::Constant("k2".to_string()),
            ],
        }; // h(k_1, k_3)

        let knowledge = Knowledge(vec![
            Message::Constant("k1".to_string()),
            Message::Constant("k2".to_string()),
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
        let mut knowledge = Knowledge(vec![
            Message::Constant("k1".to_string()),
            Message::Constant("k2".to_string()),
            Message::SymEnc {
                message: Box::new(Message::Tuple(
                    Box::new(Message::Constant("n1".to_string())),
                    Box::new(Message::Constant("k3".to_string())),
                )),
                key: Box::new(Message::Composition {
                    func: Func("h".to_string()),
                    args: vec![
                        Message::Constant("k1".to_string()),
                        Message::Constant("k2".to_string()),
                    ],
                }),
            },
            Message::SymEnc {
                message: Box::new(Message::Constant("n2".to_string())),
                key: Box::new(Message::Constant("k3".to_string())),
            },
        ]); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        augment_knowledge(&mut knowledge, &[Func("h".to_string())]);

        assert_eq!(
            knowledge,
            Knowledge(vec![
                Message::Constant("k1".to_string()),
                Message::Constant("k2".to_string()),
                Message::SymEnc {
                    message: Box::new(Message::Tuple(
                        Box::new(Message::Constant("n1".to_string())),
                        Box::new(Message::Constant("k3".to_string())),
                    )),
                    key: Box::new(Message::Composition {
                        func: Func("h".to_string()),
                        args: vec![
                            Message::Constant("k1".to_string()),
                            Message::Constant("k2".to_string()),
                        ],
                    }),
                },
                Message::SymEnc {
                    message: Box::new(Message::Constant("n2".to_string())),
                    key: Box::new(Message::Constant("k3".to_string())),
                },
                Message::Tuple(
                    Box::new(Message::Constant("n1".to_string())),
                    Box::new(Message::Constant("k3".to_string())),
                ),
                Message::Constant("n1".to_string()),
                Message::Constant("k3".to_string()),
                Message::Constant("n2".to_string()),
            ])
        );
    }
}
