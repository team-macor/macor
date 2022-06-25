use itertools::Itertools;

use crate::messages::{Knowledge, Message, MessageId, Unifier};
use crate::protocol::Func;

pub fn can_derive(knowledge: &Knowledge, goal: MessageId, unifier: &mut Unifier) -> bool {
    let mut stack = vec![goal];

    while let Some(message) = stack.pop() {
        // Axiom
        if knowledge
            .0
            .iter()
            .any(|msg| unifier.are_unified(*msg, message))
        {
            continue;
        }
        if knowledge
            .0
            .iter()
            .any(|msg| unifier.unify(*msg, message).is_ok())
        {
            continue;
        }

        // Compose
        // dbg!(unifier.resolve_full(message));
        match unifier.probe_value(message) {
            Message::Composition(func, args) => {
                match func {
                    Func::Inv => return false,
                    Func::User(c) => {
                        if !knowledge
                            .0
                            .iter()
                            .any(|f| unifier.probe_value(*f) == Message::Constant(c))
                        {
                            return false;
                        }
                    }
                    _ => {}
                }
                for a in args {
                    stack.push(a);
                }
            }
            Message::Tuple(ts) => {
                for a in ts {
                    stack.push(a);
                }
            }
            Message::Agent(_) => {
                println!(
                    "I accept {:?}, but who to unify with {:?} 🤔",
                    unifier.resolve_full(message),
                    knowledge
                        .0
                        .iter()
                        .map(|&k| unifier.resolve_full(k))
                        .collect_vec(),
                )
            }
            _ => return false,
        }
    }
    true
}

pub fn augment_knowledge(knowledge: &mut Knowledge, unifier: &mut Unifier) {
    let mut new_messages: Vec<MessageId> = Vec::new();
    loop {
        for message in knowledge.0.iter() {
            match unifier.probe_value(*message) {
                Message::Composition(func, args) => match func {
                    Func::SymEnc => {
                        if args.len() != 2 {
                            unreachable!("arguments for symmetric encrypt function must only contain 2 arguments")
                        }
                        let (message, key) = (&args[0], &args[1]);

                        if can_derive(knowledge, *key, unifier) {
                            // println!(
                            //     "adding message {:?} to new_messages",
                            //     unifier.resolve_full(*message)
                            // );
                            new_messages.push(*message);
                        }
                    }
                    Func::AsymEnc => {
                        if args.len() != 2 {
                            unreachable!("arguments for asymmetric encrypt function must only contain 2 arguments");
                        }
                        let (message, key) = (&args[0], &args[1]);

                        if let Message::Composition(Func::Inv, _) = unifier.probe_value(*key) {
                            new_messages.push(*message);
                        }

                        // TODO: inv(key) should be goal here, not message
                        if can_derive(knowledge, *message, unifier) {
                            new_messages.push(*message);
                        }
                    }
                    _ => {}
                },
                Message::Tuple(messages) => {
                    for m in messages {
                        new_messages.push(m);
                    }
                }
                _ => {}
            }
        }

        new_messages.retain(|message| !knowledge.0.contains(message));

        // remove tuples
        if new_messages.is_empty() {
            knowledge
                .0
                .retain(|m| !matches!(unifier.probe_value(*m), Message::Tuple(_)));
            knowledge.0.sort_unstable();
            knowledge.0.dedup();
            return;
        }

        knowledge.0.append(&mut new_messages);

        assert_eq!(new_messages.len(), 0);
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::messages::{Converter, Knowledge};

    // TODO: needs to handle message with commas like {|A, B|} as 1 message (call parser)
    macro_rules! scenario {
        (knowledge : $k:tt ; goals : $g:tt ;) => {
            // let knowledge: Vec<_> = $k.split(",").into_iter().map(|s| s.to_string()).collect();
            // let goals = vec![$(stringify!($g)),+];

            let knowledge = macor_parse::parse_messages($k).unwrap();
            let goals = macor_parse::parse_neg_messages($g).unwrap();

            let mut unifier = Default::default();
            let mut mapper = Default::default();
            let mut converter = Converter::new(&mut unifier, &mut mapper);

            let mut knowledge = Knowledge(
                knowledge
                    .into_iter()
                    .map(|k| converter.register_ast_message(k.into()))
                    .collect_vec(),
            );

            let goals = goals
                .into_iter()
                .map(|(g, b)| (b, converter.register_ast_message(g.into())))
                .collect_vec();

            crate::dolev_yao::augment_knowledge(&mut knowledge, &mut unifier);
            for (expected, goal) in goals {
                assert_eq!(
                    expected,
                    crate::dolev_yao::can_derive(&knowledge, goal, &mut unifier)
                );
            }
        };
    }
    #[test]
    fn my_test() {
        scenario! {
            knowledge: "k_1, k_2, f";
            goals: "f(k_1, k_2), !h(k_1, k_2), { k_1 }(f(f(k_1), k_2))";
        };
    }

    #[test]
    fn requires_augment() {
        scenario! {
            knowledge: "key, { data }(inv(key))";
            goals: "data";
        };
    }

    #[test]
    fn derive_sym_enc() {
        scenario! {
            knowledge: "key, {| data |}(key)";
            goals: "data";
        };
    }
}
