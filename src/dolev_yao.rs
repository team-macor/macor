use crate::protocol::Func;
use crate::terms::{Knowledge, Term, TermId, Unifier};

pub fn can_derive(knowledge: &Knowledge, goal: TermId, unifier: &mut Unifier) -> bool {
    let mut stack = vec![goal];

    while let Some(term) = stack.pop() {
        // Axiom
        if knowledge.0.iter().any(|t| unifier.are_unified(*t, term)) {
            continue;
        }
        if knowledge.0.iter().any(|t| unifier.unify(*t, term).is_ok()) {
            continue;
        }

        // Compose
        // dbg!(unifier.resolve_full(term));
        match unifier.probe_value(term) {
            Term::Composition(func, args) => {
                match func {
                    Func::Inv => return false,
                    Func::User(c) => {
                        if !knowledge.0.iter().any(|f| unifier.are_unified(*f, c)) {
                            return false;
                        }
                    }
                    _ => {}
                }
                for a in args {
                    stack.push(a);
                }
            }
            Term::Tuple(ts) => {
                for a in ts {
                    stack.push(a);
                }
            }
            _ => return false,
        }
    }
    true
}

pub fn augment_knowledge(knowledge: &mut Knowledge, unifier: &mut Unifier) {
    let mut new_terms: Vec<TermId> = Vec::new();
    loop {
        for term in knowledge.0.iter() {
            match unifier.probe_value(*term) {
                Term::Composition(func, args) => match func {
                    Func::SymEnc => {
                        if args.len() != 2 {
                            unreachable!("arguments for symmetric encrypt function must only contain 2 arguments")
                        }
                        let (term, key) = (&args[0], &args[1]);

                        if can_derive(knowledge, *key, unifier) {
                            // println!(
                            //     "adding term {:?} to new_terms",
                            //     unifier.resolve_full(*term)
                            // );
                            new_terms.push(*term);
                        }
                    }
                    Func::AsymEnc => {
                        if args.len() != 2 {
                            unreachable!("arguments for asymmetric encrypt function must only contain 2 arguments");
                        }
                        let (term, key) = (&args[0], &args[1]);

                        if let Term::Composition(Func::Inv, _) = unifier.probe_value(*key) {
                            new_terms.push(*term);
                        }

                        // TODO: inv(key) should be goal here, not term
                        if can_derive(knowledge, *term, unifier) {
                            new_terms.push(*term);
                        }
                    }
                    _ => {}
                },
                Term::Tuple(terms) => {
                    for m in terms {
                        new_terms.push(m);
                    }
                }
                _ => {}
            }
        }

        new_terms.retain(|term| !knowledge.0.contains(term));

        // remove tuples
        if new_terms.is_empty() {
            knowledge
                .0
                .retain(|m| !matches!(unifier.probe_value(*m), Term::Tuple(_)));
            knowledge.0.sort_unstable();
            knowledge.0.dedup();
            return;
        }

        knowledge.0.append(&mut new_terms);

        assert_eq!(new_terms.len(), 0);
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{lower::LoweringContext, terms::Knowledge};

    // TODO: needs to handle term with commas like {|A, B|} as 1 term (call parser)
    macro_rules! scenario {
        (knowledge : $k:tt ; goals : $g:tt ;) => {
            // let knowledge: Vec<_> = $k.split(",").into_iter().map(|s| s.to_string()).collect();
            // let goals = vec![$(stringify!($g)),+];

            let knowledge = macor_parse::parse_terms($k).unwrap();
            let goals = macor_parse::parse_neg_terms($g).unwrap();

            let mut unifier = Default::default();
            let mut mapper = Default::default();
            let mut ctx = LoweringContext::new(&mut unifier, &mut mapper);

            let mut knowledge = Knowledge(
                knowledge
                    .into_iter()
                    .map(|k| ctx.lower_ast_term(k.into()))
                    .collect_vec(),
            );

            let goals = goals
                .into_iter()
                .map(|(g, b)| (b, ctx.lower_ast_term(g.into())))
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
