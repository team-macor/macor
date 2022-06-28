use itertools::Itertools;

use crate::protocol::Func;
use crate::terms::{FullTerm, Term, TermId, Unifier};

#[derive(Debug, Clone, Default)]
pub struct Knowledge(Vec<TermId>);

impl Knowledge {
    pub fn new(terms: Vec<TermId>) -> Knowledge {
        Knowledge(terms)
    }
    pub fn resoled(&self, unifier: &mut Unifier) -> Vec<FullTerm> {
        self.0.iter().map(|&id| unifier.resolve_full(id)).collect()
    }
    pub fn iter(&self) -> impl Iterator<Item = TermId> + '_ {
        self.0.iter().copied()
    }
    pub fn add(&mut self, term: TermId) {
        self.0.push(term)
    }
    pub fn extend(&mut self, terms: impl IntoIterator<Item = TermId>) {
        self.0.extend(terms)
    }
    pub fn contains_term_id(&self, term: TermId) -> bool {
        self.0.contains(&term)
    }
    pub fn can_construct(&self, unifier: &mut Unifier, term: TermId) -> bool {
        // eprintln!(
        //     "Can construct {:?} with knowledge {:?}",
        //     unifier.resolve_full(term),
        //     self.0
        //         .iter()
        //         .map(|&term| unifier.resolve_full(term))
        //         .format(", ")
        // );
        // if self.0.iter().any(|&k| unifier.table.unioned(k, term)) {
        //     return true;
        // }
        // if self.0.iter().any(|&k| unifier.unify(k, term).is_ok()) {
        //     return true;
        // }

        if can_derive(self, term, unifier) {
            return true;
        }

        false
    }
}

impl IntoIterator for Knowledge {
    type Item = TermId;

    type IntoIter = std::vec::IntoIter<TermId>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Knowledge {
    type Item = TermId;

    type IntoIter = std::iter::Copied<std::slice::Iter<'a, TermId>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
    }
}

pub fn can_derive(knowledge: &Knowledge, goal: TermId, unifier: &mut Unifier) -> bool {
    let mut stack = vec![goal];

    while let Some(term) = stack.pop() {
        // Axiom
        if knowledge.iter().any(|t| unifier.are_unified(t, term)) {
            continue;
        }
        if knowledge.iter().any(|t| unifier.unify(t, term).is_ok()) {
            continue;
        }

        // Compose
        // dbg!(unifier.resolve_full(term));
        match unifier.probe_value(term) {
            Term::Composition(func, args) => {
                match func {
                    Func::Inv => return false,
                    Func::User(c) => {
                        if !knowledge.iter().any(|f| unifier.are_unified(f, c)) {
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
        for term in knowledge.iter() {
            match unifier.probe_value(term) {
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

        new_terms.retain(|term| !knowledge.contains_term_id(*term));

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
    use super::*;
    use crate::lower::LoweringContext;
    use itertools::Itertools;

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

            let mut knowledge = Knowledge::new(
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
