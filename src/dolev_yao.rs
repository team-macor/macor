use crate::protocol::Func;
use crate::terms::{FullTerm, Term, TermId, Unifier};

#[derive(Debug, Clone, Default)]
pub struct Knowledge {
    terms: Vec<TermId>,
    augmented_terms: Vec<TermId>,
}

impl Knowledge {
    pub fn new(terms: Vec<TermId>) -> Knowledge {
        Knowledge {
            terms,
            augmented_terms: vec![],
        }
    }
    pub fn resoled(&self, unifier: &mut Unifier) -> Vec<FullTerm> {
        self.iter().map(|id| unifier.resolve_full(id)).collect()
    }
    pub fn iter(&self) -> impl Iterator<Item = TermId> + '_ {
        self.terms.iter().chain(&self.augmented_terms).copied()
    }
    pub fn add(&mut self, term: TermId) {
        self.terms.push(term)
    }
    pub fn extend(&mut self, terms: impl IntoIterator<Item = TermId>) {
        self.terms.extend(terms)
    }
    pub fn contains_term_id(&self, term: TermId) -> bool {
        self.terms.contains(&term) || self.augmented_terms.contains(&term)
    }
    pub fn can_derive_inv(&self, unifier: &mut Unifier, goal: TermId) -> bool {
        self.iter().any(|t| match unifier.probe_value(t) {
            Term::Composition(Func::Inv, args) => {
                assert_eq!(args.len(), 1);
                unifier.are_unified(args[0], goal)
            }
            _ => false,
        })
    }
    pub fn can_derive(&self, unifier: &mut Unifier, goal: TermId) -> bool {
        let mut stack = vec![goal];

        while let Some(term) = stack.pop() {
            // Axiom
            if self.iter().any(|t| unifier.are_unified(t, term)) {
                continue;
            }
            // TODO: This step can branch depending on which term from the
            // knowledge was unified with. Consider propagating the branching up
            // throughout the search.
            if self.iter().any(|t| unifier.unify(t, term).is_ok()) {
                continue;
            }

            // Compose
            match unifier.probe_value(term) {
                Term::Composition(func, args) => {
                    match func {
                        Func::Inv => return false,
                        Func::User(c) => {
                            if !self.iter().any(|f| unifier.are_unified(f, c)) {
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
    pub fn augment_knowledge(&mut self, unifier: &mut Unifier) {
        enum Move {
            Keep,
            Move,
            Remove,
        }

        let mut moves = vec![];
        let mut new_terms = vec![];
        loop {
            moves.extend(
                self.terms
                    .iter()
                    .map(|&term| match unifier.probe_value(term) {
                        Term::Composition(func @ (Func::SymEnc | Func::AsymEnc), args) => {
                            let (inner, key) = if let &[inner, key] = &args[..] {
                                (inner, key)
                            } else {
                                unreachable!("{func:?} must only contain 2 arguments")
                            };

                            match func {
                                // Symmetric encryption requires the same key as
                                // encrypted with
                                Func::SymEnc if self.can_derive(unifier, key) => {
                                    new_terms.push(inner);
                                    Move::Move
                                }
                                // Singed terms can always be unwrapped
                                Func::AsymEnc if unifier.probe_value(key).is_inv() => {
                                    new_terms.push(inner);
                                    Move::Move
                                }
                                // Encrypted terms can be decrypted if the
                                // inverse key is in the knowledge
                                Func::AsymEnc
                                    if self.iter().any(|t| match unifier.probe_value(t) {
                                        Term::Composition(Func::Inv, args) => {
                                            assert_eq!(args.len(), 1);
                                            unifier.are_unified(args[0], key)
                                        }
                                        _ => false,
                                    }) =>
                                {
                                    new_terms.push(inner);
                                    Move::Move
                                }
                                _ => Move::Keep,
                            }
                        }
                        Term::Composition(_, _) => Move::Move,
                        Term::Tuple(terms) => {
                            new_terms.extend(&terms);
                            Move::Remove
                        }
                        Term::Variable(_, _) => Move::Keep,
                        Term::Constant(_, _) => Move::Move,
                        Term::Intruder => Move::Move,
                    }),
            );

            for (i, m) in moves.drain(..).enumerate().rev() {
                match m {
                    Move::Keep => {}
                    Move::Move => self.augmented_terms.push(self.terms.swap_remove(i)),
                    Move::Remove => {
                        self.terms.swap_remove(i);
                    }
                }
            }

            new_terms.retain(|term| !self.contains_term_id(*term));

            if new_terms.is_empty() {
                self.terms.sort_unstable();
                self.terms.dedup();
                self.augmented_terms.sort_unstable();
                self.augmented_terms.dedup();
                return;
            }

            self.terms.append(&mut new_terms);

            assert_eq!(new_terms.len(), 0);
        }
    }
}

impl IntoIterator for Knowledge {
    type Item = TermId;

    type IntoIter = std::iter::Chain<std::vec::IntoIter<TermId>, std::vec::IntoIter<TermId>>;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter().chain(self.augmented_terms)
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

            knowledge.augment_knowledge(&mut unifier);
            for (expected, goal) in goals {
                assert_eq!(expected, knowledge.can_derive(&mut unifier, goal));
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
    fn derive_simple_composition() {
        scenario! {
            knowledge: "f(k_1)";
            goals: "f(k_1)";
        };
    }

    #[test]
    fn derive_sym_enc() {
        scenario! {
            knowledge: "key, {| data |}(key)";
            goals: "data";
        };
    }

    #[test]
    fn derive_asym_enc() {
        scenario! {
            knowledge: "key, { data }(inv(key))";
            goals: "data";
        };
        scenario! {
            knowledge: "inv(key), { data }(key)";
            goals: "data";
        };
    }
}
