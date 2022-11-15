use itertools::Itertools;

use crate::protocol::Func;
use crate::terms::{FullTerm, Kind, Term, TermId, Unifier};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Obligation<M> {
    ShouldUnify { opaque: M, actual: M },
}

impl<M> Obligation<M> {
    pub fn map<T>(self, mut f: impl FnMut(M) -> T) -> Obligation<T> {
        match self {
            Obligation::ShouldUnify { opaque, actual } => Obligation::ShouldUnify {
                opaque: f(opaque),
                actual: f(actual),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum WithUnification {
    #[default]
    No,
    Yes,
}

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
                unifier.are_equal(args[0], goal)
            }
            _ => false,
        })
    }
    pub fn can_derives<'a>(
        &'a self,
        unifier: &'a Unifier,
        goal: TermId,
    ) -> impl Iterator<Item = Unifier> + 'a {
        return std::iter::empty()
            .chain(
                // If we allow unification, just let the variables stay. This might
                // be an abuse for `with_unification`, but in situations where
                // unifications is allowed, it is also best to postpone it, see
                // (lazy-intruder)
                {
                    let mut unifier = unifier.clone();
                    unifier.probe_value(goal).is_variable().then(|| unifier)
                },
            )
            .chain(
                // Axiom
                {
                    let mut unifier = unifier.clone();
                    self.iter()
                        .any(|t| unifier.are_equal(t, goal))
                        .then(|| unifier)
                },
            )
            .chain({
                self.iter().filter_map(move |t| {
                    let mut unifier = unifier.clone();
                    if unifier.unify(t, goal).is_ok() {
                        Some(unifier)
                    } else {
                        None
                    }
                })
            })
            .chain((move || {
                // Compose
                let mut unifier = unifier.clone();
                match unifier.probe_value(goal) {
                    Term::Composition(func, args) => {
                        match func {
                            Func::Inv => return vec![],
                            Func::User(c) => {
                                if !self.iter().any(|f| unifier.are_equal(f, c)) {
                                    return vec![];
                                }
                            }
                            _ => {}
                        }
                        args.iter()
                            .flat_map(|&arg| self.can_derives(&unifier, arg))
                            .collect_vec()
                    }
                    Term::Tuple(ts) => ts
                        .iter()
                        .flat_map(|&t| self.can_derives(&unifier, t))
                        .collect_vec(),
                    _ => vec![],
                }
            })())
            .fold(Vec::<Unifier>::new(), |mut seen, mut o| {
                if seen.iter_mut().any(|u| u.find(goal) == o.find(goal)) {
                    seen
                } else {
                    seen.push(o);
                    seen
                }
            })
            .into_iter();
    }
    pub fn can_derive(
        &self,
        unifier: &mut Unifier,
        goal: TermId,
        with_unification: WithUnification,
    ) -> bool {
        let mut stack = vec![goal];

        while let Some(term) = stack.pop() {
            // TODO: Cycles can occur in the terms can occur as a result of the
            // unification performed in this step. There must be some way to
            // prevent this by construction, but for now if the stack grows
            // larger than 100 (i.e. more than 100 sub-terms), then assume a
            // cycle has been reached and return false.
            if stack.len() > 100 {
                return false;
            }

            // If we allow unification, just let the variables stay. This might
            // be an abuse for `with_unification`, but in situations where
            // unifications is allowed, it is also best to postpone it, see
            // (lazy-intruder)
            if with_unification == WithUnification::Yes && unifier.probe_value(term).is_variable() {
                continue;
            }

            // Axiom
            if self.iter().any(|t| unifier.are_equal(t, term)) {
                continue;
            }

            if with_unification == WithUnification::Yes {
                self.iter().for_each(|t| {
                    println!(
                        "{:?} {:?} => {:?}",
                        unifier.resolve_full(t),
                        unifier.resolve_full(term),
                        unifier.unify(t, term).is_ok()
                    );
                });
            }

            // TODO: This step can branch depending on which term from the
            // knowledge was unified with. Consider propagating the branching up
            // throughout the search.
            let options: std::collections::HashSet<_> = self.iter().collect();

            options.iter().for_each(|&t| {
                let mut new_unifier = unifier.clone();
                if new_unifier.unify(t, term).is_ok() {
                    println!(
                        "{:?} {:?} => {:?}",
                        unifier.resolve_full(t),
                        unifier.resolve_full(term),
                        unifier.unify(t, term).is_ok()
                    );
                }
            });

            if with_unification == WithUnification::Yes
                && options.into_iter().any(|t| unifier.unify(t, term).is_ok())
            {
                continue;
            }

            // Compose
            match unifier.probe_value(term) {
                Term::Composition(func, args) => {
                    match func {
                        Func::Inv => return false,
                        Func::User(c) => {
                            if !self.iter().any(|f| unifier.are_equal(f, c)) {
                                return false;
                            }
                        }
                        _ => {}
                    }
                    for &a in args {
                        stack.push(a);
                    }
                }
                Term::Tuple(ts) => {
                    for &a in ts {
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
                            let (inner, key) = if let &[inner, key] = args {
                                (inner, key)
                            } else {
                                unreachable!("{func:?} must only contain 2 arguments")
                            };

                            match func {
                                // Symmetric encryption requires the same key as
                                // encrypted with
                                Func::SymEnc
                                    if self.can_derive(unifier, key, WithUnification::No) =>
                                {
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
                                            unifier.are_equal(args[0], key)
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
                            new_terms.extend_from_slice(terms);
                            Move::Remove
                        }
                        Term::Variable(_, _) => Move::Keep,
                        Term::Constant(_, _, _) => Move::Move,
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
    pub fn to_opaque(
        &self,
        unifier: &mut Unifier,
        outstanding_obligations: &mut Vec<Obligation<TermId>>,
        term: TermId,
    ) -> TermId {
        if self.can_derive(unifier, term, WithUnification::No) {
            return term;
        }

        macro_rules! opaque {
            ($term:expr) => {{
                let term = $term;

                let new = unifier.resolve_full(term);
                yansi::Paint::disable();
                let opaque = unifier.register_new_variable(
                    Some(format!(
                        "<|{:?}@{:?}|>",
                        new,
                        std::time::SystemTime::now()
                            .duration_since(std::time::SystemTime::UNIX_EPOCH)
                            .unwrap()
                            .as_nanos()
                            % 100003
                    )),
                    Kind::Other,
                );
                yansi::Paint::enable();

                outstanding_obligations.push(Obligation::ShouldUnify {
                    opaque,
                    actual: term,
                });

                opaque
            }};
        }

        if let Some(o) = outstanding_obligations.iter().copied().find(|&o| match o {
            Obligation::ShouldUnify { opaque, actual } => unifier.are_equal(actual, term),
        }) {
            match o {
                Obligation::ShouldUnify { opaque, actual } => return opaque,
            }
        }

        match unifier.probe_value(term) {
            Term::Intruder => term,
            Term::Variable(_, _) => term,
            Term::Constant(_, _, _) => term,
            Term::Composition(Func::AsymEnc, _) => term,
            Term::Composition(Func::Exp, _) => term,
            Term::Composition(Func::Inv, _) => term,
            Term::Composition(Func::SymEnc, args) => {
                let (inner, key) = (args[0], args[1]);

                if self.can_derive(unifier, key, WithUnification::No) {
                    let opaque_inner = self.to_opaque(unifier, outstanding_obligations, inner);
                    unifier.register_new_composition(Func::SymEnc, vec![opaque_inner, key])
                } else {
                    opaque!(term)
                }
            }
            Term::Composition(Func::User(f), _) => {
                if let Some(o) = outstanding_obligations.iter().copied().find(|&o| match o {
                    Obligation::ShouldUnify { opaque, actual } => unifier.are_equal(actual, term),
                }) {
                    match o {
                        Obligation::ShouldUnify { opaque, actual } => return opaque,
                    }
                }

                let opaque = opaque!(term);

                opaque
            }
            Term::Tuple(ts) => {
                let opaque_ts = ts
                    .iter()
                    .map(|&t| self.to_opaque(unifier, outstanding_obligations, t))
                    .collect_vec();

                if ts.iter().zip(&opaque_ts).any(|(a, b)| a != b) {
                    unifier.register_new_tuple(opaque_ts)
                } else {
                    term
                }
            }
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
                assert_eq!(
                    expected,
                    knowledge.can_derive(&mut unifier, goal, WithUnification::No)
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
