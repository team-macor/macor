use crate::{
    protocol::{Constant, Func, Term},
    typing::{Stage, TypedStage, UntypedStage},
};
use macor_parse::ast;

#[derive(Eq, Debug, Clone, PartialOrd, Ord, Hash, PartialEq)]
pub struct Knowledge<S: Stage = TypedStage>(pub Vec<Term<S>>);

impl Knowledge<TypedStage> {
    pub fn new(mut terms: Vec<Term<TypedStage>>) -> Self {
        terms.sort_unstable();
        terms.dedup();

        let mut k = Knowledge(terms);

        augment_knowledge(&mut k);

        k
    }
    pub fn iter(&self) -> impl Iterator<Item = &Term<TypedStage>> {
        self.0.iter()
    }

    pub fn augment_with(&mut self, term: Term<TypedStage>) {
        self.0.push(term);
        self.0.sort_unstable();
        self.0.dedup();

        augment_knowledge(self);
    }
}

impl FromIterator<Knowledge<TypedStage>> for Knowledge<TypedStage> {
    fn from_iter<T: IntoIterator<Item = Knowledge<TypedStage>>>(iter: T) -> Self {
        Knowledge::new(iter.into_iter().flat_map(|x| x.0).collect())
    }
}
impl FromIterator<Term<TypedStage>> for Knowledge<TypedStage> {
    fn from_iter<T: IntoIterator<Item = Term<TypedStage>>>(iter: T) -> Self {
        Knowledge::new(iter.into_iter().collect())
    }
}

impl<'a> From<&[ast::Term<&'a str>]> for Knowledge<UntypedStage<'a>> {
    fn from(knowledge: &[ast::Term<&'a str>]) -> Self {
        Self(knowledge.iter().cloned().map(Term::from).collect())
    }
}
impl<'a> From<Vec<ast::Term<&'a str>>> for Knowledge<UntypedStage<'a>> {
    fn from(knowledge: Vec<ast::Term<&'a str>>) -> Self {
        Self(knowledge.into_iter().map(Term::from).collect())
    }
}

pub fn can_derive(knowledge: &Knowledge<TypedStage>, goal: &Term<TypedStage>) -> bool {
    let mut stack = vec![goal];
    while let Some(term) = stack.pop() {
        if knowledge.0.contains(term) {
            continue;
        }

        match term {
            Term::Composition { func, args } => {
                if let Func::Inv = func {
                    return false;
                }

                if !func.is_public()
                    && !knowledge
                        .0
                        .contains(&Term::Constant(Constant::Function(func.clone())))
                {
                    return false;
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

pub fn augment_knowledge(knowledge: &mut Knowledge<TypedStage>) {
    let mut new_terms: Vec<Term> = Vec::new();
    loop {
        for term in knowledge.iter() {
            match term {
                Term::Composition {
                    func: Func::SymEnc,
                    args,
                } => {
                    if args.len() != 2 {
                        panic!("arguments for symmetric encrypt function must only contain 2 arguments");
                    }

                    let (term, key) = (&args[0], &args[1]);

                    if can_derive(knowledge, key) {
                        new_terms.push(term.clone());
                    }
                }
                Term::Composition {
                    func: Func::AsymEnc,
                    args,
                } => {
                    if args.len() != 2 {
                        panic!("arguments for asymmetric encrypt function must only contain 2 arguments");
                    }
                    let (term, key) = (&args[0], &args[1]);

                    if let Term::Composition {
                        func: Func::Inv,
                        args: _,
                    } = key
                    {
                        new_terms.push(term.clone());
                    }

                    if can_derive(
                        knowledge,
                        &Term::Composition {
                            func: Func::Inv,
                            args: vec![key.clone()],
                        },
                    ) {
                        new_terms.push(term.clone());
                    }
                }
                Term::Tuple(terms) => {
                    for m in terms {
                        new_terms.push(m.clone());
                    }
                }
                _ => {}
            }
        }

        new_terms.retain(|term| !knowledge.0.contains(term));

        if new_terms.is_empty() {
            knowledge.0.retain(|m| !matches!(m, Term::Tuple(_)));
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
    use crate::typing::{Type, TypingContext, TypingError, UntypedStage};
    use macor_parse::{parse_term, parse_terms};

    fn term(ctx: &mut TypingContext, src: &str) -> Term<TypedStage> {
        let x: Term<UntypedStage> = parse_term(src).unwrap().into();
        x.to_typed(ctx)
    }
    fn knowledge(ctx: &mut TypingContext, src: &str) -> Knowledge<TypedStage> {
        let x: Knowledge<UntypedStage> = parse_terms(src).unwrap().into();
        x.to_typed(ctx)
    }

    fn init_ctx() -> TypingContextBuilder {
        TypingContextBuilder::default()
    }

    #[derive(Default)]
    struct TypingContextBuilder {
        functions: Vec<&'static str>,
        numbers: Vec<&'static str>,
    }

    impl TypingContextBuilder {
        fn function(mut self, func: &'static str) -> Self {
            self.functions.push(func);
            self
        }
        fn functions<const N: usize>(mut self, funcs: [&'static str; N]) -> Self {
            for func in funcs {
                self = self.function(func);
            }
            self
        }
        fn number(mut self, num: &'static str) -> Self {
            self.numbers.push(num);
            self
        }
        fn numbers<const N: usize>(mut self, nums: [&'static str; N]) -> Self {
            for num in nums {
                self.numbers.push(num);
            }
            self
        }

        fn build(self) -> TypingContext {
            TypingContext {
                src: "".into(),
                types: self
                    .functions
                    .into_iter()
                    .map(|func| (func.into(), Type::Function))
                    .chain(
                        self.numbers
                            .into_iter()
                            .map(|num| (num.into(), Type::Number)),
                    )
                    .collect(),
                errors: unsafe {
                    std::mem::transmute([0u8; std::mem::size_of::<Vec<TypingError>>()])
                },
            }
        }
    }

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
        let mut ctx = init_ctx()
            .function("h")
            .functions(["n1", "n2"])
            .functions(["k1", "k2", "k3"])
            .build();

        let goal = term(&mut ctx, "h(k1, k2)");

        let knowledge = knowledge(&mut ctx, "k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3, h"); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        assert!(can_derive(&knowledge, &goal));
    }

    #[test]
    fn non_provable_composition() {
        /*
           init knowledge: {k1, k2, {|<n1, k3>|}h(k1,n2), {|n2|}k3 }
           goal: h(k_1, k_3)
        */
        let mut ctx = init_ctx()
            .function("h")
            .functions(["n1", "n2"])
            .functions(["k1", "k2", "k3"])
            .build();

        let goal = term(&mut ctx, "h(k1, k3)");

        let knowledge = knowledge(&mut ctx, "k1, k2, {|n1, k3|}h(k1,n2), {|n2|}k3, h");
        // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        assert!(!can_derive(&knowledge, &goal));
    }

    #[test]
    fn non_public_function() {
        /*
           init knowledge: {k1, k_2 }
           pub: h
           goal: f(k_1, k_2)
        */
        let mut ctx = init_ctx().function("f").functions(["k1", "k2"]).build();

        let goal = term(&mut ctx, "f(k1, k2)");

        let knowledge = knowledge(&mut ctx, "k1, k2");

        assert!(!can_derive(&knowledge, &goal));
    }

    #[test]
    fn augment_knowledge_slides_example() {
        /*
           init knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           final knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3, <n1, k3>, n1, k3, n2 }
        */
        let mut ctx = init_ctx()
            .function("h")
            .functions(["n1", "n2"])
            .functions(["k1", "k2", "k3"])
            .build();

        let mut q1 = knowledge(&mut ctx, "k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3, h");
        augment_knowledge(&mut q1);

        assert_eq!(
            q1,
            knowledge(
                &mut ctx,
                "k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3, n1, k3, n2, h"
            )
        );

        let mut q2 = knowledge(&mut ctx, "k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3, h");
        augment_knowledge(&mut q2);

        assert_eq!(
            q2,
            knowledge(
                &mut ctx,
                "k1, k2, {|n1, k3|}h(k1, k2), {|n2|}k3, n1, k3, n1, k3, n2, h"
            )
        );
    }
}
