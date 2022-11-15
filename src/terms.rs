use crate::protocol::Func;

use ena::unify::{InPlaceUnificationTable, UnifyKey, UnifyValue};
use itertools::Itertools;
use yansi::Paint;

#[derive(Clone, Copy, Default)]
pub struct Hint(Option<&'static str>);

impl Hint {
    pub fn none() -> Self {
        Hint(None)
    }
    pub fn unwrap(self) -> &'static str {
        self.0.unwrap()
    }
    fn join(self, other: Self) -> Self {
        match (self.0, other.0) {
            (None, None) => Hint(None),
            (None, x @ Some(_)) | (x @ Some(_), None) | (x @ Some(_), Some(_)) => Hint(x),
        }
    }
}

impl<S> From<Option<S>> for Hint
where
    String: From<S>,
{
    fn from(x: Option<S>) -> Self {
        use once_cell::sync::Lazy;

        static HINT_INTERNER: Lazy<std::sync::Mutex<indexmap::IndexMap<String, &'static str>>> =
            Lazy::new(Default::default);

        Hint(x.map(|s| -> &'static str {
            HINT_INTERNER
                .lock()
                .unwrap()
                .entry(String::from(s))
                .or_insert_with_key(|s| Box::leak(s.clone().into_boxed_str()))
        }))
    }
}

impl std::ops::Deref for Hint {
    type Target = Option<&'static str>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::hash::Hash for Hint {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}
impl Ord for Hint {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl PartialOrd for Hint {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}
impl Eq for Hint {}
impl PartialEq for Hint {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct ConstantId(u32);

impl ConstantId {
    pub fn new(i: u32) -> Self {
        ConstantId(i)
    }
}

impl std::fmt::Debug for ConstantId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct TermId(u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Kind {
    Agent,
    Other,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term<M: 'static> {
    Intruder,
    Variable(Hint, Kind),
    Constant(ConstantId, Hint, Kind),
    Composition(Func<M>, &'static [M]),
    Tuple(&'static [M]),
}
use self::Term::*;

fn leak_slice<T>(iter: impl IntoIterator<Item = T>) -> &'static [T] {
    Box::leak(iter.into_iter().collect_vec().into_boxed_slice())
}

impl<M> Term<M> {
    pub fn kind(&self) -> Kind {
        match self {
            Intruder => Kind::Agent,
            Variable(_, kind) | Constant(_, _, kind) => *kind,
            Composition(_, _) | Tuple(_) => Kind::Other,
        }
    }
    pub fn map<T>(&self, mut f: impl FnMut(&M) -> T) -> Term<T> {
        match self {
            Intruder => Intruder,
            Variable(hint, kind) => Variable(*hint, *kind),
            Constant(c, hint, kind) => Constant(*c, *hint, *kind),
            Composition(n, args) => Composition(n.map(|x| f(x)), leak_slice(args.iter().map(f))),
            Tuple(ts) => Tuple(leak_slice(ts.iter().map(f))),
        }
    }

    pub fn is_inv(&self) -> bool {
        matches!(self, Composition(Func::Inv, _))
    }

    /// Returns `true` if the term is [`Variable`].
    ///
    /// [`Variable`]: Term::Variable
    #[must_use]
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable(..))
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FullTerm(pub Term<Box<FullTerm>>);

impl<M: std::fmt::Debug> std::fmt::Debug for Term<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Intruder => write!(f, "{}", Paint::magenta("i").bold()),
            Self::Variable(hint, Kind::Agent) => {
                if let Some(k) = hint.as_deref() {
                    write!(f, "{}", Paint::cyan(k).underline())
                } else {
                    write!(f, "{}", Paint::cyan("Agent").underline())
                }
            }
            Self::Variable(hint, Kind::Other) => {
                if let Some(k) = hint.as_deref() {
                    if k.starts_with("<|") {
                        write!(
                            f,
                            "{}",
                            k.chars().map(|c| Paint::cyan(c).strikethrough()).format("")
                        )
                    } else {
                        write!(f, "{}", Paint::cyan(k))
                    }
                } else {
                    write!(f, "{}", Paint::cyan("Other"))
                }
            }
            Self::Constant(_, hint, Kind::Agent) => {
                if let Some(k) = hint.as_deref() {
                    write!(f, "!{}", Paint::red(k).underline())
                } else {
                    write!(f, "!{}", Paint::red("Agent").underline())
                }
            }
            Self::Constant(_, hint, Kind::Other) => {
                if let Some(k) = hint.as_deref() {
                    write!(f, "!{}", Paint::red(k))
                } else {
                    write!(f, "!{}", Paint::red("Other"))
                }
            }
            Self::Composition(Func::AsymEnc, args) => {
                let (term, key) = (&args[0], &args[1]);
                write!(
                    f,
                    "{l} {term:?} {r}\x1b[00m{key:?}",
                    l = Paint::magenta("{"),
                    r = Paint::magenta("}"),
                )
            }
            Self::Composition(Func::SymEnc, args) => {
                let (term, key) = (&args[0], &args[1]);
                write!(
                    f,
                    "{l} {term:?} {r}\x1b[00m{key:?}",
                    l = Paint::yellow("{|"),
                    r = Paint::yellow("|}"),
                )
            }
            Self::Composition(fun, args) => {
                if let Func::User(x) = fun {
                    write!(
                        f,
                        "{:?}{l}{:?}{r}",
                        Paint::red(x).italic(),
                        args.iter().format(", "),
                        l = Paint::red("(").dimmed().italic(),
                        r = Paint::red(")").dimmed().italic(),
                    )
                } else {
                    write!(f, "{:?}({:?})", fun, args.iter().format(", "))
                }
            }
            Self::Tuple(arg0) => write!(f, ":({:?})", arg0.iter().format(", ")),
        }
    }
}

impl std::fmt::Debug for FullTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl UnifyValue for Term<TermId> {
    type Error = UnificationError;

    fn unify_values(&l: &Self, &r: &Self) -> Result<Self, Self::Error> {
        Ok(match (l, r) {
            (Variable(l, lk), Variable(r, rk)) => {
                if lk == rk {
                    Variable(l.join(r), lk)
                } else {
                    return Err(UnificationError::DidNotUnify);
                }
            }
            (Variable(_, vk), Constant(c, h, ck)) | (Constant(c, h, ck), Variable(_, vk)) => {
                if ck == vk {
                    Constant(c, h, ck)
                } else {
                    return Err(UnificationError::DidNotUnify);
                }
            }
            (Variable(_, Kind::Agent), Intruder) | (Intruder, Variable(_, Kind::Agent)) => Intruder,
            (Variable(_, Kind::Other), Composition(func, args))
            | (Composition(func, args), Variable(_, Kind::Other)) => Composition(func, args),
            (Variable(_, Kind::Other), Tuple(ts)) | (Tuple(ts), Variable(_, Kind::Other)) => {
                Tuple(ts)
            }
            (Constant(l, lh, lk), Constant(r, rh, rk)) => {
                if l == r && lk == rk {
                    Constant(l, lh.join(rh), lk)
                } else {
                    return Err(UnificationError::DidNotUnify);
                }
            }
            (Composition(l_func, l_args), Composition(r_func, r_args)) => {
                if l_func != r_func && l_args.len() == r_args.len() {
                    return Err(UnificationError::DidNotUnify);
                } else {
                    Composition(l_func, l_args)
                }
            }
            (Tuple(ls), Tuple(rs)) => {
                if ls.len() != rs.len() {
                    return Err(UnificationError::DidNotUnify);
                } else {
                    Tuple(ls)
                }
            }
            _ => return Err(UnificationError::DidNotUnify),
        })
    }
}

impl UnifyKey for TermId {
    type Value = Term<TermId>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        TermId(u)
    }

    fn tag() -> &'static str {
        "TermId"
    }
}

#[derive(Debug, Clone)]
pub struct Unifier {
    next_constant: u32,
    intruder: TermId,
    table: InPlaceUnificationTable<TermId>,
}

impl Default for Unifier {
    fn default() -> Self {
        let mut table = InPlaceUnificationTable::default();
        let intruder = table.new_key(Intruder);
        Self {
            next_constant: 1,
            intruder,
            table,
        }
    }
}

#[derive(thiserror::Error, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnificationError {
    #[error("the terms did not unify")]
    DidNotUnify,
}

impl Unifier {
    pub fn intruder(&self) -> TermId {
        self.intruder
    }
    pub fn register_new_constant(&mut self, hint: impl Into<Hint>, kind: Kind) -> TermId {
        let cid = ConstantId(self.next_constant);
        self.next_constant += 1;
        self.table.new_key(Constant(cid, hint.into(), kind))
    }
    pub fn register_new_variable(&mut self, hint: impl Into<Hint>, kind: Kind) -> TermId {
        self.table.new_key(Variable(hint.into(), kind))
    }
    pub fn register_new_composition(&mut self, func: Func<TermId>, args: Vec<TermId>) -> TermId {
        self.table.new_key(Composition(func, leak_slice(args)))
    }
    pub fn register_new_tuple(&mut self, terms: Vec<TermId>) -> TermId {
        self.table.new_key(Tuple(leak_slice(terms)))
    }
    /// Recursively unifies the two terms and returns either of the passed
    /// terms if they indeed do unify (since they are now equivalent), or
    /// else produces and error.
    #[inline(always)]
    pub fn unify(&mut self, l: TermId, r: TermId) -> Result<TermId, UnificationError> {
        let snap = self.table.snapshot();
        match self.unify_inner(l, r) {
            Ok(v) => {
                self.table.commit(snap);
                Ok(v)
            }
            Err(e) => {
                self.table.rollback_to(snap);
                Err(e)
            }
        }
    }
    fn unify_inner(&mut self, l: TermId, r: TermId) -> Result<TermId, UnificationError> {
        Ok(
            match (self.table.probe_value(l), self.table.probe_value(r)) {
                (x @ Variable(_, _), y) | (y, x @ Variable(_, _)) if x.kind() == y.kind() => {
                    self.table.unify_var_var(l, r)?;
                    l
                }
                (Intruder, Variable(_, Kind::Agent)) | (Variable(_, Kind::Agent), Intruder) => {
                    self.table.unify_var_var(l, r)?;
                    l
                }
                (Constant(x, _, xk), Constant(y, _, yk)) => {
                    if x == y && xk == yk {
                        l
                    } else {
                        return Err(UnificationError::DidNotUnify);
                    }
                }
                (Composition(l_func, l_args), Composition(r_func, r_args)) => {
                    if l_func != r_func || l_args.len() != r_args.len() {
                        // eprintln!(
                        //     "Attempted to unify {:?} with {:?}",
                        //     self.resolve_full(l),
                        //     self.resolve_full(r)
                        // );
                        return Err(UnificationError::DidNotUnify);
                    } else {
                        for (&l_arg, &r_arg) in l_args.iter().zip_eq(r_args) {
                            self.unify_inner(l_arg, r_arg)?;
                        }
                        self.table.unify_var_var(l, r)?;
                        l
                    }
                }
                (Tuple(ls), Tuple(rs)) => {
                    if ls.len() != rs.len() {
                        return Err(UnificationError::DidNotUnify);
                    } else {
                        for (&l_arg, &r_arg) in ls.iter().zip_eq(rs) {
                            self.unify_inner(l_arg, r_arg)?;
                        }
                        self.table.unify_var_var(l, r)?;
                        l
                    }
                }
                _ => return Err(UnificationError::DidNotUnify),
            },
        )
    }
    pub fn are_equal(&mut self, a: TermId, b: TermId) -> bool {
        if a == b || self.table.unioned(a, b) {
            return true;
        }

        match (self.probe_value(a), self.probe_value(b)) {
            (Composition(f, xs), Composition(g, ys)) => {
                xs.len() == ys.len()
                    && match (f, g) {
                        (Func::User(f), Func::User(g)) => self.are_equal(f, g),
                        (f, g) => f == g,
                    }
                    && xs
                        .iter()
                        .zip_eq(ys.iter())
                        .all(|(&x, &y)| self.are_equal(x, y))
            }
            (Tuple(xs), Tuple(ys)) => {
                xs.len() == ys.len()
                    && xs
                        .iter()
                        .zip_eq(ys.iter())
                        .all(|(&x, &y)| self.are_equal(x, y))
            }
            (Variable(_, _), Variable(_, _)) => false,
            (x, y) => x == y,
        }
    }
    // pub fn try_unify(&mut self, a: TermId, b: TermId) -> Result<(), UnificationError> {
    //     // TODO: Should this be recursive?
    //     if self.table.unioned(a, b) {
    //         return Ok(());
    //     }

    //     match (self.probe_value(a), self.probe_value(b)) {
    //         (Agent(_))
    //         (a, b) => todo!("{:?} {:?}", a, b),
    //     }

    //     // println!(
    //     //     "Are unified: {:?} with {:?}",
    //     //     self.resolve_full(a),
    //     //     self.resolve_full(b)
    //     // );

    //     false
    // }

    pub fn find(&mut self, id: TermId) -> TermId {
        self.table.find(id)
    }

    pub fn probe_value(&mut self, id: TermId) -> Term<TermId> {
        self.table.inlined_probe_value(id)
    }

    pub fn resolve_full(&mut self, id: TermId) -> FullTerm {
        FullTerm(self.probe_value(id).map(|&id| box self.resolve_full(id)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Unifier {
        fn constant(&mut self, i: u32) -> TermId {
            self.table
                .new_key(Constant(ConstantId(i), Hint::none(), Kind::Other))
        }
        fn variable(&mut self) -> TermId {
            self.table.new_key(Variable(Hint::none(), Kind::Other))
        }
        fn constant_agent(&mut self, i: u32) -> TermId {
            self.table
                .new_key(Constant(ConstantId(i), Hint::none(), Kind::Agent))
        }
        fn variable_agent(&mut self) -> TermId {
            self.table.new_key(Variable(Hint::none(), Kind::Agent))
        }
    }

    #[test]
    fn very_basic_unification() -> Result<(), UnificationError> {
        let mut unifier = Unifier::default();

        let a = unifier.constant(0);
        let b = unifier.variable();

        unifier.unify(a, b)?;

        assert_eq!(unifier.table.probe_value(a), unifier.table.probe_value(b));
        assert_eq!(
            unifier.table.probe_value(b),
            Constant(ConstantId(0), Hint::none(), Kind::Other)
        );

        Ok(())
    }

    #[test]
    fn less_basic_unification() -> Result<(), UnificationError> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(0);
        let y = unifier.constant(1);

        let y_free = unifier.variable();
        let x_free = unifier.variable();

        let a = unifier.register_new_tuple(vec![x, y_free]);
        let b = unifier.register_new_tuple(vec![x_free, y]);

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Tuple(leak_slice([
                box FullTerm(Constant(ConstantId(0), Hint::none(), Kind::Other)),
                box FullTerm(Constant(ConstantId(1), Hint::none(), Kind::Other))
            ])))
        );

        Ok(())
    }

    #[test]
    fn unify_simple_composition() -> Result<(), UnificationError> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(0);
        let y = unifier.constant(1);

        let x_free = unifier.variable();
        let y_free = unifier.variable();

        let a = unifier.register_new_composition(Func::Exp, vec![x, y_free]);
        let b = unifier.register_new_composition(Func::Exp, vec![x_free, y]);

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Composition(
                Func::Exp,
                leak_slice([
                    box FullTerm(Constant(ConstantId(0), Hint::none(), Kind::Other)),
                    box FullTerm(Constant(ConstantId(1), Hint::none(), Kind::Other))
                ])
            ))
        );

        Ok(())
    }

    #[test]
    fn non_unification() {
        let mut unifier = Unifier::default();

        let x = unifier.constant(0);
        let y = unifier.constant(1);

        let free = unifier.variable();

        let a = unifier.register_new_tuple(vec![y, free]);
        let b = unifier.register_new_tuple(vec![x, y]);

        assert_eq!(unifier.unify(a, b), Err(UnificationError::DidNotUnify));
    }

    #[test]
    fn branching_unification() -> Result<(), UnificationError> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(1);
        let y = unifier.constant(2);

        let a = unifier.variable();

        let mut world_1 = unifier.clone();
        let mut world_2 = unifier.clone();

        world_1.unify(a, x)?;
        world_2.unify(a, y)?;

        assert_eq!(
            world_1.resolve_full(a),
            FullTerm(Constant(ConstantId(1), Hint::none(), Kind::Other))
        );
        assert_eq!(
            world_2.resolve_full(a),
            FullTerm(Constant(ConstantId(2), Hint::none(), Kind::Other))
        );

        Ok(())
    }

    #[test]
    fn rollback_unification() -> Result<(), UnificationError> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(1);
        let y = unifier.constant(2);

        let a = unifier.variable();

        let snap = unifier.table.snapshot();

        unifier.unify(a, x)?;

        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Constant(ConstantId(1), Hint::none(), Kind::Other))
        );

        unifier.table.rollback_to(snap);

        unifier.unify(a, y)?;

        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Constant(ConstantId(2), Hint::none(), Kind::Other))
        );

        Ok(())
    }

    #[test]
    fn intruder_agent_in_composition() -> Result<(), UnificationError> {
        let mut unifier = Unifier::default();

        let i = unifier.intruder();
        let a = unifier.variable_agent();
        let sk = unifier.constant(0);
        let sk_a = unifier.register_new_composition(Func::User(sk), vec![a]);
        let sk_i = unifier.register_new_composition(Func::User(sk), vec![i]);

        unifier.unify(sk_a, sk_i)?;

        Ok(())
    }
}

// Numbers: NA
//
// A->s: NA
//              A fixes the value of NA to be whatever
//                  .... s does not know what NA is yet ....
//              s receives its first occurrence of NA, and fixes it to that value
// s->B: NA
//              s sends its fixed value of NA to B
//              B fixes the value to what ever it was told
// B->A: NA
//              B sends the fixed value of NA to A
//              A already knows the value of NA, and thus can check its validity
//
// A->s: n1
//              s needs to unify the expected term with the received:
//              unify(n1, NA)

// A->: NA_a (1)
// ->A: NA_a (3)

// ->s: NA_s (1)
// s->: NA_s (2)

// ->B: NA_b (2)
// B->: NA_b (3)
