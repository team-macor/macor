use crate::dolev_yao::can_derive;
use crate::protocol::Func;

use ena::unify::{InPlaceUnificationTable, UnifyKey, UnifyValue};
use itertools::Itertools;
use smol_str::SmolStr;
use yansi::Paint;

#[derive(Clone, Default)]
pub struct Hint(Option<SmolStr>);

impl Hint {
    pub fn none() -> Self {
        Hint(None)
    }
    pub fn unwrap(self) -> SmolStr {
        self.0.unwrap()
    }
    fn join(&self, other: &Self) -> Self {
        match (&self.0, &other.0) {
            (None, None) => Hint(None),
            (None, x @ Some(_)) | (x @ Some(_), None) | (x @ Some(_), Some(_)) => Hint(x.clone()),
        }
    }
}

impl<S> From<Option<S>> for Hint
where
    S: Into<SmolStr>,
{
    fn from(x: Option<S>) -> Self {
        Hint(x.map(|s| s.into()))
    }
}

impl std::ops::Deref for Hint {
    type Target = Option<SmolStr>;

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

#[derive(Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct ConstantId(u32, pub Hint);

impl ConstantId {
    pub fn new(i: u32) -> Self {
        ConstantId(i, Hint::default())
    }
    pub fn hint(&self, hint: impl Into<SmolStr>) -> Self {
        ConstantId(self.0, Hint(Some(hint.into())))
    }
}

impl std::fmt::Debug for ConstantId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.1.as_deref() {
            write!(f, "!{}[{}]", name, self.0)
        } else {
            write!(f, "![{}]", self.0)
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct TermId(u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Kind {
    Agent,
    Other,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term<M> {
    Intruder,
    Variable(Hint, Kind),
    Constant(ConstantId, Kind),
    Composition(Func<M>, Vec<M>),
    Tuple(Vec<M>),
}
use self::Term::*;

impl<M> Term<M> {
    pub fn kind(&self) -> Kind {
        match self {
            Intruder => Kind::Agent,
            Variable(_, kind) | Constant(_, kind) => *kind,
            Composition(_, _) | Tuple(_) => Kind::Other,
        }
    }
    pub fn map<T>(&self, mut f: impl FnMut(&M) -> T) -> Term<T> {
        match self {
            Intruder => Intruder,
            Variable(hint, kind) => Variable(hint.clone(), *kind),
            Constant(c, kind) => Constant(c.clone(), *kind),
            Composition(n, args) => Composition(n.map(|x| f(x)), args.iter().map(f).collect()),
            Tuple(ts) => Tuple(ts.iter().map(f).collect()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FullTerm(pub Term<Box<FullTerm>>);

impl<M: std::fmt::Debug> std::fmt::Debug for Term<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Intruder => write!(f, "{}", Paint::magenta("i").bold()),
            Self::Variable(arg0, Kind::Agent) => {
                if let Some(k) = arg0.as_deref() {
                    write!(f, "{}", Paint::cyan(k).underline())
                } else {
                    write!(f, "{}", Paint::cyan("Agent").underline())
                }
            }
            Self::Variable(arg0, Kind::Other) => {
                if let Some(k) = arg0.as_deref() {
                    write!(f, "{}", Paint::cyan(k))
                } else {
                    write!(f, "{}", Paint::cyan("Other"))
                }
            }
            Self::Constant(c, kind) => match kind {
                Kind::Agent => Paint::red(c).underline().fmt(f),
                Kind::Other => Paint::red(c).fmt(f),
            },
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
    type Error = ();

    fn unify_values(l: &Self, r: &Self) -> Result<Self, Self::Error> {
        Ok(match (l, r) {
            (Variable(l, lk), Variable(r, rk)) => {
                if lk == rk {
                    Variable(l.join(r), *lk)
                } else {
                    return Err(());
                }
            }
            (Variable(_, vk), Constant(c, ck)) | (Constant(c, ck), Variable(_, vk)) => {
                if ck == vk {
                    Constant(c.clone(), *ck)
                } else {
                    return Err(());
                }
            }
            (Variable(_, Kind::Other), Composition(func, args))
            | (Composition(func, args), Variable(_, Kind::Other)) => {
                Composition(func.clone(), args.clone())
            }
            (Variable(_, Kind::Other), Tuple(ts)) | (Tuple(ts), Variable(_, Kind::Other)) => {
                Tuple(ts.clone())
            }
            (Constant(l, lk), Constant(r, rk)) => {
                if l == r && lk == rk {
                    Constant(l.clone(), *lk)
                } else {
                    return Err(());
                }
            }
            (Composition(l_func, l_args), Composition(r_func, r_args)) => {
                if l_func != r_func && l_args.len() == r_args.len() {
                    return Err(());
                } else {
                    Composition(l_func.clone(), l_args.clone())
                }
            }
            (Tuple(ls), Tuple(rs)) => {
                if ls.len() != rs.len() {
                    return Err(());
                } else {
                    Tuple(ls.clone())
                }
            }
            _ => return Err(()),
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

impl Unifier {
    pub fn intruder(&self) -> TermId {
        self.intruder
    }
    pub fn register_new_constant(&mut self, hint: impl Into<Hint>, kind: Kind) -> TermId {
        let cid = ConstantId(self.next_constant, hint.into());
        self.next_constant += 1;
        self.table.new_key(Constant(cid, kind))
    }
    pub fn register_new_variable(&mut self, hint: impl Into<Hint>, kind: Kind) -> TermId {
        self.table.new_key(Variable(hint.into(), kind))
    }
    pub fn register_term(&mut self, term: Term<TermId>) -> TermId {
        match term {
            Intruder => panic!("Intruders should not be registered. Use `Unifier::intruder` instead"),
            Variable(_, _) => panic!("Variables should not be registered explicitly. Use `Unifier::register_new_variable`"),
            Constant(_, _) => panic!("Constants should not be registered explicitly. Use `Unifier::register_new_constant`"),
            term@Composition(_, _) |
            term@Tuple(_) => self.table.new_key(term),
        }
    }
    /// Recursively unifies the two terms and returns either of the passed
    /// terms if they indeed do unify (since they are now equivalent), or
    /// else produces and error.
    pub fn unify(&mut self, l: TermId, r: TermId) -> Result<TermId, ()> {
        let snap = self.table.snapshot();
        match self.unify_inner(l, r) {
            Ok(v) => {
                self.table.commit(snap);
                Ok(v)
            }
            Err(()) => {
                self.table.rollback_to(snap);
                Err(())
            }
        }
    }
    fn unify_inner(&mut self, l: TermId, r: TermId) -> Result<TermId, ()> {
        Ok(
            match (self.table.probe_value(l), self.table.probe_value(r)) {
                (x @ Variable(_, _), y) | (y, x @ Variable(_, _)) if x.kind() == y.kind() => {
                    self.table.unify_var_var(l, r)?;
                    l
                }
                (Constant(x, xk), Constant(y, yk)) => {
                    if x == y && xk == yk {
                        l
                    } else {
                        return Err(());
                    }
                }
                (Composition(l_func, l_args), Composition(r_func, r_args)) => {
                    if l_func != r_func || l_args.len() != r_args.len() {
                        // eprintln!(
                        //     "Attempted to unify {:?} with {:?}",
                        //     self.resolve_full(l),
                        //     self.resolve_full(r)
                        // );
                        return Err(());
                    } else {
                        for (l_arg, r_arg) in l_args.into_iter().zip_eq(r_args) {
                            self.unify_inner(l_arg, r_arg)?;
                        }
                        self.table.unify_var_var(l, r)?;
                        l
                    }
                }
                (Tuple(ls), Tuple(rs)) => {
                    if ls.len() != rs.len() {
                        return Err(());
                    } else {
                        for (l_arg, r_arg) in ls.into_iter().zip_eq(rs) {
                            self.unify_inner(l_arg, r_arg)?;
                        }
                        self.table.unify_var_var(l, r)?;
                        l
                    }
                }
                _ => return Err(()),
            },
        )
    }
    pub fn are_unified(&mut self, a: TermId, b: TermId) -> bool {
        if self.table.unioned(a, b) {
            return true;
        }

        match (self.probe_value(a), self.probe_value(b)) {
            (Composition(f, xs), Composition(g, ys)) => {
                xs.len() == ys.len()
                    && match (f, g) {
                        (Func::User(f), Func::User(g)) => self.are_unified(f, g),
                        (f, g) => f == g,
                    }
                    && xs
                        .iter()
                        .zip_eq(ys.iter())
                        .all(|(&x, &y)| self.are_unified(x, y))
            }
            (Tuple(xs), Tuple(ys)) => {
                xs.len() == ys.len()
                    && xs
                        .iter()
                        .zip_eq(ys.iter())
                        .all(|(&x, &y)| self.are_unified(x, y))
            }
            (x, y) => x == y,
        }
    }
    // pub fn try_unify(&mut self, a: TermId, b: TermId) -> Result<(), ()> {
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

    pub fn probe_value(&mut self, id: TermId) -> Term<TermId> {
        self.table.probe_value(id)
    }

    pub fn resolve_full(&mut self, id: TermId) -> FullTerm {
        FullTerm(
            self.table
                .probe_value(id)
                .map(|&id| box self.resolve_full(id)),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Unifier {
        fn constant(&mut self, i: u32) -> TermId {
            self.table
                .new_key(Constant(ConstantId(i, Hint::none()), Kind::Other))
        }
        fn variable(&mut self) -> TermId {
            self.table.new_key(Variable(Hint::none(), Kind::Other))
        }
    }

    #[test]
    fn very_basic_unification() -> Result<(), ()> {
        let mut unifier = Unifier::default();

        let a = unifier.constant(0);
        let b = unifier.variable();

        unifier.unify(a, b)?;

        assert_eq!(unifier.table.probe_value(a), unifier.table.probe_value(b));
        assert_eq!(
            unifier.table.probe_value(b),
            Constant(ConstantId(0, Hint::none()), Kind::Other)
        );

        Ok(())
    }

    #[test]
    fn less_basic_unification() -> Result<(), ()> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(0);
        let y = unifier.constant(1);

        let y_free = unifier.variable();
        let x_free = unifier.variable();

        let a = unifier.table.new_key(Tuple(vec![x, y_free]));
        let b = unifier.table.new_key(Tuple(vec![x_free, y]));

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Tuple(vec![
                box FullTerm(Constant(ConstantId(0, Hint::none()), Kind::Other)),
                box FullTerm(Constant(ConstantId(1, Hint::none()), Kind::Other))
            ]))
        );

        Ok(())
    }

    #[test]
    fn unify_simple_composition() -> Result<(), ()> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(0);
        let y = unifier.constant(1);

        let x_free = unifier.variable();
        let y_free = unifier.variable();

        let a = unifier
            .table
            .new_key(Composition(Func::Exp, vec![x, y_free]));
        let b = unifier
            .table
            .new_key(Composition(Func::Exp, vec![x_free, y]));

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Composition(
                Func::Exp,
                vec![
                    box FullTerm(Constant(ConstantId(0, Hint::none()), Kind::Other)),
                    box FullTerm(Constant(ConstantId(1, Hint::none()), Kind::Other))
                ]
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

        let a = unifier.table.new_key(Tuple(vec![y, free]));
        let b = unifier.table.new_key(Tuple(vec![x, y]));

        assert_eq!(unifier.unify(a, b), Err(()));
    }

    #[test]
    fn branching_unification() -> Result<(), ()> {
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
            FullTerm(Constant(ConstantId(1, Hint::none()), Kind::Other))
        );
        assert_eq!(
            world_2.resolve_full(a),
            FullTerm(Constant(ConstantId(2, Hint::none()), Kind::Other))
        );

        Ok(())
    }

    #[test]
    fn rollback_unification() -> Result<(), ()> {
        let mut unifier = Unifier::default();

        let x = unifier.constant(1);
        let y = unifier.constant(2);

        let a = unifier.variable();

        let snap = unifier.table.snapshot();

        unifier.unify(a, x)?;

        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Constant(ConstantId(1, Hint::none()), Kind::Other))
        );

        unifier.table.rollback_to(snap);

        unifier.unify(a, y)?;

        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Constant(ConstantId(2, Hint::none()), Kind::Other))
        );

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

#[derive(Debug, Clone, Default)]
pub struct Knowledge(pub Vec<TermId>);

impl Knowledge {
    pub fn resoled(&self, unifier: &mut Unifier) -> Vec<FullTerm> {
        self.0.iter().map(|&id| unifier.resolve_full(id)).collect()
    }
}

impl Knowledge {
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
