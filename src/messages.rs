use crate::dolev_yao::can_derive;
use crate::protocol::{self, AgentName, Direction, Func, Protocol, SessionId};

use ena::unify::{InPlaceUnificationTable, UnifyKey, UnifyValue};
use indexmap::IndexMap;
use itertools::Itertools;
use macor_parse::ast::Ident;
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

impl<M> Term<M> {
    pub fn kind(&self) -> Kind {
        match self {
            Term::Intruder => Kind::Agent,
            Term::Variable(_, kind) | Term::Constant(_, kind) => *kind,
            Term::Composition(_, _) | Term::Tuple(_) => Kind::Other,
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
        use self::Term::*;

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
    intruder: TermId,
    table: InPlaceUnificationTable<TermId>,
}

impl Default for Unifier {
    fn default() -> Self {
        let mut table = InPlaceUnificationTable::default();
        let intruder = table.new_key(Term::Intruder);
        Self { intruder, table }
    }
}

impl Unifier {
    pub fn intruder(&self) -> TermId {
        self.intruder
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
        use self::Term::*;

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
        use self::Term::*;

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

    //     use self::Term::*;

    //     match (self.probe_value(a), self.probe_value(b)) {
    //         (Term::Agent(_))
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
        let term = match self.table.probe_value(id) {
            Term::Intruder => Term::Intruder,
            Term::Variable(v, kind) => Term::Variable(v, kind),
            Term::Constant(c, kind) => Term::Constant(c, kind),
            Term::Composition(func, args) => Term::Composition(
                func.map(|&id| box self.resolve_full(id)),
                args.into_iter()
                    .map(|arg| box self.resolve_full(arg))
                    .collect(),
            ),
            Term::Tuple(ts) => {
                Term::Tuple(ts.into_iter().map(|t| box self.resolve_full(t)).collect())
            }
        };
        FullTerm(term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Unifier {
        fn constant(&mut self, i: u32) -> TermId {
            self.table
                .new_key(Term::Constant(ConstantId(i, Hint::none()), Kind::Other))
        }
        fn variable(&mut self) -> TermId {
            self.table
                .new_key(Term::Variable(Hint::none(), Kind::Other))
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
            Term::Constant(ConstantId(0, Hint::none()), Kind::Other)
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

        let a = unifier.table.new_key(Term::Tuple(vec![x, y_free]));
        let b = unifier.table.new_key(Term::Tuple(vec![x_free, y]));

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Term::Tuple(vec![
                box FullTerm(Term::Constant(ConstantId(0, Hint::none()), Kind::Other)),
                box FullTerm(Term::Constant(ConstantId(1, Hint::none()), Kind::Other))
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
            .new_key(Term::Composition(Func::Exp, vec![x, y_free]));
        let b = unifier
            .table
            .new_key(Term::Composition(Func::Exp, vec![x_free, y]));

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Term::Composition(
                Func::Exp,
                vec![
                    box FullTerm(Term::Constant(ConstantId(0, Hint::none()), Kind::Other)),
                    box FullTerm(Term::Constant(ConstantId(1, Hint::none()), Kind::Other))
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

        let a = unifier.table.new_key(Term::Tuple(vec![y, free]));
        let b = unifier.table.new_key(Term::Tuple(vec![x, y]));

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
            FullTerm(Term::Constant(ConstantId(1, Hint::none()), Kind::Other))
        );
        assert_eq!(
            world_2.resolve_full(a),
            FullTerm(Term::Constant(ConstantId(2, Hint::none()), Kind::Other))
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
            FullTerm(Term::Constant(ConstantId(1, Hint::none()), Kind::Other))
        );

        unifier.table.rollback_to(snap);

        unifier.unify(a, y)?;

        assert_eq!(
            unifier.resolve_full(a),
            FullTerm(Term::Constant(ConstantId(2, Hint::none()), Kind::Other))
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ForWho {
    Intruder(SessionId),
    Agent(SessionId, protocol::AgentName),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct VariableKey {
    for_who: ForWho,
    variable: SmolStr,
}

#[derive(Debug)]
pub struct Converter<'a> {
    pub unifier: &'a mut Unifier,
    pub mappings: &'a mut Mappings,
}

#[derive(Debug, Clone)]
pub struct Mappings {
    next_constant: u32,
    global_agent_table: IndexMap<AgentName, TermId>,
    agent_table: IndexMap<(ForWho, SmallStr), TermId>,
    func_table: IndexMap<Func, TermId>,
    global_constant_table: IndexMap<SmallStr, TermId>,
    constant_table: IndexMap<VariableKey, TermId>,
    variable_table: IndexMap<VariableKey, TermId>,
}

impl Default for Mappings {
    fn default() -> Self {
        Self {
            next_constant: 1,
            global_agent_table: Default::default(),
            agent_table: Default::default(),
            func_table: Default::default(),
            global_constant_table: Default::default(),
            constant_table: Default::default(),
            variable_table: Default::default(),
        }
    }
}

impl<'a> Converter<'a> {
    pub fn new(unifier: &'a mut Unifier, mappings: &'a mut Mappings) -> Self {
        Self { unifier, mappings }
    }

    pub fn get_agent(&mut self, ctx: &ForWho, agent_to_get: &protocol::AgentName) -> TermId {
        if agent_to_get.0.is_constant() {
            return *self
                .mappings
                .global_agent_table
                .entry(agent_to_get.clone())
                .or_insert_with(|| {
                    let cid =
                        ConstantId::new(self.mappings.next_constant).hint(agent_to_get.0.clone());
                    self.mappings.next_constant += 1;
                    self.unifier.table.new_key(Term::Constant(cid, Kind::Agent))
                });
        }

        const AGENTS_FIX_TO_THEMSELVES: bool = false;

        match ctx {
            ForWho::Intruder(session_id) => self.get_agent(
                &ForWho::Agent(*session_id, agent_to_get.clone()),
                agent_to_get,
            ),
            ForWho::Agent(session_id, agent) => *self
                .mappings
                .agent_table
                .entry((ctx.clone(), agent_to_get.0.clone().into()))
                .or_insert_with(|| {
                    if agent_to_get.0.is_constant() {
                        let cid = ConstantId::new(self.mappings.next_constant)
                            .hint(agent_to_get.0.clone());
                        self.mappings.next_constant += 1;
                        self.unifier.table.new_key(Term::Constant(cid, Kind::Agent))
                    } else if agent_to_get == agent && AGENTS_FIX_TO_THEMSELVES {
                        let cid = ConstantId::new(self.mappings.next_constant).hint(format!(
                            "{:?}_{:?}",
                            agent_to_get.0.clone(),
                            session_id.0
                        ));
                        self.mappings.next_constant += 1;
                        self.unifier.table.new_key(Term::Constant(cid, Kind::Agent))
                    } else {
                        self.unifier.table.new_key(Term::Variable(
                            Some(format!("{}@{}:{}", agent.0, session_id.0, agent_to_get.0)).into(),
                            Kind::Agent,
                        ))
                    }
                }),
        }
    }
    pub fn get_function_constant(&mut self, func: Func) -> TermId {
        match func {
            Func::SymEnc | Func::AsymEnc | Func::Exp | Func::Inv => *self
                .mappings
                .func_table
                .entry(func.clone())
                .or_insert_with(|| {
                    let cid =
                        ConstantId::new(self.mappings.next_constant).hint(format!("{:?}", func));
                    self.mappings.next_constant += 1;
                    self.unifier.table.new_key(Term::Constant(cid, Kind::Other))
                }),
            Func::User(c) => self.register_global_constant_term(c.as_str()),
        }
    }

    pub fn register_global_constant(&mut self, constant_name: &str) -> ConstantId {
        let term = self.register_global_constant_term(constant_name);
        match self.unifier.table.probe_value(term) {
            Term::Constant(c, _) => c,
            _ => unreachable!(),
        }
    }
    pub fn register_global_constant_term(&mut self, constant_name: &str) -> TermId {
        *self
            .mappings
            .global_constant_table
            .entry(constant_name.into())
            .or_insert_with(|| {
                let cid = ConstantId::new(self.mappings.next_constant).hint(constant_name);
                self.mappings.next_constant += 1;
                self.unifier.table.new_key(Term::Constant(cid, Kind::Other))
            })
    }
    pub fn register_constant(
        &mut self,
        for_who: &ForWho,
        constant_name: &Ident<SmallStr>,
    ) -> TermId {
        *self
            .mappings
            .constant_table
            .entry(VariableKey {
                for_who: for_who.clone(),
                variable: constant_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Agent(session_id, agent) => {
                    let cid = ConstantId::new(self.mappings.next_constant)
                        .hint(format!("{}@{}:{}", &agent.0, session_id.0, constant_name));
                    self.mappings.next_constant += 1;
                    self.unifier.table.new_key(Term::Constant(cid, Kind::Other))
                }
            })
    }
    pub fn register_variable(
        &mut self,
        for_who: &ForWho,
        variable_name: &Ident<SmallStr>,
    ) -> TermId {
        *self
            .mappings
            .variable_table
            .entry(VariableKey {
                for_who: for_who.clone(),
                variable: variable_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Agent(session_id, agent) => self.unifier.table.new_key(Term::Variable(
                    Some(format!("{}@{}:{}", agent.0, session_id.0, variable_name)).into(),
                    Kind::Other,
                )),
            })
    }

    fn initiate_typed_variable(
        &mut self,
        for_who: &ForWho,
        initiators: &IndexMap<protocol::Variable, protocol::AgentName>,
        var: &protocol::Variable,
    ) -> TermId {
        match var {
            protocol::Variable::Agent(a) => self.get_agent(for_who, a),
            protocol::Variable::SymmetricKey(n) | protocol::Variable::Number(n) => {
                match (initiators.get(var), for_who) {
                    (Some(initiator), ForWho::Agent(_, agent)) if initiator == agent => {
                        self.register_constant(for_who, &n.convert())
                    }
                    (Some(_), ForWho::Agent(_, _)) => self.register_variable(for_who, &n.convert()),
                    (Some(initiator), ForWho::Intruder(session_id)) => self.register_constant(
                        &ForWho::Agent(*session_id, initiator.clone()),
                        &n.convert(),
                    ),
                    _ => todo!("Variable {:?} ({:?})", var, initiators),
                }
            }
        }
    }
    pub fn register_typed_term(
        &mut self,
        for_who: &ForWho,
        initiations: &IndexMap<protocol::Variable, protocol::AgentName>,
        term: protocol::Term,
    ) -> TermId {
        match term {
            protocol::Term::Variable(var) => {
                self.initiate_typed_variable(for_who, initiations, &var)
            }
            protocol::Term::Constant(c) => match c {
                protocol::Constant::Agent(a) => self.get_agent(for_who, &a),
                protocol::Constant::Function(f) => self.get_function_constant(f),
                protocol::Constant::Intruder => self.unifier.intruder(),
                protocol::Constant::Nonce(_) => todo!(),
            },
            protocol::Term::Composition { func, args } => {
                let term = Term::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant_term(u.as_str())),
                    },
                    args.into_iter()
                        .map(|arg| self.register_typed_term(for_who, initiations, arg))
                        .collect(),
                );
                self.unifier.table.new_key(term)
            }
            protocol::Term::Tuple(ts) => {
                let term = Term::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_typed_term(for_who, initiations, t))
                        .collect(),
                );
                self.unifier.table.new_key(term)
            }
        }
    }
    pub fn register_ast_term(
        &mut self,
        term: protocol::Term<crate::typing::UntypedStage>,
    ) -> TermId {
        match term {
            protocol::Term::Variable(v) => self.register_global_constant_term(v.as_str()),
            protocol::Term::Constant(_) => unreachable!(),
            protocol::Term::Composition { func, args } => {
                let term = Term::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant_term(u.as_str())),
                    },
                    args.into_iter()
                        .map(|t| self.register_ast_term(t))
                        .collect(),
                );

                self.unifier.table.new_key(term)
            }
            protocol::Term::Tuple(ts) => {
                let term = Term::Tuple(ts.into_iter().map(|t| self.register_ast_term(t)).collect());
                self.unifier.table.new_key(term)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Knowledge(pub Vec<TermId>);

impl Knowledge {
    pub fn resoled(&self, unifier: &mut Unifier) -> Vec<FullTerm> {
        self.0.iter().map(|&id| unifier.resolve_full(id)).collect()
    }
}

type SmallStr = SmolStr;

#[derive(Debug, Clone)]
pub struct Transaction {
    pub ast_node: protocol::PacketPattern,
    pub sender: TermId,
    pub receiver: TermId,
    pub direction: Direction,
    pub terms: Vec<TermId>,
}

#[derive(Debug, Clone)]
pub struct SessionAgent {
    pub name: Ident<SmallStr>,
    pub agent_id: TermId,
    pub initial_knowledge: Knowledge,
    pub strand: Vec<Transaction>,
}

#[derive(Debug, Clone)]
pub struct Secret {
    pub between_agents: Vec<TermId>,
    pub term: TermId,
}

#[derive(Debug, Clone)]
pub struct Session {
    pub session_id: SessionId,
    pub agents: Vec<SessionAgent>,
    pub secrets: Vec<Secret>,
}

impl Session {
    pub fn new(protocol: &Protocol, session_id: SessionId, converter: &mut Converter) -> Session {
        let agents = protocol
            .agents
            .iter()
            .map(|agent| {
                let for_who = ForWho::Agent(session_id, agent.name.clone());

                let mut initial_knowledge: Vec<_> = agent
                    .initial_knowledge
                    .iter()
                    .map(|term| {
                        converter.register_typed_term(&for_who, &protocol.initiations, term.clone())
                    })
                    .collect();

                let initiates = agent.terms.iter().flat_map(|pattern| {
                    if pattern.direction == Direction::Outgoing {
                        pattern.initiates.clone()
                    } else {
                        Default::default()
                    }
                });

                initial_knowledge.extend(initiates.map(|var| {
                    converter.initiate_typed_variable(&for_who, &protocol.initiations, &var)
                }));

                initial_knowledge
                    .sort_unstable_by_key(|term| converter.unifier.resolve_full(*term));
                initial_knowledge.dedup();

                SessionAgent {
                    name: agent.name.0.clone(),
                    agent_id: converter.get_agent(&for_who, &agent.name),
                    initial_knowledge: Knowledge(initial_knowledge),
                    strand: agent
                        .terms
                        .iter()
                        .map(|pattern| Transaction {
                            ast_node: pattern.clone(),
                            sender: converter.get_agent(&for_who, &pattern.from),
                            receiver: converter.get_agent(&for_who, &pattern.to),
                            direction: pattern.direction,
                            terms: pattern
                                .packet
                                .iter()
                                .map(|term| {
                                    converter.register_typed_term(
                                        &for_who,
                                        &protocol.initiations,
                                        term.clone(),
                                    )
                                })
                                .collect_vec(),
                        })
                        .collect(),
                }
            })
            .collect_vec();

        let secrets = protocol
            .goals
            .iter()
            .flat_map(|goal| match goal {
                protocol::Goal::SecretBetween(agents, terms) => terms
                    .iter()
                    .map(|term| Secret {
                        between_agents: agents
                            .iter()
                            .map(|agent| converter.get_agent(&ForWho::Intruder(session_id), agent))
                            .collect(),
                        term: converter.register_typed_term(
                            &ForWho::Intruder(session_id),
                            &protocol.initiations,
                            term.clone(),
                        ),
                    })
                    .collect_vec(),
                protocol::Goal::Authenticates(_, _, _) => {
                    eprintln!("⚠️ no authentication goals");
                    vec![]
                }
            })
            .collect();

        Session {
            session_id,
            agents,
            secrets,
        }
    }
    pub fn print(&self, unifier: &mut Unifier) {
        println!();
        println!("#############");
        println!("# SESSION {} #", self.session_id.0);
        println!("#############");
        for agent in &self.agents {
            println!();
            println!("> {} ({:?})", agent.name, agent.agent_id);
            println!(
                "> IK: {:?}",
                agent
                    .initial_knowledge
                    .0
                    .iter()
                    .map(|&term| unifier.resolve_full(term))
                    .collect_vec()
            );
            for t in &agent.strand {
                println!(
                    ">> {:?}->{:?}: {:?}",
                    unifier.resolve_full(t.sender),
                    unifier.resolve_full(t.receiver),
                    t.terms
                        .iter()
                        .map(|&term| unifier.resolve_full(term))
                        .collect_vec()
                );
            }
        }
        println!();
        println!("=== SECRETS ===");
        for secret in &self.secrets {
            println!("{:?}", unifier.resolve_full(secret.term));
        }
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
