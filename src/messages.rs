use std::collections::VecDeque;

use crate::dolev_yao::{augment_knowledge, can_derive};
use crate::protocol::{self, ActorName, Direction, Func, Protocol, SessionId};

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
pub struct MessageId(u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Kind {
    Actor,
    Other,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Message<M> {
    Intruder,
    Variable(Hint, Kind),
    Constant(ConstantId, Kind),
    Composition(Func<M>, Vec<M>),
    Tuple(Vec<M>),
}

impl<M> Message<M> {
    pub fn kind(&self) -> Kind {
        match self {
            Message::Intruder => Kind::Actor,
            Message::Variable(_, kind) | Message::Constant(_, kind) => *kind,
            Message::Composition(_, _) | Message::Tuple(_) => Kind::Other,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FullMessage(pub Message<Box<FullMessage>>);

impl<M: std::fmt::Debug> std::fmt::Debug for Message<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Intruder => write!(f, "{}", Paint::magenta("i").bold()),
            Self::Variable(arg0, Kind::Actor) => {
                if let Some(k) = arg0.as_deref() {
                    write!(f, "{}", Paint::cyan(k).underline())
                } else {
                    write!(f, "{}", Paint::cyan("Actor").underline())
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
                Kind::Actor => Paint::red(c).underline().fmt(f),
                Kind::Other => Paint::red(c).fmt(f),
            },
            Self::Composition(Func::AsymEnc, args) => {
                let (msg, key) = (&args[0], &args[1]);
                write!(
                    f,
                    "{l} {msg:?} {r}\x1b[00m{key:?}",
                    l = Paint::magenta("{"),
                    r = Paint::magenta("}"),
                )
            }
            Self::Composition(Func::SymEnc, args) => {
                let (msg, key) = (&args[0], &args[1]);
                write!(
                    f,
                    "{l} {msg:?} {r}\x1b[00m{key:?}",
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

impl std::fmt::Debug for FullMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl UnifyValue for Message<MessageId> {
    type Error = ();

    fn unify_values(l: &Self, r: &Self) -> Result<Self, Self::Error> {
        use self::Message::*;

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

impl UnifyKey for MessageId {
    type Value = Message<MessageId>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        MessageId(u)
    }

    fn tag() -> &'static str {
        "MessageId"
    }
}

#[derive(Debug, Clone)]
pub struct Unifier {
    intruder: MessageId,
    table: InPlaceUnificationTable<MessageId>,
}

impl Default for Unifier {
    fn default() -> Self {
        let mut table = InPlaceUnificationTable::default();
        let intruder = table.new_key(Message::Intruder);
        Self { intruder, table }
    }
}

impl Unifier {
    pub fn intruder(&self) -> MessageId {
        self.intruder
    }
    /// Recursively unifies the two messages and returns either of the passed
    /// messages if they indeed do unify (since they are now equivalent), or
    /// else produces and error.
    pub fn unify(&mut self, l: MessageId, r: MessageId) -> Result<MessageId, ()> {
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
    fn unify_inner(&mut self, l: MessageId, r: MessageId) -> Result<MessageId, ()> {
        use self::Message::*;

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
    pub fn are_unified(&mut self, a: MessageId, b: MessageId) -> bool {
        use self::Message::*;

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
    // pub fn try_unify(&mut self, a: MessageId, b: MessageId) -> Result<(), ()> {
    //     // TODO: Should this be recursive?
    //     if self.table.unioned(a, b) {
    //         return Ok(());
    //     }

    //     use self::Message::*;

    //     match (self.probe_value(a), self.probe_value(b)) {
    //         (Message::Agent(_))
    //         (a, b) => todo!("{:?} {:?}", a, b),
    //     }

    //     // println!(
    //     //     "Are unified: {:?} with {:?}",
    //     //     self.resolve_full(a),
    //     //     self.resolve_full(b)
    //     // );

    //     false
    // }

    pub fn probe_value(&mut self, id: MessageId) -> Message<MessageId> {
        self.table.probe_value(id)
    }

    pub fn resolve_full(&mut self, id: MessageId) -> FullMessage {
        let msg = match self.table.probe_value(id) {
            Message::Intruder => Message::Intruder,
            Message::Variable(v, kind) => Message::Variable(v, kind),
            Message::Constant(c, kind) => Message::Constant(c, kind),
            Message::Composition(func, args) => Message::Composition(
                func.map(|&id| box self.resolve_full(id)),
                args.into_iter()
                    .map(|arg| box self.resolve_full(arg))
                    .collect(),
            ),
            Message::Tuple(ts) => {
                Message::Tuple(ts.into_iter().map(|t| box self.resolve_full(t)).collect())
            }
        };
        FullMessage(msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Unifier {
        fn constant(&mut self, i: u32) -> MessageId {
            self.table
                .new_key(Message::Constant(ConstantId(i, Hint::none()), Kind::Other))
        }
        fn variable(&mut self) -> MessageId {
            self.table
                .new_key(Message::Variable(Hint::none(), Kind::Other))
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
            Message::Constant(ConstantId(0, Hint::none()), Kind::Other)
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

        let a = unifier.table.new_key(Message::Tuple(vec![x, y_free]));
        let b = unifier.table.new_key(Message::Tuple(vec![x_free, y]));

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullMessage(Message::Tuple(vec![
                box FullMessage(Message::Constant(ConstantId(0, Hint::none()), Kind::Other)),
                box FullMessage(Message::Constant(ConstantId(1, Hint::none()), Kind::Other))
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
            .new_key(Message::Composition(Func::Exp, vec![x, y_free]));
        let b = unifier
            .table
            .new_key(Message::Composition(Func::Exp, vec![x_free, y]));

        unifier.unify(a, b)?;

        assert_eq!(unifier.resolve_full(a), unifier.resolve_full(b));
        assert_eq!(
            unifier.resolve_full(a),
            FullMessage(Message::Composition(
                Func::Exp,
                vec![
                    box FullMessage(Message::Constant(ConstantId(0, Hint::none()), Kind::Other)),
                    box FullMessage(Message::Constant(ConstantId(1, Hint::none()), Kind::Other))
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

        let a = unifier.table.new_key(Message::Tuple(vec![y, free]));
        let b = unifier.table.new_key(Message::Tuple(vec![x, y]));

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
            FullMessage(Message::Constant(ConstantId(1, Hint::none()), Kind::Other))
        );
        assert_eq!(
            world_2.resolve_full(a),
            FullMessage(Message::Constant(ConstantId(2, Hint::none()), Kind::Other))
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
            FullMessage(Message::Constant(ConstantId(1, Hint::none()), Kind::Other))
        );

        unifier.table.rollback_to(snap);

        unifier.unify(a, y)?;

        assert_eq!(
            unifier.resolve_full(a),
            FullMessage(Message::Constant(ConstantId(2, Hint::none()), Kind::Other))
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
//              s needs to unify the expected message with the received:
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
    Actor(SessionId, protocol::ActorName),
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
    global_actor_table: IndexMap<ActorName, MessageId>,
    actor_table: IndexMap<(ForWho, SmallStr), MessageId>,
    func_table: IndexMap<Func, MessageId>,
    global_constant_table: IndexMap<SmallStr, MessageId>,
    constant_table: IndexMap<VariableKey, MessageId>,
    variable_table: IndexMap<VariableKey, MessageId>,
}

impl Default for Mappings {
    fn default() -> Self {
        Self {
            next_constant: 1,
            global_actor_table: Default::default(),
            actor_table: Default::default(),
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

    pub fn get_actor(&mut self, ctx: &ForWho, agent_to_get: &protocol::ActorName) -> MessageId {
        if agent_to_get.0.is_constant() {
            return *self
                .mappings
                .global_actor_table
                .entry(agent_to_get.clone())
                .or_insert_with(|| {
                    let cid =
                        ConstantId::new(self.mappings.next_constant).hint(agent_to_get.0.clone());
                    self.mappings.next_constant += 1;
                    self.unifier
                        .table
                        .new_key(Message::Constant(cid, Kind::Actor))
                });
        }

        const AGENTS_FIX_TO_THEMSELVES: bool = false;

        match ctx {
            ForWho::Intruder(session_id) => self.get_actor(
                &ForWho::Actor(*session_id, agent_to_get.clone()),
                agent_to_get,
            ),
            ForWho::Actor(session_id, agent) => *self
                .mappings
                .actor_table
                .entry((ctx.clone(), agent_to_get.0.clone().into()))
                .or_insert_with(|| {
                    if agent_to_get.0.is_constant() {
                        let cid = ConstantId::new(self.mappings.next_constant)
                            .hint(agent_to_get.0.clone());
                        self.mappings.next_constant += 1;
                        self.unifier
                            .table
                            .new_key(Message::Constant(cid, Kind::Actor))
                    } else if agent_to_get == agent && AGENTS_FIX_TO_THEMSELVES {
                        let cid = ConstantId::new(self.mappings.next_constant).hint(format!(
                            "{:?}_{:?}",
                            agent_to_get.0.clone(),
                            session_id.0
                        ));
                        self.mappings.next_constant += 1;
                        self.unifier
                            .table
                            .new_key(Message::Constant(cid, Kind::Actor))
                    } else {
                        self.unifier.table.new_key(Message::Variable(
                            Some(format!("{}@{}:{}", agent.0, session_id.0, agent_to_get.0)).into(),
                            Kind::Actor,
                        ))
                    }
                }),
        }
    }
    pub fn get_function_constant(&mut self, func: Func) -> MessageId {
        match func {
            Func::SymEnc | Func::AsymEnc | Func::Exp | Func::Inv => *self
                .mappings
                .func_table
                .entry(func.clone())
                .or_insert_with(|| {
                    let cid =
                        ConstantId::new(self.mappings.next_constant).hint(format!("{:?}", func));
                    self.mappings.next_constant += 1;
                    self.unifier
                        .table
                        .new_key(Message::Constant(cid, Kind::Other))
                }),
            Func::User(c) => self.register_global_constant_msg(c.as_str()),
        }
    }

    pub fn register_global_constant(&mut self, constant_name: &str) -> ConstantId {
        let msg = self.register_global_constant_msg(constant_name);
        match self.unifier.table.probe_value(msg) {
            Message::Constant(c, _) => c,
            _ => unreachable!(),
        }
    }
    pub fn register_global_constant_msg(&mut self, constant_name: &str) -> MessageId {
        *self
            .mappings
            .global_constant_table
            .entry(constant_name.into())
            .or_insert_with(|| {
                let cid = ConstantId::new(self.mappings.next_constant).hint(constant_name);
                self.mappings.next_constant += 1;
                self.unifier
                    .table
                    .new_key(Message::Constant(cid, Kind::Other))
            })
    }
    pub fn register_constant(
        &mut self,
        for_who: &ForWho,
        constant_name: &Ident<SmallStr>,
    ) -> MessageId {
        *self
            .mappings
            .constant_table
            .entry(VariableKey {
                for_who: for_who.clone(),
                variable: constant_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Actor(session_id, agent) => {
                    let cid = ConstantId::new(self.mappings.next_constant)
                        .hint(format!("{}@{}:{}", &agent.0, session_id.0, constant_name));
                    self.mappings.next_constant += 1;
                    self.unifier
                        .table
                        .new_key(Message::Constant(cid, Kind::Other))
                }
            })
    }
    pub fn register_variable(
        &mut self,
        for_who: &ForWho,
        variable_name: &Ident<SmallStr>,
    ) -> MessageId {
        *self
            .mappings
            .variable_table
            .entry(VariableKey {
                for_who: for_who.clone(),
                variable: variable_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Actor(session_id, agent) => self.unifier.table.new_key(Message::Variable(
                    Some(format!("{}@{}:{}", agent.0, session_id.0, variable_name)).into(),
                    Kind::Other,
                )),
            })
    }

    fn initiate_typed_variable(
        &mut self,
        for_who: &ForWho,
        initiators: &IndexMap<protocol::Variable, protocol::ActorName>,
        var: &protocol::Variable,
    ) -> MessageId {
        match var {
            protocol::Variable::Actor(a) => self.get_actor(for_who, a),
            protocol::Variable::SymmetricKey(n) | protocol::Variable::Number(n) => {
                match (initiators.get(var), for_who) {
                    (Some(initiator), ForWho::Actor(_, agent)) if initiator == agent => {
                        self.register_constant(for_who, &n.convert())
                    }
                    (Some(_), ForWho::Actor(_, _)) => self.register_variable(for_who, &n.convert()),
                    (Some(initiator), ForWho::Intruder(session_id)) => self.register_constant(
                        &ForWho::Actor(*session_id, initiator.clone()),
                        &n.convert(),
                    ),
                    _ => todo!("Variable {:?} ({:?})", var, initiators),
                }
            }
        }
    }
    pub fn register_typed_message(
        &mut self,
        for_who: &ForWho,
        initiations: &IndexMap<protocol::Variable, protocol::ActorName>,
        msg: protocol::Message,
    ) -> MessageId {
        match msg {
            protocol::Message::Variable(var) => {
                self.initiate_typed_variable(for_who, initiations, &var)
            }
            protocol::Message::Constant(c) => match c {
                protocol::Constant::Actor(a) => self.get_actor(for_who, &a),
                protocol::Constant::Function(f) => self.get_function_constant(f),
                protocol::Constant::Intruder => self.unifier.intruder(),
                protocol::Constant::Nonce(_) => todo!(),
            },
            protocol::Message::Composition { func, args } => {
                let msg = Message::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant_msg(u.as_str())),
                    },
                    args.into_iter()
                        .map(|arg| self.register_typed_message(for_who, initiations, arg))
                        .collect(),
                );
                self.unifier.table.new_key(msg)
            }
            protocol::Message::Tuple(ts) => {
                let msg = Message::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_typed_message(for_who, initiations, t))
                        .collect(),
                );
                self.unifier.table.new_key(msg)
            }
        }
    }
    pub fn register_ast_message(
        &mut self,
        msg: protocol::Message<crate::typing::UntypedStage>,
    ) -> MessageId {
        match msg {
            protocol::Message::Variable(v) => self.register_global_constant_msg(v.as_str()),
            protocol::Message::Constant(_) => unreachable!(),
            protocol::Message::Composition { func, args } => {
                let msg = Message::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant_msg(u.as_str())),
                    },
                    args.into_iter()
                        .map(|t| self.register_ast_message(t))
                        .collect(),
                );

                self.unifier.table.new_key(msg)
            }
            protocol::Message::Tuple(ts) => {
                let msg = Message::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_ast_message(t))
                        .collect(),
                );
                self.unifier.table.new_key(msg)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Knowledge(pub Vec<MessageId>);

impl Knowledge {
    pub fn resoled(&self, unifier: &mut Unifier) -> Vec<FullMessage> {
        self.0.iter().map(|&id| unifier.resolve_full(id)).collect()
    }
}

type SmallStr = SmolStr;

#[derive(Debug, Clone)]
pub struct Transaction {
    pub ast_node: protocol::PacketPattern,
    pub sender: MessageId,
    pub receiver: MessageId,
    pub direction: Direction,
    pub messages: Vec<MessageId>,
}

#[derive(Debug, Clone)]
pub struct SessionActor {
    pub name: Ident<SmallStr>,
    pub actor_id: MessageId,
    pub initial_knowledge: Knowledge,
    pub strand: Vec<Transaction>,
}

#[derive(Debug, Clone)]
pub struct Secret {
    pub between_actors: Vec<MessageId>,
    pub msg: MessageId,
}

#[derive(Debug, Clone)]
pub struct Session {
    pub session_id: SessionId,
    pub actors: Vec<SessionActor>,
    pub secrets: Vec<Secret>,
}

impl Session {
    pub fn new(protocol: &Protocol, session_id: SessionId, converter: &mut Converter) -> Session {
        let actors = protocol
            .actors
            .iter()
            .map(|actor| {
                let for_who = ForWho::Actor(session_id, actor.name.clone());

                let mut initial_knowledge: Vec<_> = actor
                    .initial_knowledge
                    .iter()
                    .map(|msg| {
                        converter.register_typed_message(
                            &for_who,
                            &protocol.initiations,
                            msg.clone(),
                        )
                    })
                    .collect();

                let initiates = actor.messages.iter().flat_map(|pattern| {
                    if pattern.direction == Direction::Outgoing {
                        pattern.initiates.clone()
                    } else {
                        Default::default()
                    }
                });

                initial_knowledge.extend(initiates.map(|var| {
                    converter.initiate_typed_variable(&for_who, &protocol.initiations, &var)
                }));

                initial_knowledge.sort_unstable_by_key(|msg| converter.unifier.resolve_full(*msg));
                initial_knowledge.dedup();

                SessionActor {
                    name: actor.name.0.clone(),
                    actor_id: converter.get_actor(&for_who, &actor.name),
                    initial_knowledge: Knowledge(initial_knowledge),
                    strand: actor
                        .messages
                        .iter()
                        .map(|pattern| Transaction {
                            ast_node: pattern.clone(),
                            sender: converter.get_actor(&for_who, &pattern.from),
                            receiver: converter.get_actor(&for_who, &pattern.to),
                            direction: pattern.direction,
                            messages: pattern
                                .packet
                                .iter()
                                .map(|msg| {
                                    converter.register_typed_message(
                                        &for_who,
                                        &protocol.initiations,
                                        msg.clone(),
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
                protocol::Goal::SecretBetween(agents, msgs) => msgs
                    .iter()
                    .map(|msg| Secret {
                        between_actors: agents
                            .iter()
                            .map(|agent| converter.get_actor(&ForWho::Intruder(session_id), agent))
                            .collect(),
                        msg: converter.register_typed_message(
                            &ForWho::Intruder(session_id),
                            &protocol.initiations,
                            msg.clone(),
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
            actors,
            secrets,
        }
    }
    pub fn print(&self, unifier: &mut Unifier) {
        println!();
        println!("#############");
        println!("# SESSION {} #", self.session_id.0);
        println!("#############");
        for actor in &self.actors {
            println!();
            println!("> {} ({:?})", actor.name, actor.actor_id);
            println!(
                "> IK: {:?}",
                actor
                    .initial_knowledge
                    .0
                    .iter()
                    .map(|&msg| unifier.resolve_full(msg))
                    .collect_vec()
            );
            for t in &actor.strand {
                println!(
                    ">> {:?}->{:?}: {:?}",
                    unifier.resolve_full(t.sender),
                    unifier.resolve_full(t.receiver),
                    t.messages
                        .iter()
                        .map(|&msg| unifier.resolve_full(msg))
                        .collect_vec()
                );
            }
        }
        println!();
        println!("=== SECRETS ===");
        for secret in &self.secrets {
            println!("{:?}", unifier.resolve_full(secret.msg));
        }
    }
}

impl Knowledge {
    pub fn can_construct(&self, unifier: &mut Unifier, msg: MessageId) -> bool {
        // eprintln!(
        //     "Can construct {:?} with knowledge {:?}",
        //     unifier.resolve_full(msg),
        //     self.0
        //         .iter()
        //         .map(|&msg| unifier.resolve_full(msg))
        //         .format(", ")
        // );
        // if self.0.iter().any(|&k| unifier.table.unioned(k, msg)) {
        //     return true;
        // }
        // if self.0.iter().any(|&k| unifier.unify(k, msg).is_ok()) {
        //     return true;
        // }

        if can_derive(self, msg, unifier) {
            return true;
        }

        false
    }
}
