use std::ops::Deref;

use itertools::Itertools;
use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq)]
pub struct Document<S: Deref<Target = str>> {
    pub name: Ident<S>,
    pub types: Vec<(TypesKey, Vec<Ident<S>>)>,
    pub knowledge: Knowledge<S>,
    pub actions: Vec<Action<S>>,
    pub goals: Vec<Goal<S>>,
}

impl<S: Deref<Target = str>> Document<S> {
    pub fn map<T: Deref<Target = str>>(self, f: impl Fn(S) -> T + Copy) -> Document<T> {
        Document {
            name: self.name.map(f),
            types: self
                .types
                .into_iter()
                .map(|(k, is)| (k, is.into_iter().map(|i| i.map(f)).collect()))
                .collect(),
            knowledge: self.knowledge.map(f),
            actions: self.actions.into_iter().map(|a| a.map(f)).collect(),
            goals: self.goals.into_iter().map(|g| g.map(f)).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypesKey {
    Agent,
    Number,
    SymmetricKey,
    PublicKey,
    Function,
}

impl std::fmt::Display for TypesKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypesKey::Agent => write!(f, "Agent"),
            TypesKey::Number => write!(f, "Number"),
            TypesKey::SymmetricKey => write!(f, "Symmetric_key"),
            TypesKey::PublicKey => write!(f, "Public_key"),
            TypesKey::Function => write!(f, "Function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Goal<S: Deref<Target = str>> {
    Authenticates {
        a: Ident<S>,
        b: Ident<S>,
        terms: Vec<Term<S>>,
        weakly: bool,
    },
    SecretBetween {
        terms: Vec<Term<S>>,
        agents: Vec<Ident<S>>,
        guessable: bool,
    },
}

impl<S: Deref<Target = str>> Goal<S> {
    pub fn map<T: Deref<Target = str>>(self, f: impl Fn(S) -> T + Copy) -> Goal<T> {
        match self {
            Goal::Authenticates {
                a,
                b,
                terms,
                weakly,
            } => Goal::Authenticates {
                a: a.map(f),
                b: b.map(f),
                terms: terms.into_iter().map(|term| term.map(f)).collect(),
                weakly,
            },
            Goal::SecretBetween {
                terms,
                agents,
                guessable,
            } => Goal::SecretBetween {
                terms: terms.into_iter().map(|term| term.map(f)).collect(),
                agents: agents.into_iter().map(|i| i.map(f)).collect(),
                guessable,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term<S: Deref<Target = str>> {
    Var(Ident<S>),
    Fun(Ident<S>, Vec<Term<S>>),
    SymEnc(Vec<Term<S>>, Box<Term<S>>),
    AsymEnc(Vec<Term<S>>, Box<Term<S>>),
}

impl<S: Deref<Target = str>> std::fmt::Display for Term<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Term::Var(i) => write!(f, "{}", i),
            Term::Fun(i, terms) => write!(f, "{}({})", i, terms.iter().format(",")),
            Term::SymEnc(term, key) => write!(f, "{{| {} |}}{}", term.iter().format(","), key),
            Term::AsymEnc(term, key) => write!(f, "{{ {} }}{}", term.iter().format(","), key),
        }
    }
}

impl<S: Deref<Target = str>> Term<S> {
    pub fn map<T: Deref<Target = str>>(self, f: impl Fn(S) -> T + Copy) -> Term<T> {
        match self {
            Term::Var(i) => Term::Var(i.map(f)),
            Term::Fun(i, terms) => Term::Fun(
                i.map(f),
                terms.into_iter().map(|term| term.map(f)).collect(),
            ),

            Term::SymEnc(terms, key) => Term::SymEnc(
                terms.into_iter().map(|term| term.map(f)).collect(),
                box key.map(f),
            ),
            Term::AsymEnc(terms, key) => Term::AsymEnc(
                terms.into_iter().map(|term| term.map(f)).collect(),
                box key.map(f),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Action<S: Deref<Target = str>> {
    pub from: Ident<S>,
    pub to: Ident<S>,
    pub terms: Vec<Term<S>>,
}

impl<S: Deref<Target = str>> Action<S> {
    pub fn map<T: Deref<Target = str>>(self, f: impl Fn(S) -> T + Copy) -> Action<T> {
        Action {
            from: self.from.map(f),
            to: self.to.map(f),
            terms: self.terms.into_iter().map(|mgs| mgs.map(f)).collect(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Knowledge<S: Deref<Target = str>> {
    pub agents: Vec<(Ident<S>, Vec<Term<S>>)>,
    pub wheres: Vec<Where<S>>,
}

impl<S: Deref<Target = str>> Knowledge<S> {
    pub fn map<T: Deref<Target = str>>(self, f: impl Fn(S) -> T + Copy) -> Knowledge<T> {
        Knowledge {
            agents: self
                .agents
                .into_iter()
                .map(|(i, terms)| {
                    (
                        i.map(f),
                        terms.into_iter().map(|term| term.map(f)).collect(),
                    )
                })
                .collect(),

            wheres: self.wheres.into_iter().map(|w| w.map(f)).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Where<S: Deref<Target = str>> {
    NotEqual(Ident<S>, Ident<S>),
}

impl<S: Deref<Target = str>> Where<S> {
    pub fn map<T: Deref<Target = str>>(self, f: impl Fn(S) -> T + Copy) -> Where<T> {
        match self {
            Where::NotEqual(l, r) => Where::NotEqual(l.map(f), r.map(f)),
        }
    }
}

#[derive(Clone)]
pub struct Ident<S>(pub S, pub miette::SourceSpan);

impl<S> Ident<S> {
    pub fn map<T>(self, f: impl Fn(S) -> T + Copy) -> Ident<T> {
        Ident(f(self.0), self.1)
    }
    pub fn span(&self) -> miette::SourceSpan {
        self.1
    }
    pub fn convert<T>(&self) -> Ident<T>
    where
        S: Into<T> + Clone,
    {
        Ident(self.0.clone().into(), self.1)
    }
    pub fn as_str(&self) -> &str
    where
        S: Deref<Target = str>,
    {
        self.as_ref()
    }
    pub fn is_constant(&self) -> bool
    where
        S: Deref<Target = str>,
    {
        self.as_str().starts_with(|c: char| c.is_lowercase())
    }
    pub fn is_variable(&self) -> bool
    where
        S: Deref<Target = str>,
    {
        !self.is_constant()
    }
    pub fn contains(&self, byte_offset: usize) -> bool {
        let x = self.1;
        x.offset() <= byte_offset && byte_offset <= x.offset() + x.len()
    }
}

impl<S: std::hash::Hash> std::hash::Hash for Ident<S> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<S: Ord> Ord for Ident<S> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<S: PartialOrd> PartialOrd for Ident<S> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<S: PartialEq> PartialEq for Ident<S> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<S: Eq> Eq for Ident<S> {}

impl<S: Deref<Target = str>> std::fmt::Debug for Ident<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.deref())
    }
}
impl<S: Deref<Target = str>> std::fmt::Display for Ident<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.deref())
    }
}
impl<S: Deref<Target = str>> AsRef<str> for Ident<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<'a> From<&'a str> for Ident<&'a str> {
    fn from(s: &'a str) -> Self {
        Ident(s, (0, 0).into())
    }
}
impl<'a> From<&'a str> for Ident<String> {
    fn from(s: &'a str) -> Self {
        Ident(s.to_string(), (0, 0).into())
    }
}
impl<'a> From<&'a str> for Ident<SmolStr> {
    fn from(s: &'a str) -> Self {
        Ident(s.into(), (0, 0).into())
    }
}
