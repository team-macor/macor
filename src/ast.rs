#[derive(Debug, Clone)]
pub struct Document<'a> {
    pub name: Ident<&'a str>,
    pub types: Vec<(TypesKey, Vec<Ident<&'a str>>)>,
    pub knowledge: Knowledge<'a>,
    pub actions: Vec<Action<'a>>,
    pub goals: Vec<Goal<'a>>,
}

#[derive(Debug, Clone)]
pub enum TypesKey {
    Agent,
    Number,
    SymmetricKey,
    PublicKey,
    Function,
}

#[derive(Debug, Clone)]
pub enum Goal<'a> {
    Authenticates {
        a: Ident<&'a str>,
        b: Ident<&'a str>,
        msgs: Vec<Message<'a>>,
        weakly: bool,
    },
    SecretBetween {
        msg: Message<'a>,
        agents: Vec<Ident<&'a str>>,
        guessable: bool,
    },
}

#[derive(Clone)]
pub struct Ident<S>(pub S, pub miette::SourceSpan);

impl<S> Ident<S> {
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
        S: AsRef<str>,
    {
        self.as_ref()
    }
    pub fn is_constant(&self) -> bool
    where
        S: AsRef<str>,
    {
        self.as_str().starts_with(|c: char| c.is_lowercase())
    }
    pub fn is_variable(&self) -> bool
    where
        S: AsRef<str>,
    {
        !self.is_constant()
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

impl<S: AsRef<str>> std::fmt::Debug for Ident<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ref())
    }
}
impl<S: AsRef<str>> std::fmt::Display for Ident<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ref())
    }
}
impl<S: AsRef<str>> AsRef<str> for Ident<S> {
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

#[derive(Debug, Clone)]
pub enum Message<'a> {
    Var(Ident<&'a str>),
    Fun(Ident<&'a str>, Vec<Message<'a>>),
    SymEnc(Vec<Message<'a>>, Box<Message<'a>>),
    AsymEnc(Vec<Message<'a>>, Box<Message<'a>>),
}

#[derive(Debug, Clone)]
pub struct Action<'a> {
    pub from: Ident<&'a str>,
    pub to: Ident<&'a str>,
    pub msgs: Vec<Message<'a>>,
}

#[derive(Debug, Clone)]
pub struct Knowledge<'a> {
    pub agents: Vec<(Ident<&'a str>, Vec<Message<'a>>)>,
}
