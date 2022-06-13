use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct Document<'a> {
    pub name: Ident<'a>,
    pub types: Vec<(TypesKey, Vec<Ident<'a>>)>,
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
        a: Ident<'a>,
        b: Ident<'a>,
        msgs: Vec<Message<'a>>,
        weakly: bool,
    },
    SecretBetween {
        msg: Message<'a>,
        agents: Vec<Ident<'a>>,
        guessable: bool,
    },
}

#[derive(Clone, PartialEq, Eq)]
pub struct Ident<'a>(pub &'a str);

impl<'a> std::fmt::Debug for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> Deref for Ident<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum Message<'a> {
    Var(Ident<'a>),
    Fun(Ident<'a>, Vec<Message<'a>>),
    SymEnc(Vec<Message<'a>>, Box<Message<'a>>),
    AsymEnc(Vec<Message<'a>>, Box<Message<'a>>),
}

#[derive(Debug, Clone)]
pub struct Action<'a> {
    pub from: Ident<'a>,
    pub to: Ident<'a>,
    pub msgs: Vec<Message<'a>>,
}

#[derive(Debug, Clone)]
pub struct Knowledge<'a> {
    pub agents: Vec<(Ident<'a>, Vec<Message<'a>>)>,
}
