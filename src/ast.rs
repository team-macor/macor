use std::ops::Deref;

#[derive(Debug)]
pub struct Document<'a> {
    pub name: Ident<'a>,
    pub types: Vec<(TypesKey, Vec<Ident<'a>>)>,
    pub knowledge: Knowledge<'a>,
    pub actions: Vec<Action<'a>>,
    pub goals: Vec<Goal<'a>>,
}

#[derive(Debug)]
pub enum TypesKey {
    Agent,
    Number,
    SymmetricKey,
    PublicKey,
    Function,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Deref for Ident<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug)]
pub enum Message<'a> {
    Var(Ident<'a>),
    Fun(Ident<'a>, Vec<Message<'a>>),
}

#[derive(Debug)]
pub struct Action<'a> {
    pub from: Ident<'a>,
    pub to: Ident<'a>,
    pub msgs: Vec<Message<'a>>,
}

#[derive(Debug)]
pub struct Knowledge<'a> {
    pub agents: Vec<(Ident<'a>, Vec<Message<'a>>)>,
}
