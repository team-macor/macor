#![allow(non_snake_case, non_camel_case_types)]
use anyhow::Result;
use omg::terms::*;
use omg::{Base, Channel};
use std::{collections::HashMap, marker::PhantomData};
pub trait Terms: Base {
    type User_pk: Clone + std::fmt::Debug + serde::Serialize + serde::de::DeserializeOwned;
    fn pk(&mut self, arg_0: &<Self as Base>::Agent) -> <Self as Base>::AsymmetricKey;
}
impl Terms for () {
    type User_pk = ();
    fn pk(&mut self, arg_0: &<Self as Base>::Agent) -> <Self as Base>::AsymmetricKey {}
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde :: Serialize, serde :: Deserialize)]
pub enum ProtocolAgent {
    A,
    B,
}
#[doc = r" All messages in the protocol"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub enum Message<T: Terms> {
    Initiate(Initiate<T>),
    M0(M0<T>),
    M1(M1<T>),
}
#[doc = "[@Agent(A), @Agent(B), pk(@Agent(A)), pk(@Agent(B)), Inverse(pk(@Agent(A)))]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct Initiate<T: Terms>(
    #[doc = "@Agent(A)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Agent(B)"] pub Agent<<T as Base>::Agent>,
    #[doc = "pk(@Agent(A))"]
    pub  AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
    #[doc = "pk(@Agent(B))"]
    pub  AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
    #[doc = "Inverse(pk(@Agent(A)))"]
    pub  Inv<
        <T as Base>::AsymmetricKeyInv,
        (AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>),
    >,
);
#[doc = "[{| :(@Agent(A), @Agent(B), @Number(NA)) |}@SymmetricKey(KAB), { :(@Agent(A), @Agent(B), @SymmetricKey(KAB)) }pk(@Agent(B))]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M0<T: Terms>(
    #[doc = "{| :(@Agent(A), @Agent(B), @Number(NA)) |}@SymmetricKey(KAB)"]
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            Agent<<T as Base>::Agent>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
        )>,
        SymmetricKey<<T as Base>::SymmetricKey>,
    >,
    #[doc = "{ :(@Agent(A), @Agent(B), @SymmetricKey(KAB)) }pk(@Agent(B))"]
    pub  AsymEnc<
        <T as Base>::AsymEnc,
        Tuple<(
            Agent<<T as Base>::Agent>,
            Agent<<T as Base>::Agent>,
            SymmetricKey<<T as Base>::SymmetricKey>,
        )>,
        AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
    >,
);
#[doc = "[@SymmetricKey(KAB), @Number(NA), @Agent(A), @Agent(B)]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M1<T: Terms>(
    #[doc = "@SymmetricKey(KAB)"] pub SymmetricKey<<T as Base>::SymmetricKey>,
    #[doc = "@Number(NA)"] pub Number<<T as Base>::Number>,
    #[doc = "@Agent(A)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Agent(B)"] pub Agent<<T as Base>::Agent>,
);
pub mod agent_A {
    use super::*;
    #[doc = "The state for agent A"]
    pub struct State<T: Terms> {
        knowledge: Knowledge<T>,
        stage: Stage,
    }
    #[doc = r" The stage of the protocol for agent"]
    pub enum Stage {
        Initiate,
        M1,
        Terminated,
    }
    impl Default for Stage {
        fn default() -> Self {
            Self::Initiate
        }
    }
    #[derive(Debug)]
    pub enum SendMessage<T: Terms> {
        Nothing,
        Send {
            msg: Outgoing<T>,
            connect: bool,
            to: (ProtocolAgent, T::Agent),
        },
    }
    #[derive(Debug)]
    pub enum NextAction<T: Terms> {
        Terminate,
        RecvFrom(ProtocolAgent, T::Agent),
        ListenOn(ListenPort, T::Agent),
    }
    #[derive(Debug)]
    pub struct Response<T: Terms> {
        pub save_connection_as: Option<ProtocolAgent>,
        pub send: SendMessage<T>,
        pub action: NextAction<T>,
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde :: Serialize, serde :: Deserialize)]
    pub enum ListenPort {}
    impl From<ListenPort> for ProtocolAgent {
        fn from(value: ListenPort) -> Self {
            match value {}
        }
    }
    pub type InitialKnowledge<T> = (
        Agent<<T as Base>::Agent>,
        Agent<<T as Base>::Agent>,
        AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
        AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
        Inv<
            <T as Base>::AsymmetricKeyInv,
            (AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>),
        >,
    );
    impl<T: Terms> State<T> {
        pub fn init() -> Self {
            State {
                knowledge: Default::default(),
                stage: Default::default(),
            }
        }
        pub fn handle_message(&mut self, base: &mut T, msg: Ingoing<T>) -> Result<Response<T>> {
            match (&self.stage, &msg) {
                (Stage::Initiate, Ingoing::Initiate(_)) => self.stage = Stage::M1,
                (Stage::M1, Ingoing::M1(_)) => self.stage = Stage::Terminated,
                _ => anyhow::bail!("out of order!"),
            }
            progress(base, &mut self.knowledge, msg)
        }
    }
    #[doc = r" The knowledge required for agent"]
    pub struct Knowledge<T: Terms> {
        #[doc = "@Agent(A)"]
        i0: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i1: Option<Agent<<T as Base>::Agent>>,
        #[doc = "pk(@Agent(A))"]
        i2: Option<AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "pk(@Agent(B))"]
        i3: Option<AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "Inverse(pk(@Agent(A)))"]
        i4: Option<
            Inv<
                <T as Base>::AsymmetricKeyInv,
                (AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>),
            >,
        >,
        #[doc = "@Number(NA)"]
        i5: Option<Number<<T as Base>::Number>>,
        #[doc = "@SymmetricKey(KAB)"]
        i6: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        #[doc = ":(@Agent(A), @Agent(B), @Number(NA))"]
        i7: Option<
            Tuple<(
                Agent<<T as Base>::Agent>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        #[doc = "{| :(@Agent(A), @Agent(B), @Number(NA)) |}@SymmetricKey(KAB)"]
        i8: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    Agent<<T as Base>::Agent>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                )>,
                SymmetricKey<<T as Base>::SymmetricKey>,
            >,
        >,
        #[doc = ":(@Agent(A), @Agent(B), @SymmetricKey(KAB))"]
        i9: Option<
            Tuple<(
                Agent<<T as Base>::Agent>,
                Agent<<T as Base>::Agent>,
                SymmetricKey<<T as Base>::SymmetricKey>,
            )>,
        >,
        #[doc = "{ :(@Agent(A), @Agent(B), @SymmetricKey(KAB)) }pk(@Agent(B))"]
        i10: Option<
            AsymEnc<
                <T as Base>::AsymEnc,
                Tuple<(
                    Agent<<T as Base>::Agent>,
                    Agent<<T as Base>::Agent>,
                    SymmetricKey<<T as Base>::SymmetricKey>,
                )>,
                AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = "@SymmetricKey(KAB)"]
        i11: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        #[doc = "@Number(NA)"]
        i12: Option<Number<<T as Base>::Number>>,
        #[doc = "@Agent(A)"]
        i13: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i14: Option<Agent<<T as Base>::Agent>>,
    }
    impl<T: Terms> Default for Knowledge<T> {
        fn default() -> Self {
            Self {
                i0: None,
                i1: None,
                i2: None,
                i3: None,
                i4: None,
                i5: None,
                i6: None,
                i7: None,
                i8: None,
                i9: None,
                i10: None,
                i11: None,
                i12: None,
                i13: None,
                i14: None,
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), pk(@Agent(A)), pk(@Agent(B)), Inverse(pk(@Agent(A)))"]
        Initiate(Initiate<T>),
        #[doc = "@SymmetricKey(KAB), @Number(NA), @Agent(A), @Agent(B)"]
        M1(M1<T>),
    }
    impl<T: Terms> From<Ingoing<T>> for Message<T> {
        fn from(v: Ingoing<T>) -> Self {
            match v {
                Ingoing::Initiate(m) => Self::Initiate(m),
                Ingoing::M1(m) => Self::M1(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::Initiate(m) => Ok(Self::Initiate(m)),
                Message::M1(m) => Ok(Self::M1(m)),
                _ => Err(()),
            }
        }
    }
    #[doc = r" Outgoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    pub enum Outgoing<T: Terms> {
        #[doc = "{| :(@Agent(A), @Agent(B), @Number(NA)) |}@SymmetricKey(KAB), { :(@Agent(A), @Agent(B), @SymmetricKey(KAB)) }pk(@Agent(B))"]
        M0(M0<T>),
    }
    impl<T: Terms> From<Outgoing<T>> for Message<T> {
        fn from(v: Outgoing<T>) -> Self {
            match v {
                Outgoing::M0(m) => Self::M0(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M0(m) => Ok(Self::M0(m)),
                _ => Err(()),
            }
        }
    }
    pub fn run<T: Terms, C: Channel<Message<T>, Message<T>>>(
        base: &mut T,
        ports: impl Fn(ListenPort) -> C::Addr,
        con: impl Fn(ProtocolAgent, &T::Agent) -> C::Recipient,
        init: Initiate<T>,
    ) -> anyhow::Result<()>
    where
        C::Error: Send + Sync + 'static,
    {
        let mut channels = HashMap::<ProtocolAgent, C>::new();
        let mut state = State::init();
        let mut action = state.handle_message(base, Ingoing::Initiate(init))?;
        loop {
            match action.send {
                SendMessage::Send {
                    msg,
                    connect,
                    to: (name, to),
                } => {
                    let c = if connect {
                        let c = C::connect(con(name, &to))?;
                        channels.entry(name).or_insert(c)
                    } else {
                        channels.get_mut(&name).unwrap()
                    };
                    c.send(msg.into())?;
                }
                SendMessage::Nothing => {}
            }
            let channel = match action.action {
                NextAction::Terminate => break Ok(()),
                NextAction::RecvFrom(p, c) => channels.get_mut(&p).unwrap(),
                NextAction::ListenOn(p, c) => {
                    let channel = C::listen(ports(p))?;
                    channels.entry(p.into()).or_insert(channel)
                }
            };
            let msg = channel.receive()?;
            action = state.handle_message(base, msg.try_into().unwrap())?;
        }
    }
    fn progress<T: Terms>(
        base: &mut T,
        knowledge: &mut Knowledge<T>,
        msg: Ingoing<T>,
    ) -> Result<Response<T>> {
        match msg {
            Ingoing::Initiate(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(Initiate));
                let _enter = scope.enter();
                knowledge.i0 = Some(m.0);
                knowledge.i1 = Some(m.1);
                knowledge.i2 = Some(m.2);
                knowledge.i3 = Some(m.3);
                knowledge.i4 = Some(m.4);
                knowledge.i5 = Some(Number(base.generate_nonce()));
                knowledge.i6 = Some(SymmetricKey(base.generate_sym_key()));
                knowledge.i7 = Some(Tuple((
                    knowledge.i0.clone().unwrap(),
                    knowledge.i1.clone().unwrap(),
                    knowledge.i5.clone().unwrap(),
                )));
                knowledge.i8 = Some(SymEnc(
                    base.symmetric_encrypt(
                        knowledge.i7.as_ref().unwrap(),
                        knowledge.i6.as_ref().unwrap(),
                    )?,
                    PhantomData,
                ));
                knowledge.i9 = Some(Tuple((
                    knowledge.i0.clone().unwrap(),
                    knowledge.i1.clone().unwrap(),
                    knowledge.i6.clone().unwrap(),
                )));
                knowledge.i10 = Some(AsymEnc(
                    base.asymmetric_encrypt(
                        knowledge.i9.as_ref().unwrap(),
                        &knowledge.i3.as_ref().unwrap().0,
                    )?,
                    PhantomData,
                ));
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Send {
                        msg: Outgoing::M0(M0(
                            knowledge.i8.clone().unwrap(),
                            knowledge.i10.clone().unwrap(),
                        )),
                        connect: true,
                        to: (ProtocolAgent::B, knowledge.i1.clone().unwrap().0),
                    },
                    action: NextAction::RecvFrom(ProtocolAgent::B, knowledge.i1.clone().unwrap().0),
                })
            }
            Ingoing::M1(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M1));
                let _enter = scope.enter();
                knowledge.i11 = Some(m.0);
                assert_eq!(knowledge.i6, knowledge.i11);
                knowledge.i12 = Some(m.1);
                assert_eq!(knowledge.i5, knowledge.i12);
                knowledge.i13 = Some(m.2);
                assert_eq!(knowledge.i0, knowledge.i13);
                knowledge.i14 = Some(m.3);
                assert_eq!(knowledge.i1, knowledge.i14);
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Nothing,
                    action: NextAction::Terminate,
                })
            }
        }
    }
}
pub mod agent_B {
    use super::*;
    #[doc = "The state for agent B"]
    pub struct State<T: Terms, F>
    where
        F: Fn(&mut T, &M0<T>) -> Result<InitialKnowledge<T>>,
    {
        knowledge: Knowledge<T>,
        stage: Stage,
        compute_initial_knowledge: F,
    }
    #[doc = r" The stage of the protocol for agent"]
    pub enum Stage {
        M0,
        Terminated,
    }
    impl Default for Stage {
        fn default() -> Self {
            Self::M0
        }
    }
    #[derive(Debug)]
    pub enum SendMessage<T: Terms> {
        Nothing,
        Send {
            msg: Outgoing<T>,
            connect: bool,
            to: (ProtocolAgent, T::Agent),
        },
    }
    #[derive(Debug)]
    pub enum NextAction<T: Terms> {
        Terminate,
        RecvFrom(ProtocolAgent, T::Agent),
        ListenOn(ListenPort, T::Agent),
    }
    #[derive(Debug)]
    pub struct Response<T: Terms> {
        pub save_connection_as: Option<ProtocolAgent>,
        pub send: SendMessage<T>,
        pub action: NextAction<T>,
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde :: Serialize, serde :: Deserialize)]
    pub enum ListenPort {
        A,
    }
    impl From<ListenPort> for ProtocolAgent {
        fn from(value: ListenPort) -> Self {
            match value {
                ListenPort::A => Self::A,
            }
        }
    }
    pub type InitialKnowledge<T> = (
        Agent<<T as Base>::Agent>,
        Agent<<T as Base>::Agent>,
        AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
        AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
        Inv<
            <T as Base>::AsymmetricKeyInv,
            (AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>),
        >,
    );
    impl<T: Terms, F> State<T, F>
    where
        F: Fn(&mut T, &M0<T>) -> Result<InitialKnowledge<T>>,
    {
        pub fn init(compute_initial_knowledge: F) -> Self {
            State {
                knowledge: Default::default(),
                stage: Default::default(),
                compute_initial_knowledge,
            }
        }
        pub fn handle_message(&mut self, base: &mut T, msg: Ingoing<T>) -> Result<Response<T>> {
            match (&self.stage, &msg) {
                (Stage::M0, Ingoing::M0(_)) => self.stage = Stage::Terminated,
                _ => anyhow::bail!("out of order!"),
            }
            progress(
                base,
                &mut self.compute_initial_knowledge,
                &mut self.knowledge,
                msg,
            )
        }
    }
    #[doc = r" The knowledge required for agent"]
    pub struct Knowledge<T: Terms> {
        #[doc = "@Agent(A)"]
        i0: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i1: Option<Agent<<T as Base>::Agent>>,
        #[doc = "pk(@Agent(A))"]
        i2: Option<AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "pk(@Agent(B))"]
        i3: Option<AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "Inverse(pk(@Agent(B)))"]
        i4: Option<
            Inv<
                <T as Base>::AsymmetricKeyInv,
                (AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>),
            >,
        >,
        #[doc = "{| :(@Agent(A), @Agent(B), @Number(NA)) |}@SymmetricKey(KAB)"]
        i5: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    Agent<<T as Base>::Agent>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                )>,
                SymmetricKey<<T as Base>::SymmetricKey>,
            >,
        >,
        #[doc = "{ :(@Agent(A), @Agent(B), @SymmetricKey(KAB)) }pk(@Agent(B))"]
        i6: Option<
            AsymEnc<
                <T as Base>::AsymEnc,
                Tuple<(
                    Agent<<T as Base>::Agent>,
                    Agent<<T as Base>::Agent>,
                    SymmetricKey<<T as Base>::SymmetricKey>,
                )>,
                AsymmetricKey<<T as Base>::AsymmetricKey, (Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = ":(@Agent(A), @Agent(B), @SymmetricKey(KAB))"]
        i7: Option<
            Tuple<(
                Agent<<T as Base>::Agent>,
                Agent<<T as Base>::Agent>,
                SymmetricKey<<T as Base>::SymmetricKey>,
            )>,
        >,
        #[doc = "@Agent(A)"]
        i8: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i9: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@SymmetricKey(KAB)"]
        i10: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        #[doc = ":(@Agent(A), @Agent(B), @Number(NA))"]
        i11: Option<
            Tuple<(
                Agent<<T as Base>::Agent>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        #[doc = "@Agent(A)"]
        i12: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i13: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Number(NA)"]
        i14: Option<Number<<T as Base>::Number>>,
    }
    impl<T: Terms> Default for Knowledge<T> {
        fn default() -> Self {
            Self {
                i0: None,
                i1: None,
                i2: None,
                i3: None,
                i4: None,
                i5: None,
                i6: None,
                i7: None,
                i8: None,
                i9: None,
                i10: None,
                i11: None,
                i12: None,
                i13: None,
                i14: None,
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "{| :(@Agent(A), @Agent(B), @Number(NA)) |}@SymmetricKey(KAB), { :(@Agent(A), @Agent(B), @SymmetricKey(KAB)) }pk(@Agent(B))"]
        M0(M0<T>),
    }
    impl<T: Terms> From<Ingoing<T>> for Message<T> {
        fn from(v: Ingoing<T>) -> Self {
            match v {
                Ingoing::M0(m) => Self::M0(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M0(m) => Ok(Self::M0(m)),
                _ => Err(()),
            }
        }
    }
    #[doc = r" Outgoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    pub enum Outgoing<T: Terms> {
        #[doc = "@SymmetricKey(KAB), @Number(NA), @Agent(A), @Agent(B)"]
        M1(M1<T>),
    }
    impl<T: Terms> From<Outgoing<T>> for Message<T> {
        fn from(v: Outgoing<T>) -> Self {
            match v {
                Outgoing::M1(m) => Self::M1(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M1(m) => Ok(Self::M1(m)),
                _ => Err(()),
            }
        }
    }
    pub type InitialMsg<T> = M0<T>;
    pub fn listen<T: Terms, C: Channel<Message<T>, Message<T>>>(
        base: &mut T,
        ports: impl Fn(ListenPort) -> C::Addr,
        con: impl Fn(ProtocolAgent, &T::Agent) -> C::Recipient,
        f: impl Fn(&mut T, &InitialMsg<T>) -> Result<InitialKnowledge<T>>,
    ) -> anyhow::Result<()>
    where
        C::Error: Send + Sync + 'static,
    {
        let mut channels = HashMap::<ProtocolAgent, C>::new();
        let mut state = State::<T, _>::init(f);
        let mut recv_on: ProtocolAgent = ProtocolAgent::A;
        loop {
            let channel = if channels.contains_key(&recv_on) {
                channels.get_mut(&recv_on).unwrap()
            } else {
                let channel = C::listen(ports(ListenPort::A))?;
                channels.entry(recv_on).or_insert(channel)
            };
            let msg = channel.receive()?;
            let msg = state.handle_message(base, msg.try_into().unwrap())?;
            match msg.send {
                SendMessage::Nothing => {}
                SendMessage::Send {
                    msg,
                    connect,
                    to: (name, to),
                } => {
                    let c = if connect {
                        let c = C::connect(con(name, &to))?;
                        channels.entry(name).or_insert(c)
                    } else {
                        channels.get_mut(&name).unwrap()
                    };
                    c.send(msg.into())?;
                }
            }
            recv_on = match msg.action {
                NextAction::Terminate => break Ok(()),
                NextAction::RecvFrom(p, c) => p,
                NextAction::ListenOn(p, c) => {
                    let channel = C::listen(ports(p))?;
                    channels.insert(p.into(), channel);
                    p.into()
                }
            };
        }
    }
    fn progress<T: Terms>(
        base: &mut T,
        compute_initial_knowledge: &mut impl Fn(&mut T, &M0<T>) -> Result<InitialKnowledge<T>>,
        knowledge: &mut Knowledge<T>,
        msg: Ingoing<T>,
    ) -> Result<Response<T>> {
        match msg {
            Ingoing::M0(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M0));
                let _enter = scope.enter();
                let (k0, k1, k2, k3, k4) = compute_initial_knowledge(base, &m)?;
                knowledge.i0 = Some(k0);
                knowledge.i1 = Some(k1);
                knowledge.i2 = Some(k2);
                knowledge.i3 = Some(k3);
                knowledge.i4 = Some(k4);
                knowledge.i5 = Some(m.0);
                knowledge.i6 = Some(m.1);
                knowledge.i7 = Some(base.asymmetric_dencrypt(
                    &knowledge.i6.as_ref().unwrap().0,
                    &knowledge.i4.as_ref().unwrap().0,
                )?);
                knowledge.i8 = Some(knowledge.i7.as_ref().unwrap().0 .0.clone());
                assert_eq!(knowledge.i0, knowledge.i8);
                knowledge.i9 = Some(knowledge.i7.as_ref().unwrap().0 .1.clone());
                assert_eq!(knowledge.i1, knowledge.i9);
                knowledge.i10 = Some(knowledge.i7.as_ref().unwrap().0 .2.clone());
                knowledge.i11 = Some(base.symmetric_dencrypt(
                    &knowledge.i5.as_ref().unwrap().0,
                    knowledge.i10.as_ref().unwrap(),
                )?);
                knowledge.i12 = Some(knowledge.i11.as_ref().unwrap().0 .0.clone());
                assert_eq!(knowledge.i0, knowledge.i12);
                knowledge.i13 = Some(knowledge.i11.as_ref().unwrap().0 .1.clone());
                assert_eq!(knowledge.i1, knowledge.i13);
                knowledge.i14 = Some(knowledge.i11.as_ref().unwrap().0 .2.clone());
                Ok(Response {
                    save_connection_as: Some(ProtocolAgent::A),
                    send: SendMessage::Send {
                        msg: Outgoing::M1(M1(
                            knowledge.i10.clone().unwrap(),
                            knowledge.i14.clone().unwrap(),
                            knowledge.i0.clone().unwrap(),
                            knowledge.i1.clone().unwrap(),
                        )),
                        connect: false,
                        to: (ProtocolAgent::A, knowledge.i0.clone().unwrap().0),
                    },
                    action: NextAction::Terminate,
                })
            }
        }
    }
}
