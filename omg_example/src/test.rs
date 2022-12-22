#![allow(non_snake_case, non_camel_case_types)]
use anyhow::Result;
use omg::terms::*;
use omg::{Base, Channel};
use std::{collections::HashMap, marker::PhantomData};
pub trait Terms: Base {
    type User_sk: Clone + std::fmt::Debug + serde::Serialize + serde::de::DeserializeOwned;
    fn sk(&mut self, arg_0: &<Self as Base>::Agent, arg_1: &<Self as Base>::Agent)
        -> Self::User_sk;
}
impl Terms for () {
    type User_sk = ();
    fn sk(
        &mut self,
        arg_0: &<Self as Base>::Agent,
        arg_1: &<Self as Base>::Agent,
    ) -> Self::User_sk {
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde :: Serialize, serde :: Deserialize)]
pub enum ProtocolAgent {
    A,
    B,
    s,
}
#[doc = r" All messages in the protocol"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub enum Message<T: Terms> {
    Initiate(Initiate<T>),
    M0(M0<T>),
    M1(M1<T>),
    M2(M2<T>),
    M3(M3<T>),
    M4(M4<T>),
}
#[doc = "[@Agent(A), @Agent(B), #Agent(s), sk(@Agent(B), #Agent(s))]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct Initiate<T: Terms>(
    #[doc = "@Agent(A)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Agent(B)"] pub Agent<<T as Base>::Agent>,
    #[doc = "#Agent(s)"] pub Agent<<T as Base>::Agent>,
    #[doc = "sk(@Agent(B), #Agent(s))"]
    pub  Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
);
#[doc = "[@Agent(A), @Agent(B), @Number(NB)]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M0<T: Terms>(
    #[doc = "@Agent(A)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Agent(B)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Number(NB)"] pub Number<<T as Base>::Number>,
);
#[doc = "[@Agent(A), @Agent(B), @Number(NA), @Number(NB)]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M1<T: Terms>(
    #[doc = "@Agent(A)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Agent(B)"] pub Agent<<T as Base>::Agent>,
    #[doc = "@Number(NA)"] pub Number<<T as Base>::Number>,
    #[doc = "@Number(NB)"] pub Number<<T as Base>::Number>,
);
#[doc = "[{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M2<T: Terms>(
    #[doc = "{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2)) |}sk(@Agent(A), #Agent(s))"]
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            SymKey<<T as Base>::SymKey>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(Number<<T as Base>::Number>,)>,
                SymKey<<T as Base>::SymKey>,
            >,
        )>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    >,
    #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            SymKey<<T as Base>::SymKey>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
            SymKey<<T as Base>::SymKey>,
        )>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    >,
);
#[doc = "[{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M3<T: Terms>(
    #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            SymKey<<T as Base>::SymKey>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
            SymKey<<T as Base>::SymKey>,
        )>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    >,
);
#[doc = "[@SymmetricKey(K2)]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M4<T: Terms>(#[doc = "@SymmetricKey(K2)"] pub SymKey<<T as Base>::SymKey>);
pub mod agent_A {
    use super::*;
    #[doc = "The state for agent A"]
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
        M2,
        M4,
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
        B,
    }
    impl From<ListenPort> for ProtocolAgent {
        fn from(value: ListenPort) -> Self {
            match value {
                ListenPort::B => Self::B,
            }
        }
    }
    pub type InitialKnowledge<T> = (
        Agent<<T as Base>::Agent>,
        Agent<<T as Base>::Agent>,
        Agent<<T as Base>::Agent>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
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
                (Stage::M0, Ingoing::M0(_)) => self.stage = Stage::M2,
                (Stage::M2, Ingoing::M2(_)) => self.stage = Stage::M4,
                (Stage::M4, Ingoing::M4(_)) => self.stage = Stage::Terminated,
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
        #[doc = "#Agent(s)"]
        i2: Option<Agent<<T as Base>::Agent>>,
        #[doc = "sk(@Agent(A), #Agent(s))"]
        i3: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        #[doc = "@Agent(A)"]
        i4: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i5: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Number(NB)"]
        i6: Option<Number<<T as Base>::Number>>,
        #[doc = "@Number(NA)"]
        i7: Option<Number<<T as Base>::Number>>,
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2)) |}sk(@Agent(A), #Agent(s))"]
        i8: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymKey<<T as Base>::SymKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    SymEnc<
                        <T as Base>::SymEnc,
                        Tuple<(Number<<T as Base>::Number>,)>,
                        SymKey<<T as Base>::SymKey>,
                    >,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        i9: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymKey<<T as Base>::SymKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    SymKey<<T as Base>::SymKey>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = ":(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2))"]
        i10: Option<
            Tuple<(
                SymKey<<T as Base>::SymKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                SymEnc<
                    <T as Base>::SymEnc,
                    Tuple<(Number<<T as Base>::Number>,)>,
                    SymKey<<T as Base>::SymKey>,
                >,
            )>,
        >,
        #[doc = "@SymmetricKey(KAB)"]
        i11: Option<SymKey<<T as Base>::SymKey>>,
        #[doc = "@Agent(B)"]
        i12: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Number(NA)"]
        i13: Option<Number<<T as Base>::Number>>,
        #[doc = "{| :(@Number(Delayed)) |}@SymmetricKey(K2)"]
        i14: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(Number<<T as Base>::Number>,)>,
                SymKey<<T as Base>::SymKey>,
            >,
        >,
        #[doc = "@SymmetricKey(K2)"]
        i15: Option<SymKey<<T as Base>::SymKey>>,
        #[doc = ":(@Number(Delayed))"]
        i16: Option<Tuple<(Number<<T as Base>::Number>,)>>,
        #[doc = "@Number(Delayed)"]
        i17: Option<Number<<T as Base>::Number>>,
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
                i15: None,
                i16: None,
                i17: None,
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), @Number(NB)"]
        M0(M0<T>),
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        M2(M2<T>),
        #[doc = "@SymmetricKey(K2)"]
        M4(M4<T>),
    }
    impl<T: Terms> From<Ingoing<T>> for Message<T> {
        fn from(v: Ingoing<T>) -> Self {
            match v {
                Ingoing::M0(m) => Self::M0(m),
                Ingoing::M2(m) => Self::M2(m),
                Ingoing::M4(m) => Self::M4(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M0(m) => Ok(Self::M0(m)),
                Message::M2(m) => Ok(Self::M2(m)),
                Message::M4(m) => Ok(Self::M4(m)),
                _ => Err(()),
            }
        }
    }
    #[doc = r" Outgoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    pub enum Outgoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), @Number(NA), @Number(NB)"]
        M1(M1<T>),
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        M3(M3<T>),
    }
    impl<T: Terms> From<Outgoing<T>> for Message<T> {
        fn from(v: Outgoing<T>) -> Self {
            match v {
                Outgoing::M1(m) => Self::M1(m),
                Outgoing::M3(m) => Self::M3(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M1(m) => Ok(Self::M1(m)),
                Message::M3(m) => Ok(Self::M3(m)),
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
        let mut recv_on: ProtocolAgent = ProtocolAgent::B;
        loop {
            let channel = if channels.contains_key(&recv_on) {
                channels.get_mut(&recv_on).unwrap()
            } else {
                let channel = C::listen(ports(ListenPort::B))?;
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
                let (k0, k1, k2, k3) = compute_initial_knowledge(base, &m)?;
                knowledge.i0 = Some(k0);
                knowledge.i1 = Some(k1);
                knowledge.i2 = Some(k2);
                knowledge.i3 = Some(k3);
                knowledge.i4 = Some(m.0);
                assert_eq!(knowledge.i0, knowledge.i4);
                knowledge.i5 = Some(m.1);
                assert_eq!(knowledge.i1, knowledge.i5);
                knowledge.i6 = Some(m.2);
                knowledge.i7 = Some(Number(base.gen_nonce()));
                Ok(Response {
                    save_connection_as: Some(ProtocolAgent::B),
                    send: SendMessage::Send {
                        msg: Outgoing::M1(M1(
                            knowledge.i0.clone().unwrap(),
                            knowledge.i1.clone().unwrap(),
                            knowledge.i7.clone().unwrap(),
                            knowledge.i6.clone().unwrap(),
                        )),
                        connect: true,
                        to: (ProtocolAgent::s, knowledge.i2.clone().unwrap().0),
                    },
                    action: NextAction::RecvFrom(ProtocolAgent::s, knowledge.i2.clone().unwrap().0),
                })
            }
            Ingoing::M2(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M2));
                let _enter = scope.enter();
                knowledge.i8 = Some(m.0);
                knowledge.i9 = Some(m.1);
                knowledge.i10 = Some(base.sym_decrypt(
                    &knowledge.i8.as_ref().unwrap().0,
                    knowledge.i3.as_ref().unwrap(),
                )?);
                knowledge.i11 = Some(knowledge.i10.as_ref().unwrap().0 .0.clone());
                knowledge.i12 = Some(knowledge.i10.as_ref().unwrap().0 .1.clone());
                assert_eq!(knowledge.i1, knowledge.i12);
                knowledge.i13 = Some(knowledge.i10.as_ref().unwrap().0 .2.clone());
                assert_eq!(knowledge.i7, knowledge.i13);
                knowledge.i14 = Some(knowledge.i10.as_ref().unwrap().0 .3.clone());
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Send {
                        msg: Outgoing::M3(M3(knowledge.i9.clone().unwrap())),
                        connect: false,
                        to: (ProtocolAgent::B, knowledge.i1.clone().unwrap().0),
                    },
                    action: NextAction::RecvFrom(ProtocolAgent::B, knowledge.i1.clone().unwrap().0),
                })
            }
            Ingoing::M4(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M4));
                let _enter = scope.enter();
                knowledge.i15 = Some(m.0);
                knowledge.i16 = Some(base.sym_decrypt(
                    &knowledge.i14.as_ref().unwrap().0,
                    knowledge.i15.as_ref().unwrap(),
                )?);
                knowledge.i17 = Some(knowledge.i16.as_ref().unwrap().0 .0.clone());
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
    pub struct State<T: Terms> {
        knowledge: Knowledge<T>,
        stage: Stage,
    }
    #[doc = r" The stage of the protocol for agent"]
    pub enum Stage {
        Initiate,
        M3,
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
        Agent<<T as Base>::Agent>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
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
                (Stage::Initiate, Ingoing::Initiate(_)) => self.stage = Stage::M3,
                (Stage::M3, Ingoing::M3(_)) => self.stage = Stage::Terminated,
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
        #[doc = "#Agent(s)"]
        i2: Option<Agent<<T as Base>::Agent>>,
        #[doc = "sk(@Agent(B), #Agent(s))"]
        i3: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        #[doc = "@Number(NB)"]
        i4: Option<Number<<T as Base>::Number>>,
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        i5: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymKey<<T as Base>::SymKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    SymKey<<T as Base>::SymKey>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = ":(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2))"]
        i6: Option<
            Tuple<(
                SymKey<<T as Base>::SymKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                SymKey<<T as Base>::SymKey>,
            )>,
        >,
        #[doc = "@SymmetricKey(KAB)"]
        i7: Option<SymKey<<T as Base>::SymKey>>,
        #[doc = "@Agent(A)"]
        i8: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Number(NB)"]
        i9: Option<Number<<T as Base>::Number>>,
        #[doc = "@SymmetricKey(K2)"]
        i10: Option<SymKey<<T as Base>::SymKey>>,
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
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), #Agent(s), sk(@Agent(B), #Agent(s))"]
        Initiate(Initiate<T>),
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        M3(M3<T>),
    }
    impl<T: Terms> From<Ingoing<T>> for Message<T> {
        fn from(v: Ingoing<T>) -> Self {
            match v {
                Ingoing::Initiate(m) => Self::Initiate(m),
                Ingoing::M3(m) => Self::M3(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::Initiate(m) => Ok(Self::Initiate(m)),
                Message::M3(m) => Ok(Self::M3(m)),
                _ => Err(()),
            }
        }
    }
    #[doc = r" Outgoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    pub enum Outgoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), @Number(NB)"]
        M0(M0<T>),
        #[doc = "@SymmetricKey(K2)"]
        M4(M4<T>),
    }
    impl<T: Terms> From<Outgoing<T>> for Message<T> {
        fn from(v: Outgoing<T>) -> Self {
            match v {
                Outgoing::M0(m) => Self::M0(m),
                Outgoing::M4(m) => Self::M4(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M0(m) => Ok(Self::M0(m)),
                Message::M4(m) => Ok(Self::M4(m)),
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
                knowledge.i4 = Some(Number(base.gen_nonce()));
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Send {
                        msg: Outgoing::M0(M0(
                            knowledge.i0.clone().unwrap(),
                            knowledge.i1.clone().unwrap(),
                            knowledge.i4.clone().unwrap(),
                        )),
                        connect: true,
                        to: (ProtocolAgent::A, knowledge.i0.clone().unwrap().0),
                    },
                    action: NextAction::RecvFrom(ProtocolAgent::A, knowledge.i0.clone().unwrap().0),
                })
            }
            Ingoing::M3(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M3));
                let _enter = scope.enter();
                knowledge.i5 = Some(m.0);
                knowledge.i6 = Some(base.sym_decrypt(
                    &knowledge.i5.as_ref().unwrap().0,
                    knowledge.i3.as_ref().unwrap(),
                )?);
                knowledge.i7 = Some(knowledge.i6.as_ref().unwrap().0 .0.clone());
                knowledge.i8 = Some(knowledge.i6.as_ref().unwrap().0 .1.clone());
                assert_eq!(knowledge.i0, knowledge.i8);
                knowledge.i9 = Some(knowledge.i6.as_ref().unwrap().0 .2.clone());
                assert_eq!(knowledge.i4, knowledge.i9);
                knowledge.i10 = Some(knowledge.i6.as_ref().unwrap().0 .3.clone());
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Send {
                        msg: Outgoing::M4(M4(knowledge.i10.clone().unwrap())),
                        connect: false,
                        to: (ProtocolAgent::A, knowledge.i0.clone().unwrap().0),
                    },
                    action: NextAction::Terminate,
                })
            }
        }
    }
}
pub mod agent_s {
    use super::*;
    #[doc = "The state for agent s"]
    pub struct State<T: Terms, F>
    where
        F: Fn(&mut T, &M1<T>) -> Result<InitialKnowledge<T>>,
    {
        knowledge: Knowledge<T>,
        stage: Stage,
        compute_initial_knowledge: F,
    }
    #[doc = r" The stage of the protocol for agent"]
    pub enum Stage {
        M1,
        Terminated,
    }
    impl Default for Stage {
        fn default() -> Self {
            Self::M1
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
        Agent<<T as Base>::Agent>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    );
    impl<T: Terms, F> State<T, F>
    where
        F: Fn(&mut T, &M1<T>) -> Result<InitialKnowledge<T>>,
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
                (Stage::M1, Ingoing::M1(_)) => self.stage = Stage::Terminated,
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
        #[doc = "#Agent(s)"]
        i2: Option<Agent<<T as Base>::Agent>>,
        #[doc = "sk(@Agent(A), #Agent(s))"]
        i3: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        #[doc = "sk(@Agent(B), #Agent(s))"]
        i4: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        #[doc = "@Agent(A)"]
        i5: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i6: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Number(NA)"]
        i7: Option<Number<<T as Base>::Number>>,
        #[doc = "@Number(NB)"]
        i8: Option<Number<<T as Base>::Number>>,
        #[doc = "@SymmetricKey(KAB)"]
        i9: Option<SymKey<<T as Base>::SymKey>>,
        #[doc = "@Number(Delayed)"]
        i10: Option<Number<<T as Base>::Number>>,
        #[doc = "@SymmetricKey(K2)"]
        i11: Option<SymKey<<T as Base>::SymKey>>,
        #[doc = ":(@Number(Delayed))"]
        i12: Option<Tuple<(Number<<T as Base>::Number>,)>>,
        #[doc = "{| :(@Number(Delayed)) |}@SymmetricKey(K2)"]
        i13: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(Number<<T as Base>::Number>,)>,
                SymKey<<T as Base>::SymKey>,
            >,
        >,
        #[doc = ":(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2))"]
        i14: Option<
            Tuple<(
                SymKey<<T as Base>::SymKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                SymEnc<
                    <T as Base>::SymEnc,
                    Tuple<(Number<<T as Base>::Number>,)>,
                    SymKey<<T as Base>::SymKey>,
                >,
            )>,
        >,
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2)) |}sk(@Agent(A), #Agent(s))"]
        i15: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymKey<<T as Base>::SymKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    SymEnc<
                        <T as Base>::SymEnc,
                        Tuple<(Number<<T as Base>::Number>,)>,
                        SymKey<<T as Base>::SymKey>,
                    >,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = ":(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2))"]
        i16: Option<
            Tuple<(
                SymKey<<T as Base>::SymKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                SymKey<<T as Base>::SymKey>,
            )>,
        >,
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        i17: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymKey<<T as Base>::SymKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    SymKey<<T as Base>::SymKey>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
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
                i15: None,
                i16: None,
                i17: None,
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), @Number(NA), @Number(NB)"]
        M1(M1<T>),
    }
    impl<T: Terms> From<Ingoing<T>> for Message<T> {
        fn from(v: Ingoing<T>) -> Self {
            match v {
                Ingoing::M1(m) => Self::M1(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M1(m) => Ok(Self::M1(m)),
                _ => Err(()),
            }
        }
    }
    #[doc = r" Outgoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    pub enum Outgoing<T: Terms> {
        #[doc = "{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA), {| :(@Number(Delayed)) |}@SymmetricKey(K2)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), @SymmetricKey(K2)) |}sk(@Agent(B), #Agent(s))"]
        M2(M2<T>),
    }
    impl<T: Terms> From<Outgoing<T>> for Message<T> {
        fn from(v: Outgoing<T>) -> Self {
            match v {
                Outgoing::M2(m) => Self::M2(m),
            }
        }
    }
    impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {
        type Error = ();
        fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
            match v {
                Message::M2(m) => Ok(Self::M2(m)),
                _ => Err(()),
            }
        }
    }
    pub type InitialMsg<T> = M1<T>;
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
        compute_initial_knowledge: &mut impl Fn(&mut T, &M1<T>) -> Result<InitialKnowledge<T>>,
        knowledge: &mut Knowledge<T>,
        msg: Ingoing<T>,
    ) -> Result<Response<T>> {
        match msg {
            Ingoing::M1(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M1));
                let _enter = scope.enter();
                let (k0, k1, k2, k3, k4) = compute_initial_knowledge(base, &m)?;
                knowledge.i0 = Some(k0);
                knowledge.i1 = Some(k1);
                knowledge.i2 = Some(k2);
                knowledge.i3 = Some(k3);
                knowledge.i4 = Some(k4);
                knowledge.i5 = Some(m.0);
                assert_eq!(knowledge.i0, knowledge.i5);
                knowledge.i6 = Some(m.1);
                assert_eq!(knowledge.i1, knowledge.i6);
                knowledge.i7 = Some(m.2);
                knowledge.i8 = Some(m.3);
                knowledge.i9 = Some(SymKey(base.gen_sym_key()));
                knowledge.i10 = Some(Number(base.gen_nonce()));
                knowledge.i11 = Some(SymKey(base.gen_sym_key()));
                knowledge.i12 = Some(Tuple((knowledge.i10.clone().unwrap(),)));
                knowledge.i13 = Some(SymEnc(
                    base.sym_encrypt(
                        knowledge.i12.as_ref().unwrap(),
                        knowledge.i11.as_ref().unwrap(),
                    )?,
                    PhantomData,
                ));
                knowledge.i14 = Some(Tuple((
                    knowledge.i9.clone().unwrap(),
                    knowledge.i1.clone().unwrap(),
                    knowledge.i7.clone().unwrap(),
                    knowledge.i13.clone().unwrap(),
                )));
                knowledge.i15 = Some(SymEnc(
                    base.sym_encrypt(
                        knowledge.i14.as_ref().unwrap(),
                        knowledge.i3.as_ref().unwrap(),
                    )?,
                    PhantomData,
                ));
                knowledge.i16 = Some(Tuple((
                    knowledge.i9.clone().unwrap(),
                    knowledge.i0.clone().unwrap(),
                    knowledge.i8.clone().unwrap(),
                    knowledge.i11.clone().unwrap(),
                )));
                knowledge.i17 = Some(SymEnc(
                    base.sym_encrypt(
                        knowledge.i16.as_ref().unwrap(),
                        knowledge.i4.as_ref().unwrap(),
                    )?,
                    PhantomData,
                ));
                Ok(Response {
                    save_connection_as: Some(ProtocolAgent::A),
                    send: SendMessage::Send {
                        msg: Outgoing::M2(M2(
                            knowledge.i15.clone().unwrap(),
                            knowledge.i17.clone().unwrap(),
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
