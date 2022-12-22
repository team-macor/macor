#![allow(non_snake_case, non_camel_case_types)]
use anyhow::Result;
use omg::terms::*;
use omg::{Base, Channel};
use std::{collections::HashMap, marker::PhantomData};
pub trait Terms: Base {
    type User_pk: Clone + std::fmt::Debug + serde::Serialize + serde::de::DeserializeOwned;
    fn pk(&mut self, arg_0: &<Self as Base>::Agent) -> <Self as Base>::AsymKey;
}
impl Terms for () {
    type User_pk = ();
    fn pk(&mut self, arg_0: &<Self as Base>::Agent) -> <Self as Base>::AsymKey {}
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
    #[doc = "pk(@Agent(A))"] pub AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
    #[doc = "pk(@Agent(B))"] pub AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
    #[doc = "Inverse(pk(@Agent(A)))"]
    pub  Inv<<T as Base>::AsymKeyInv, (AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>)>,
);
#[doc = "[{ :(@Agent(A), @Agent(B)) }pk(@Agent(B))]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M0<T: Terms>(
    #[doc = "{ :(@Agent(A), @Agent(B)) }pk(@Agent(B))"]
    pub  AsymEnc<
        <T as Base>::AsymEnc,
        Tuple<(Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
    >,
);
#[doc = "[@Agent(A), @Agent(B)]"]
#[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
#[serde(bound = "")]
pub struct M1<T: Terms>(
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
        AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
        AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
        Inv<<T as Base>::AsymKeyInv, (AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>)>,
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
        i2: Option<AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "pk(@Agent(B))"]
        i3: Option<AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "Inverse(pk(@Agent(A)))"]
        i4: Option<
            Inv<
                <T as Base>::AsymKeyInv,
                (AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>),
            >,
        >,
        #[doc = ":(@Agent(A), @Agent(B))"]
        i5: Option<Tuple<(Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>>,
        #[doc = "{ :(@Agent(A), @Agent(B)) }pk(@Agent(B))"]
        i6: Option<
            AsymEnc<
                <T as Base>::AsymEnc,
                Tuple<(Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
                AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = "@Agent(A)"]
        i7: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i8: Option<Agent<<T as Base>::Agent>>,
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
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "@Agent(A), @Agent(B), pk(@Agent(A)), pk(@Agent(B)), Inverse(pk(@Agent(A)))"]
        Initiate(Initiate<T>),
        #[doc = "@Agent(A), @Agent(B)"]
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
        #[doc = "{ :(@Agent(A), @Agent(B)) }pk(@Agent(B))"]
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
                knowledge.i5 = Some(Tuple((
                    knowledge.i0.clone().unwrap(),
                    knowledge.i1.clone().unwrap(),
                )));
                knowledge.i6 = Some(AsymEnc(
                    base.asym_encrypt(
                        knowledge.i5.as_ref().unwrap(),
                        &knowledge.i3.as_ref().unwrap().0,
                    )?,
                    PhantomData,
                ));
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Send {
                        msg: Outgoing::M0(M0(knowledge.i6.clone().unwrap())),
                        connect: true,
                        to: (ProtocolAgent::B, knowledge.i1.clone().unwrap().0),
                    },
                    action: NextAction::RecvFrom(ProtocolAgent::B, knowledge.i1.clone().unwrap().0),
                })
            }
            Ingoing::M1(m) => {
                let scope = tracing::span!(tracing::Level::INFO, stringify!(M1));
                let _enter = scope.enter();
                knowledge.i7 = Some(m.0);
                assert_eq!(knowledge.i0, knowledge.i7);
                knowledge.i8 = Some(m.1);
                assert_eq!(knowledge.i1, knowledge.i8);
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
        AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
        AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
        Inv<<T as Base>::AsymKeyInv, (AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>)>,
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
        i2: Option<AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "pk(@Agent(B))"]
        i3: Option<AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>>,
        #[doc = "Inverse(pk(@Agent(B)))"]
        i4: Option<
            Inv<
                <T as Base>::AsymKeyInv,
                (AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>),
            >,
        >,
        #[doc = "{ :(@Agent(A), @Agent(B)) }pk(@Agent(B))"]
        i5: Option<
            AsymEnc<
                <T as Base>::AsymEnc,
                Tuple<(Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
                AsymKey<<T as Base>::AsymKey, (Agent<<T as Base>::Agent>)>,
            >,
        >,
        #[doc = ":(@Agent(A), @Agent(B))"]
        i6: Option<Tuple<(Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>>,
        #[doc = "@Agent(A)"]
        i7: Option<Agent<<T as Base>::Agent>>,
        #[doc = "@Agent(B)"]
        i8: Option<Agent<<T as Base>::Agent>>,
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
            }
        }
    }
    #[doc = r" Ingoing messages for agent"]
    #[derive(Debug, Clone, serde :: Serialize, serde :: Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        #[doc = "{ :(@Agent(A), @Agent(B)) }pk(@Agent(B))"]
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
        #[doc = "@Agent(A), @Agent(B)"]
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
                knowledge.i6 = Some(base.asym_decrypt(
                    &knowledge.i5.as_ref().unwrap().0,
                    &knowledge.i4.as_ref().unwrap().0,
                )?);
                knowledge.i7 = Some(knowledge.i6.as_ref().unwrap().0 .0.clone());
                assert_eq!(knowledge.i0, knowledge.i7);
                knowledge.i8 = Some(knowledge.i6.as_ref().unwrap().0 .1.clone());
                assert_eq!(knowledge.i1, knowledge.i8);
                Ok(Response {
                    save_connection_as: Some(ProtocolAgent::A),
                    send: SendMessage::Send {
                        msg: Outgoing::M1(M1(
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
