#![allow(non_snake_case, non_camel_case_types)]
use anyhow::Result;
use omg::terms::*;
use omg::{Base, Channel};
use std::{collections::HashMap, marker::PhantomData};
pub trait Terms: Base {
    type User_sk: Clone + std::fmt::Debug + serde::Serialize + serde::de::DeserializeOwned;
}
impl Terms for () {
    type User_sk = ();
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum ProtocolAgent {
    A,
    B,
    s,
}
/// All messages in the protocol
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub enum Message<T: Terms> {
    Initiate(Initiate<T>),
    M0(M0<T>),
    M1(M1<T>),
    M2(M2<T>),
    M3(M3<T>),
    M4(M4<T>),
}
/// [@Agent(A), @Agent(B), #Agent(s), sk(@Agent(B), #Agent(s))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub struct Initiate<T: Terms>(
    /// @Agent(A)
    pub Agent<<T as Base>::Agent>,
    /// @Agent(B)
    pub Agent<<T as Base>::Agent>,
    /// #Agent(s)
    pub Agent<<T as Base>::Agent>,
    /// sk(@Agent(B), #Agent(s))
    pub Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
);
/// [@Agent(A), @Agent(B), @Number(NB)]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub struct M0<T: Terms>(
    /// @Agent(A)
    pub Agent<<T as Base>::Agent>,
    /// @Agent(B)
    pub Agent<<T as Base>::Agent>,
    /// @Number(NB)
    pub Number<<T as Base>::Number>,
);
/// [@Agent(A), @Agent(B), @Number(NA), @Number(NB)]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub struct M1<T: Terms>(
    /// @Agent(A)
    pub Agent<<T as Base>::Agent>,
    /// @Agent(B)
    pub Agent<<T as Base>::Agent>,
    /// @Number(NA)
    pub Number<<T as Base>::Number>,
    /// @Number(NB)
    pub Number<<T as Base>::Number>,
);
/// [{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub struct M2<T: Terms>(
    /// {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s))
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            SymmetricKey<<T as Base>::SymmetricKey>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
        )>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    >,
    /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            SymmetricKey<<T as Base>::SymmetricKey>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
        )>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    >,
);
/// [{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub struct M3<T: Terms>(
    /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
    pub  SymEnc<
        <T as Base>::SymEnc,
        Tuple<(
            SymmetricKey<<T as Base>::SymmetricKey>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
            Agent<<T as Base>::Agent>,
            Number<<T as Base>::Number>,
        )>,
        Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
    >,
);
/// [sk(@Agent(B), #Agent(s))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub struct M4<T: Terms>(
    /// sk(@Agent(B), #Agent(s))
    pub Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
);
pub mod agent_A {
    use super::*;
    /// The state for agent A
    pub struct State<T: Terms, F>
    where
        F: Fn(&mut T, &M0<T>) -> Result<InitialKnowledge<T>>,
    {
        knowledge: Knowledge<T>,
        stage: Stage,
        compute_initial_knowledge: F,
    }
    /// The stage of the protocol for agent A
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

    /// The knowledge required for agent A
    pub struct Knowledge<T: Terms> {
        /// @Agent(A)
        i0: Option<Agent<<T as Base>::Agent>>,
        /// @Agent(B)
        i1: Option<Agent<<T as Base>::Agent>>,
        /// #Agent(s)
        i2: Option<Agent<<T as Base>::Agent>>,
        /// sk(@Agent(A), #Agent(s))
        i3: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        /// @Agent(A)
        i4: Option<Agent<<T as Base>::Agent>>,
        /// @Agent(B)
        i5: Option<Agent<<T as Base>::Agent>>,
        /// @Number(NB)
        i6: Option<Number<<T as Base>::Number>>,
        /// @Number(NA)
        i7: Option<Number<<T as Base>::Number>>,
        /// {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s))
        i8: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymmetricKey<<T as Base>::SymmetricKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
        i9: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymmetricKey<<T as Base>::SymmetricKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        /// :(@SymmetricKey(KAB), @Agent(B), @Number(NA))
        i10: Option<
            Tuple<(
                SymmetricKey<<T as Base>::SymmetricKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        /// @SymmetricKey(KAB)
        i11: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        /// @Agent(B)
        i12: Option<Agent<<T as Base>::Agent>>,
        /// @Number(NA)
        i13: Option<Number<<T as Base>::Number>>,
        /// sk(@Agent(B), #Agent(s))
        i14: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        /// :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh))
        i15: Option<
            Tuple<(
                SymmetricKey<<T as Base>::SymmetricKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        /// @SymmetricKey(KAB)
        i16: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        /// @Agent(A)
        i17: Option<Agent<<T as Base>::Agent>>,
        /// @Number(NB)
        i18: Option<Number<<T as Base>::Number>>,
        /// #Agent(s)
        i19: Option<Agent<<T as Base>::Agent>>,
        /// @Number(Shhh)
        i20: Option<Number<<T as Base>::Number>>,
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
                i18: None,
                i19: None,
                i20: None,
            }
        }
    }
    /// Ingoing messages for A
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        /// @Agent(A), @Agent(B), @Number(NB)
        M0(M0<T>),
        /// {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
        M2(M2<T>),
        /// sk(@Agent(B), #Agent(s))
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
    /// Outgoing messages for A
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(bound = "")]
    pub enum Outgoing<T: Terms> {
        /// @Agent(A), @Agent(B), @Number(NA), @Number(NB)
        M1(M1<T>),
        /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
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
        ports: impl Fn(ListenPort) -> std::net::SocketAddr,
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
                let scope = tracing::span!(tracing::Level::INFO, "M0");
                let _enter = scope.enter();
                // ComputeInitialKnowledgeFrom { number_given: 3, into: [InfoId(0), InfoId(1), InfoId(2), InfoId(3)] }
                let (k0, k1, k2, k3) = compute_initial_knowledge(base, &m)?;
                knowledge.i0 = Some(k0);
                knowledge.i1 = Some(k1);
                knowledge.i2 = Some(k2);
                knowledge.i3 = Some(k3);
                // Retrieve { index: 0, id: InfoId(4) }
                knowledge.i4 = Some(m.0);
                // Compare { trusted: InfoId(0), new: InfoId(4) }
                assert_eq!(knowledge.i0, knowledge.i4);
                // Retrieve { index: 1, id: InfoId(5) }
                knowledge.i5 = Some(m.1);
                // Compare { trusted: InfoId(1), new: InfoId(5) }
                assert_eq!(knowledge.i1, knowledge.i5);
                // Retrieve { index: 2, id: InfoId(6) }
                knowledge.i6 = Some(m.2);
                // Generate(Nonce, InfoId(7))
                knowledge.i7 = Some(Number(base.generate_nonce()));
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
                let scope = tracing::span!(tracing::Level::INFO, "M2");
                let _enter = scope.enter();
                // Retrieve { index: 0, id: InfoId(8) }
                knowledge.i8 = Some(m.0);
                // Retrieve { index: 1, id: InfoId(9) }
                knowledge.i9 = Some(m.1);
                // DecryptSymmetric { term: InfoId(8), key: InfoId(3), into: InfoId(10) }
                knowledge.i10 = Some(base.symmetric_dencrypt(
                    &knowledge.i8.as_ref().unwrap().0,
                    knowledge.i3.as_ref().unwrap(),
                )?);
                // Extract { from: InfoId(10), index: 0, into: InfoId(11) }
                knowledge.i11 = Some(knowledge.i10.as_ref().unwrap().0 .0.clone());
                // Extract { from: InfoId(10), index: 1, into: InfoId(12) }
                knowledge.i12 = Some(knowledge.i10.as_ref().unwrap().0 .1.clone());
                // Compare { trusted: InfoId(1), new: InfoId(12) }
                assert_eq!(knowledge.i1, knowledge.i12);
                // Extract { from: InfoId(10), index: 2, into: InfoId(13) }
                knowledge.i13 = Some(knowledge.i10.as_ref().unwrap().0 .2.clone());
                // Compare { trusted: InfoId(7), new: InfoId(13) }
                assert_eq!(knowledge.i7, knowledge.i13);
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
                let scope = tracing::span!(tracing::Level::INFO, "M4");
                let _enter = scope.enter();
                // Retrieve { index: 0, id: InfoId(14) }
                knowledge.i14 = Some(m.0);
                // DecryptSymmetric { term: InfoId(9), key: InfoId(14), into: InfoId(15) }
                knowledge.i15 = Some(base.symmetric_dencrypt(
                    &knowledge.i9.as_ref().unwrap().0,
                    knowledge.i14.as_ref().unwrap(),
                )?);
                // Extract { from: InfoId(15), index: 0, into: InfoId(16) }
                knowledge.i16 = Some(knowledge.i15.as_ref().unwrap().0 .0.clone());
                // Compare { trusted: InfoId(11), new: InfoId(16) }
                assert_eq!(knowledge.i11, knowledge.i16);
                // Extract { from: InfoId(15), index: 1, into: InfoId(17) }
                knowledge.i17 = Some(knowledge.i15.as_ref().unwrap().0 .1.clone());
                // Compare { trusted: InfoId(0), new: InfoId(17) }
                assert_eq!(knowledge.i0, knowledge.i17);
                // Extract { from: InfoId(15), index: 2, into: InfoId(18) }
                knowledge.i18 = Some(knowledge.i15.as_ref().unwrap().0 .2.clone());
                // Compare { trusted: InfoId(6), new: InfoId(18) }
                assert_eq!(knowledge.i6, knowledge.i18);
                // Extract { from: InfoId(15), index: 3, into: InfoId(19) }
                knowledge.i19 = Some(knowledge.i15.as_ref().unwrap().0 .3.clone());
                // Compare { trusted: InfoId(2), new: InfoId(19) }
                assert_eq!(knowledge.i2, knowledge.i19);
                // Extract { from: InfoId(15), index: 4, into: InfoId(20) }
                knowledge.i20 = Some(knowledge.i15.as_ref().unwrap().0 .4.clone());
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
    /// The state for agent B
    pub struct State<T: Terms> {
        knowledge: Knowledge<T>,
        stage: Stage,
    }
    /// The stage of the protocol for agent B
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

    /// The knowledge required for agent B
    pub struct Knowledge<T: Terms> {
        /// @Agent(A)
        i0: Option<Agent<<T as Base>::Agent>>,
        /// @Agent(B)
        i1: Option<Agent<<T as Base>::Agent>>,
        /// #Agent(s)
        i2: Option<Agent<<T as Base>::Agent>>,
        /// sk(@Agent(B), #Agent(s))
        i3: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        /// @Number(NB)
        i4: Option<Number<<T as Base>::Number>>,
        /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
        i5: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymmetricKey<<T as Base>::SymmetricKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        /// :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh))
        i6: Option<
            Tuple<(
                SymmetricKey<<T as Base>::SymmetricKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        /// @SymmetricKey(KAB)
        i7: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        /// @Agent(A)
        i8: Option<Agent<<T as Base>::Agent>>,
        /// @Number(NB)
        i9: Option<Number<<T as Base>::Number>>,
        /// #Agent(s)
        i10: Option<Agent<<T as Base>::Agent>>,
        /// @Number(Shhh)
        i11: Option<Number<<T as Base>::Number>>,
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
            }
        }
    }
    /// Ingoing messages for B
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        /// @Agent(A), @Agent(B), #Agent(s), sk(@Agent(B), #Agent(s))
        Initiate(Initiate<T>),
        /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
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
    /// Outgoing messages for B
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(bound = "")]
    pub enum Outgoing<T: Terms> {
        /// @Agent(A), @Agent(B), @Number(NB)
        M0(M0<T>),
        /// sk(@Agent(B), #Agent(s))
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
        ports: impl Fn(ListenPort) -> std::net::SocketAddr,
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
                let scope = tracing::span!(tracing::Level::INFO, "Initiate");
                let _enter = scope.enter();
                // Retrieve { index: 0, id: InfoId(0) }
                knowledge.i0 = Some(m.0);
                // Retrieve { index: 1, id: InfoId(1) }
                knowledge.i1 = Some(m.1);
                // Retrieve { index: 2, id: InfoId(2) }
                knowledge.i2 = Some(m.2);
                // Retrieve { index: 3, id: InfoId(3) }
                knowledge.i3 = Some(m.3);
                // Generate(Nonce, InfoId(4))
                knowledge.i4 = Some(Number(base.generate_nonce()));
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
                let scope = tracing::span!(tracing::Level::INFO, "M3");
                let _enter = scope.enter();
                // Retrieve { index: 0, id: InfoId(5) }
                knowledge.i5 = Some(m.0);
                // DecryptSymmetric { term: InfoId(5), key: InfoId(3), into: InfoId(6) }
                knowledge.i6 = Some(base.symmetric_dencrypt(
                    &knowledge.i5.as_ref().unwrap().0,
                    knowledge.i3.as_ref().unwrap(),
                )?);
                // Extract { from: InfoId(6), index: 0, into: InfoId(7) }
                knowledge.i7 = Some(knowledge.i6.as_ref().unwrap().0 .0.clone());
                // Extract { from: InfoId(6), index: 1, into: InfoId(8) }
                knowledge.i8 = Some(knowledge.i6.as_ref().unwrap().0 .1.clone());
                // Compare { trusted: InfoId(0), new: InfoId(8) }
                assert_eq!(knowledge.i0, knowledge.i8);
                // Extract { from: InfoId(6), index: 2, into: InfoId(9) }
                knowledge.i9 = Some(knowledge.i6.as_ref().unwrap().0 .2.clone());
                // Compare { trusted: InfoId(4), new: InfoId(9) }
                assert_eq!(knowledge.i4, knowledge.i9);
                // Extract { from: InfoId(6), index: 3, into: InfoId(10) }
                knowledge.i10 = Some(knowledge.i6.as_ref().unwrap().0 .3.clone());
                // Compare { trusted: InfoId(2), new: InfoId(10) }
                assert_eq!(knowledge.i2, knowledge.i10);
                // Extract { from: InfoId(6), index: 4, into: InfoId(11) }
                knowledge.i11 = Some(knowledge.i6.as_ref().unwrap().0 .4.clone());
                Ok(Response {
                    save_connection_as: None,
                    send: SendMessage::Send {
                        msg: Outgoing::M4(M4(knowledge.i3.clone().unwrap())),
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
    /// The state for agent s
    pub struct State<T: Terms, F>
    where
        F: Fn(&mut T, &M1<T>) -> Result<InitialKnowledge<T>>,
    {
        knowledge: Knowledge<T>,
        stage: Stage,
        compute_initial_knowledge: F,
    }
    /// The stage of the protocol for agent s
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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

    /// The knowledge required for agent s
    pub struct Knowledge<T: Terms> {
        /// @Agent(A)
        i0: Option<Agent<<T as Base>::Agent>>,
        /// @Agent(B)
        i1: Option<Agent<<T as Base>::Agent>>,
        /// #Agent(s)
        i2: Option<Agent<<T as Base>::Agent>>,
        /// sk(@Agent(A), #Agent(s))
        i3: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        /// sk(@Agent(B), #Agent(s))
        i4: Option<
            Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
        >,
        /// @Agent(A)
        i5: Option<Agent<<T as Base>::Agent>>,
        /// @Agent(B)
        i6: Option<Agent<<T as Base>::Agent>>,
        /// @Number(NA)
        i7: Option<Number<<T as Base>::Number>>,
        /// @Number(NB)
        i8: Option<Number<<T as Base>::Number>>,
        /// @SymmetricKey(KAB)
        i9: Option<SymmetricKey<<T as Base>::SymmetricKey>>,
        /// @Number(Shhh)
        i10: Option<Number<<T as Base>::Number>>,
        /// :(@SymmetricKey(KAB), @Agent(B), @Number(NA))
        i11: Option<
            Tuple<(
                SymmetricKey<<T as Base>::SymmetricKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        /// {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s))
        i12: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymmetricKey<<T as Base>::SymmetricKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                )>,
                Func<<T as Terms>::User_sk, (Agent<<T as Base>::Agent>, Agent<<T as Base>::Agent>)>,
            >,
        >,
        /// :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh))
        i13: Option<
            Tuple<(
                SymmetricKey<<T as Base>::SymmetricKey>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
                Agent<<T as Base>::Agent>,
                Number<<T as Base>::Number>,
            )>,
        >,
        /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
        i14: Option<
            SymEnc<
                <T as Base>::SymEnc,
                Tuple<(
                    SymmetricKey<<T as Base>::SymmetricKey>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
                    Agent<<T as Base>::Agent>,
                    Number<<T as Base>::Number>,
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
            }
        }
    }
    /// Ingoing messages for s
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(bound = "")]
    pub enum Ingoing<T: Terms> {
        /// @Agent(A), @Agent(B), @Number(NA), @Number(NB)
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
    /// Outgoing messages for s
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(bound = "")]
    pub enum Outgoing<T: Terms> {
        /// {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
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
        ports: impl Fn(ListenPort) -> std::net::SocketAddr,
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
                let scope = tracing::span!(tracing::Level::INFO, "M1");
                let _enter = scope.enter();
                // ComputeInitialKnowledgeFrom { number_given: 4, into: [InfoId(0), InfoId(1), InfoId(2), InfoId(3), InfoId(4)] }
                let (k0, k1, k2, k3, k4) = compute_initial_knowledge(base, &m)?;
                knowledge.i0 = Some(k0);
                knowledge.i1 = Some(k1);
                knowledge.i2 = Some(k2);
                knowledge.i3 = Some(k3);
                knowledge.i4 = Some(k4);
                // Retrieve { index: 0, id: InfoId(5) }
                knowledge.i5 = Some(m.0);
                // Compare { trusted: InfoId(0), new: InfoId(5) }
                assert_eq!(knowledge.i0, knowledge.i5);
                // Retrieve { index: 1, id: InfoId(6) }
                knowledge.i6 = Some(m.1);
                // Compare { trusted: InfoId(1), new: InfoId(6) }
                assert_eq!(knowledge.i1, knowledge.i6);
                // Retrieve { index: 2, id: InfoId(7) }
                knowledge.i7 = Some(m.2);
                // Retrieve { index: 3, id: InfoId(8) }
                knowledge.i8 = Some(m.3);
                // Generate(SymKey, InfoId(9))
                knowledge.i9 = Some(SymmetricKey(base.generate_sym_key()));
                // Generate(Nonce, InfoId(10))
                knowledge.i10 = Some(Number(base.generate_nonce()));
                // CreateTuple { from: [InfoId(9), InfoId(1), InfoId(7)], into: InfoId(11) }
                knowledge.i11 = Some(Tuple((
                    knowledge.i9.clone().unwrap(),
                    knowledge.i1.clone().unwrap(),
                    knowledge.i7.clone().unwrap(),
                )));
                // SymEnc { body: InfoId(11), key: InfoId(3), into: InfoId(12) }
                knowledge.i12 = Some(SymEnc(
                    base.symmetric_encrypt(
                        knowledge.i11.as_ref().unwrap(),
                        knowledge.i3.as_ref().unwrap(),
                    )?,
                    PhantomData,
                ));
                // CreateTuple { from: [InfoId(9), InfoId(0), InfoId(8), InfoId(2), InfoId(10)], into: InfoId(13) }
                knowledge.i13 = Some(Tuple((
                    knowledge.i9.clone().unwrap(),
                    knowledge.i0.clone().unwrap(),
                    knowledge.i8.clone().unwrap(),
                    knowledge.i2.clone().unwrap(),
                    knowledge.i10.clone().unwrap(),
                )));
                // SymEnc { body: InfoId(13), key: InfoId(4), into: InfoId(14) }
                knowledge.i14 = Some(SymEnc(
                    base.symmetric_encrypt(
                        knowledge.i13.as_ref().unwrap(),
                        knowledge.i4.as_ref().unwrap(),
                    )?,
                    PhantomData,
                ));
                Ok(Response {
                    save_connection_as: Some(ProtocolAgent::A),
                    send: SendMessage::Send {
                        msg: Outgoing::M2(M2(
                            knowledge.i12.clone().unwrap(),
                            knowledge.i14.clone().unwrap(),
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
