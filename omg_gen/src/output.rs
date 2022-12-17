use itertools::Itertools;
use macor::protocol::{Func, Term};

use crate::v2::{
    AgentModel, GenerateTy, Instruction, MessageThingy, NextAction, ProtocolModel, SendMessage,
};

pub fn term_to_rust_ty(t: &Term) -> String {
    match t {
        Term::Variable(v) => match v {
            macor::protocol::Variable::Agent(_) => "Agent<<T as Base>::Agent>".to_string(),
            macor::protocol::Variable::SymmetricKey(_) => {
                "SymmetricKey<<T as Base>::SymmetricKey>".to_string()
            }
            macor::protocol::Variable::Number(_) => "Number<<T as Base>::Number>".to_string(),
        },
        Term::Constant(c) => match c {
            macor::protocol::Constant::Intruder => unreachable!(),
            macor::protocol::Constant::Agent(_) => "Agent<<T as Base>::Agent>".to_string(),
            macor::protocol::Constant::Function(_) => todo!(),
            macor::protocol::Constant::Nonce(_) => todo!(),
        },
        Term::Composition { func, args } => match func {
            Func::SymEnc => format!(
                "SymEnc<<T as Base>::SymEnc, {}, {}>",
                term_to_rust_ty(&args[0]),
                term_to_rust_ty(&args[1])
            ),
            Func::AsymEnc => todo!(),
            Func::Exp => todo!(),
            Func::Inv => todo!(),
            Func::User(name) => format!(
                "Func<<T as Terms>::User_{name}, ({})>",
                args.iter().map(term_to_rust_ty).format(", ")
            ),
        },
        Term::Tuple(ts) => format!("Tuple<({})>", ts.iter().map(term_to_rust_ty).format(", ")),
    }
}

impl ProtocolModel {
    pub fn rust(&self) -> anyhow::Result<String> {
        use std::fmt::Write;

        let mut buf = String::new();

        writeln!(buf, "#![allow(non_snake_case, non_camel_case_types)]")?;
        writeln!(
            buf,
            "use std::{{collections::HashMap, marker::PhantomData}};"
        )?;
        writeln!(buf, "use anyhow::Result;")?;
        writeln!(buf, "use omg::{{Base, Channel}};")?;
        writeln!(buf, "use omg::terms::*;")?;

        writeln!(buf, "pub trait Terms: Base {{")?;
        for ty in &self.instance.types {
            // TODO: Only include public functions
            writeln!(
                buf,
                "\ttype User_{}: Clone + std::fmt::Debug + serde::Serialize + serde::de::DeserializeOwned;",
                ty.name
            )?;
            // writeln!(buf, "\t{};", omg.fn_signature(name.as_str(), args))?;
        }
        writeln!(buf, "}}")?;

        writeln!(buf, "impl Terms for () {{")?;
        for ty in &self.instance.types {
            writeln!(buf, "\ttype User_{} = ();", ty.name)?;
            // writeln!(buf, "\t{} {{ () }}", omg.fn_signature(name.as_str(), args))?;
        }
        writeln!(buf, "}}")?;

        writeln!(
            buf,
            "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]"
        )?;
        writeln!(buf, "pub enum ProtocolAgent {{")?;
        for a in &self.agents {
            writeln!(buf, "\t{},", a.name)?;
        }
        writeln!(buf, "}}")?;

        writeln!(buf, "/// All messages in the protocol")?;
        writeln!(
            buf,
            "#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]"
        )?;
        writeln!(buf, "#[serde(bound = \"\")]")?;
        writeln!(buf, "pub enum Message<T: Terms> {{",)?;
        for (id, _) in &self.messages {
            writeln!(buf, "{id}({id}<T>),")?;
        }
        writeln!(buf, "}}")?;
        for (id, terms) in &self.messages {
            writeln!(buf, "/// {:?}", terms)?;
            writeln!(
                buf,
                "#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]"
            )?;
            writeln!(buf, "#[serde(bound = \"\")]")?;
            writeln!(
                buf,
                "pub struct {id}<T: Terms>({});",
                terms
                    .iter()
                    .map(|t| format!("/// {t:?}\npub {}", term_to_rust_ty(t)))
                    .format(", ")
            )?;
        }

        for a in &self.agents {
            writeln!(buf, "{}", a.rust()?)?;
        }

        Ok(buf)
    }
}

impl AgentModel {
    fn compute_initial_knowledge_fn_ty(&self) -> Option<String> {
        if let Some(iik) = &self.initialize_knowledge_from_first_message {
            Some(format!(
                "Fn(&mut T, &{}<T>) -> Result<InitialKnowledge<T>>",
                iik.from_msg,
                // iik.from_types
                //     .iter()
                //     .map(term_to_rust_ty)
                //     .map(|t| format!("&{t}"))
                //     .format(", "),
                // iik.to_types.iter().map(term_to_rust_ty).format(", "),
            ))
        } else {
            None
        }
    }

    fn rust(&self) -> anyhow::Result<String> {
        use std::fmt::Write;

        let mut buf = String::new();

        writeln!(buf, "pub mod agent_{} {{", self.name)?;
        writeln!(buf, "use super::*;")?;

        // State

        if let Some(ty) = self.compute_initial_knowledge_fn_ty() {
            writeln!(buf, "/// The state for agent {}", self.name)?;
            writeln!(buf, "pub struct State<T: Terms, F> where F: {ty} {{")?;
            writeln!(buf, " knowledge: Knowledge<T>,")?;
            writeln!(buf, " stage: Stage,")?;
            writeln!(buf, " compute_initial_knowledge: F,")?;
            writeln!(buf, "}}")?;
        } else {
            writeln!(buf, "/// The state for agent {}", self.name)?;
            writeln!(buf, "pub struct State<T: Terms>  {{")?;
            writeln!(buf, " knowledge: Knowledge<T>,")?;
            writeln!(buf, " stage: Stage,")?;
            writeln!(buf, "}}")?;
        }

        writeln!(buf, "/// The stage of the protocol for agent {}", self.name)?;
        writeln!(buf, "pub enum Stage {{")?;
        for n in &self.stage_names {
            writeln!(buf, "  {n},")?;
        }
        writeln!(buf, "}}")?;
        writeln!(
            buf,
            "impl Default for Stage {{ fn default() -> Self {{ Self::{} }} }}",
            self.ingoing.first().unwrap().0
        )?;

        writeln!(
            buf,
            r#"
            #[derive(Debug)]
            pub enum SendMessage<T: Terms> {{
                Nothing,
                Send {{
                    msg: Outgoing<T>,
                    connect: bool,
                    to: (ProtocolAgent, T::Agent),
                }},
            }}
            #[derive(Debug)]
            pub enum NextAction<T: Terms> {{
                Terminate,
                RecvFrom(ProtocolAgent, T::Agent),
                ListenOn(ListenPort, T::Agent),
            }}
            #[derive(Debug)]
            pub struct Response<T: Terms> {{
                pub save_connection_as: Option<ProtocolAgent>,
                pub send: SendMessage<T>,
                pub action: NextAction<T>,
            }}
            "#
        )?;

        writeln!(
            buf,
            "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]"
        )?;
        writeln!(buf, "pub enum ListenPort {{")?;
        for a in &self.listen_ports {
            writeln!(buf, "{a},")?;
        }
        writeln!(buf, "}}")?;
        writeln!(buf, "impl From<ListenPort> for ProtocolAgent {{")?;
        writeln!(buf, "fn from(value: ListenPort) -> Self {{")?;
        writeln!(buf, "match value {{")?;
        for a in &self.listen_ports {
            writeln!(buf, "ListenPort::{a} => Self::{a},")?;
        }
        writeln!(buf, "}}")?;
        writeln!(buf, "}}")?;
        writeln!(buf, "}}")?;

        write!(
            buf,
            "pub type InitialKnowledge<T> = ({});",
            self.initial_knowledge
                .iter()
                .map(|(_, t)| term_to_rust_ty(t))
                .format(", ")
        )?;

        let stage_transition = format!(
            r#"
            match (&self.stage, &msg) {{
                {}
                _ => anyhow::bail!("out of order!"),
            }}
            "#,
            self.stage_names
                .iter()
                .zip(self.stage_names.iter().skip(1))
                .map(|(id, next)| {
                    format!("(Stage::{id}, Ingoing::{id}(_)) => self.stage = Stage::{next},")
                })
                .format("\n")
        );

        if let Some(ty) = self.compute_initial_knowledge_fn_ty() {
            writeln!(
                buf,
                r#"
impl<T: Terms, F> State<T, F>
where
    F: {ty},
{{
    pub fn init(compute_initial_knowledge: F) -> Self {{
        State {{
            knowledge: Default::default(),
            stage: Default::default(),
            compute_initial_knowledge,
        }}
    }}
    pub fn handle_message(&mut self, base: &mut T, msg: Ingoing<T>) -> Result<Response<T>> {{
        {stage_transition}

        progress(
            base,
            &mut self.compute_initial_knowledge,
            &mut self.knowledge,
            msg,
        )
    }}
}}
"#
            )?;
        } else {
            writeln!(
                buf,
                r#"
impl<T: Terms> State<T> {{
    pub fn init() -> Self {{
        State {{
            knowledge: Default::default(),
            stage: Default::default(),
        }}
    }}
    pub fn handle_message(&mut self, base: &mut T, msg: Ingoing<T>) -> Result<Response<T>> {{
        {stage_transition}

        progress(
            base,
            &mut self.knowledge,
            msg,
        )
    }}
}}
"#
            )?;
        }

        // Registers

        writeln!(buf, "/// The knowledge required for agent {}", self.name)?;
        writeln!(buf, "pub struct Knowledge<T: Terms> {{")?;

        for (i, t) in &self.registers {
            writeln!(buf, "/// {:?}", t)?;
            writeln!(buf, "{}: Option<{}>,", i, term_to_rust_ty(t))?;
        }

        writeln!(buf, "}}")?;
        writeln!(buf, "impl<T: Terms> Default for Knowledge<T> {{")?;
        writeln!(buf, "  fn default() -> Self {{")?;
        writeln!(buf, "     Self {{")?;

        for (i, _) in &self.registers {
            writeln!(buf, "    {}: None,", i)?;
        }

        writeln!(buf, "    }}")?;
        writeln!(buf, "  }}")?;
        writeln!(buf, "}}")?;

        // Ingoing

        writeln!(buf, "/// Ingoing messages for {}", self.name)?;
        writeln!(
            buf,
            "#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]"
        )?;
        writeln!(buf, "#[serde(bound = \"\")]")?;
        writeln!(buf, "pub enum Ingoing<T: Terms> {{")?;
        for (m, ts) in &self.ingoing {
            writeln!(buf, "/// {:?}", ts.iter().format(", "))?;
            writeln!(buf, "{m}({m}<T>),",)?;
        }
        writeln!(buf, "}}")?;
        writeln!(buf, "impl<T: Terms> From<Ingoing<T>> for Message<T> {{")?;
        writeln!(buf, "  fn from(v: Ingoing<T>) -> Self {{")?;
        writeln!(buf, "    match v {{")?;
        for (m, _) in &self.ingoing {
            writeln!(buf, "  Ingoing::{m}(m) => Self::{m}(m),",)?;
        }
        writeln!(buf, "    }}")?;
        writeln!(buf, "  }}")?;
        writeln!(buf, "}}")?;
        writeln!(buf, "impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {{")?;
        writeln!(buf, "  type Error = ();")?;
        writeln!(
            buf,
            "  fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {{"
        )?;
        writeln!(buf, "    match v {{")?;
        for (m, _) in &self.ingoing {
            writeln!(buf, "  Message::{m}(m) => Ok(Self::{m}(m)),",)?;
        }
        writeln!(buf, "      _ => Err(()),")?;
        writeln!(buf, "    }}")?;
        writeln!(buf, "  }}")?;
        writeln!(buf, "}}")?;

        // Outgoing

        writeln!(buf, "/// Outgoing messages for {}", self.name)?;
        writeln!(
            buf,
            "#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]"
        )?;
        writeln!(buf, "#[serde(bound = \"\")]")?;
        writeln!(buf, "pub enum Outgoing<T: Terms> {{")?;
        for (m, ts) in &self.outgoing {
            writeln!(buf, "/// {:?}", ts.iter().format(", "))?;
            writeln!(buf, "{m}({m}<T>),")?;
        }
        writeln!(buf, "}}")?;
        writeln!(buf, "impl<T: Terms> From<Outgoing<T>> for Message<T> {{")?;
        writeln!(buf, "  fn from(v: Outgoing<T>) -> Self {{")?;
        writeln!(buf, "    match v {{")?;
        for (m, _) in &self.outgoing {
            writeln!(buf, "  Outgoing::{m}(m) => Self::{m}(m),",)?;
        }
        writeln!(buf, "    }}")?;
        writeln!(buf, "  }}")?;
        writeln!(buf, "}}")?;
        writeln!(buf, "impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {{")?;
        writeln!(buf, "  type Error = ();")?;
        writeln!(
            buf,
            "  fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {{"
        )?;
        writeln!(buf, "    match v {{")?;
        for (m, _) in &self.outgoing {
            writeln!(buf, "  Message::{m}(m) => Ok(Self::{m}(m)),",)?;
        }
        writeln!(buf, "      _ => Err(()),")?;
        writeln!(buf, "    }}")?;
        writeln!(buf, "  }}")?;
        writeln!(buf, "}}")?;

        let run = if let Some(f) = &self.initialize_knowledge_from_first_message {
            writeln!(buf, "pub type InitialMsg<T> = {}<T>;", f.from_msg)?;

            r#"
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

                let mut recv_on: ProtocolAgent = ProtocolAgent::INITIAL_AGENT;

                loop {
                    let channel = if channels.contains_key(&recv_on) {
                        channels.get_mut(&recv_on).unwrap()
                    } else {
                        let channel = C::listen(ports(ListenPort::INITIAL_AGENT))?;
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
            "#
            .replace("INITIAL_AGENT", &f.from_agent.to_string())
        } else {
            r#"
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
            "#
            .to_string()
        };

        writeln!(buf, "{run}")?;

        // Perform

        writeln!(buf, "fn progress<T: Terms>(")?;
        writeln!(buf, "  base: &mut T,")?;
        if let Some(ty) = &self.compute_initial_knowledge_fn_ty() {
            writeln!(buf, "  compute_initial_knowledge: &mut impl {ty},",)?;
        }
        writeln!(buf, "  knowledge: &mut Knowledge<T>,")?;
        writeln!(buf, "  msg: Ingoing<T>,")?;
        writeln!(buf, ") -> Result<Response<T>> {{")?;
        writeln!(buf, "match msg {{")?;

        for msg in &self.messages {
            writeln!(buf, "Ingoing::{}(m) =>", msg.id)?;
            writeln!(buf, "{{")?;
            writeln!(
                buf,
                "let scope = tracing::span!(tracing::Level::INFO, \"{}\");",
                msg.id
            )?;
            writeln!(buf, "let _enter = scope.enter();")?;

            for inst in &msg.instructions {
                writeln!(buf, "// {inst:?}")?;
                writeln!(buf, "{}", inst.rust())?;
            }

            let send = match &msg.response.send {
                SendMessage::Nothing => format!("SendMessage::Nothing"),
                SendMessage::Send {
                    nr,
                    body,
                    connect,
                    to: (name, to),
                } => {
                    format!(
                        "SendMessage::Send {{
                            msg: Outgoing::{nr}({nr}({})),
                            connect: {connect},
                            to: (ProtocolAgent::{name}, knowledge.{to}.clone().unwrap().0),
                        }}",
                        body.iter()
                            .map(|i| format!("knowledge.{i}.clone().unwrap()"))
                            .join(","),
                    )
                }
            };
            let action = match &msg.response.action {
                NextAction::Terminate => "NextAction::Terminate".to_string(),
                NextAction::RecvFrom(p, a) => {
                    format!("NextAction::RecvFrom(ProtocolAgent::{p}, knowledge.{a}.clone().unwrap().0)")
                }
                NextAction::ListenOn(p, a) => {
                    format!(
                        "NextAction::ListenOn(ListenPort::{p}, knowledge.{a}.clone().unwrap().0)"
                    )
                }
            };

            writeln!(
                buf,
                "Ok(Response {{ save_connection_as: {}, send: {send}, action: {action} }})",
                msg.response
                    .save_connection_as
                    .clone()
                    .map(|a| format!("Some(ProtocolAgent::{a:?})"))
                    .unwrap_or_else(|| "None".to_string())
            )?;

            // let end_with = match &msg.n {
            //     MessageEnd::Send { nr, body, to } => format!(
            //         "Ok(SendMessage::Send(Outgoing::{nr}({}), knowledge.{to}.clone().unwrap().0))",
            //         body.iter()
            //             .map(|i| format!("knowledge.{i}.clone().unwrap()"))
            //             .join(","),
            //     ),
            //     MessageEnd::Terminate => format!("Ok(Response::Terminate)"),
            // };
            // writeln!(buf, "{end_with}")?;

            writeln!(buf, "}}")?;
        }

        writeln!(buf, "}}")?;
        writeln!(buf, "}}")?;

        writeln!(buf, "}}")?;

        Ok(buf)
    }
}

impl Instruction {
    fn rust(&self) -> String {
        match self {
            Instruction::Retrieve { index, id } => {
                format!("knowledge.{id} = Some(m.{index});")
            }
            Instruction::Generate(ty, init) => {
                let (f, wrapper) = match ty {
                    GenerateTy::Nonce => ("generate_nonce", "Number"),
                    GenerateTy::SymKey => ("generate_sym_key", "SymmetricKey"),
                };
                format!("knowledge.{init} = Some({wrapper}(base.{f}()));")
            }
            Instruction::DecryptSymmetric { term, key, into } => {
                format!("knowledge.{into} = Some(base.symmetric_dencrypt(&knowledge.{term}.as_ref().unwrap().0, knowledge.{key}.as_ref().unwrap())?);")
            }
            Instruction::Compare { trusted, new } => {
                format!("assert_eq!(knowledge.{trusted}, knowledge.{new});")
            }
            Instruction::Extract { from, index, into } => {
                format!("knowledge.{into} = Some(knowledge.{from}.as_ref().unwrap().0 .{index}.clone());")
            }
            Instruction::CreateTuple { from, into } => {
                format!(
                    "knowledge.{into} = Some(Tuple(({})));",
                    from.iter()
                        .map(|i| format!("knowledge.{i}.clone().unwrap()"))
                        .format(", ")
                )
            }
            Instruction::SymEnc { body, key, into } => {
                format!("knowledge.{into} = Some(SymEnc(base.symmetric_encrypt(knowledge.{body}.as_ref().unwrap(), knowledge.{key}.as_ref().unwrap())?, PhantomData));")
            }
            Instruction::ComputeInitialKnowledgeFrom { number_given, into } => {
                format!(
                    "let ({}) = compute_initial_knowledge(base, &m)?;\n{}",
                    into.iter()
                        .enumerate()
                        .map(|(i, _)| format!("k{i}"))
                        .format(", "),
                    // (0..*number_given).map(|i| format!("&m.{i}")).format(", "),
                    into.iter()
                        .enumerate()
                        .map(|(idx, i)| format!("knowledge.{i} = Some(k{idx});"))
                        .format("\n"),
                )
            }
        }
    }
}
