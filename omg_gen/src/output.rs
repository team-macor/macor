use itertools::Itertools;
use macor::{
    protocol::{AgentName, Func, MessageNr, Term},
    typing::Type,
};
use quote::{__private::TokenStream, format_ident, quote, ToTokens, TokenStreamExt};

use crate::v2::{
    AgentModel, GenerateTy, InfoId, Instruction, MessageId, NextAction, ProtocolModel,
    RegisterIndex, SendMessage,
};

pub fn term_to_rust_ty(t: &Term) -> TokenStream {
    match t {
        Term::Variable(v) => match v {
            macor::protocol::Variable::Agent(_) => quote!(Agent<<T as Base>::Agent>),
            macor::protocol::Variable::SymmetricKey(_) => {
                quote!(SymmetricKey<<T as Base>::SymmetricKey>)
            }
            macor::protocol::Variable::Number(_) => quote!(Number<<T as Base>::Number>),
        },
        Term::Constant(c) => match c {
            macor::protocol::Constant::Intruder => unreachable!(),
            macor::protocol::Constant::Agent(_) => quote!(Agent<<T as Base>::Agent>),
            macor::protocol::Constant::Function(_) => todo!(),
            macor::protocol::Constant::Nonce(_) => todo!(),
        },
        Term::Composition { func, args } => match func {
            Func::SymEnc => {
                let body = term_to_rust_ty(&args[0]);
                let key = term_to_rust_ty(&args[1]);
                quote!(
                    SymEnc<<T as Base>::SymEnc, #body, #key>
                )
            }
            Func::AsymEnc => {
                let body = term_to_rust_ty(&args[0]);
                let key = term_to_rust_ty(&args[1]);
                quote!(
                    AsymEnc<<T as Base>::AsymEnc, #body, #key>
                )
            }
            Func::Exp => todo!(),
            Func::Inv => {
                let args = args.iter().map(term_to_rust_ty);
                quote!(
                    Inv<<T as Base>::AsymmetricKeyInv, (#(#args),*)>
                )
            }
            Func::AsymKey(_) => {
                let args = args.iter().map(term_to_rust_ty);
                quote!(
                    AsymmetricKey<<T as Base>::AsymmetricKey, (#(#args),*)>
                )
            }
            Func::User(name) => {
                let name = format_ident!("User_{name}");
                let args = args.iter().map(term_to_rust_ty);
                quote!(
                    Func<<T as Terms>::#name, (#(#args),*)>
                )
            }
        },
        Term::Tuple(ts) => {
            let ts = ts.iter().map(term_to_rust_ty);
            quote!(Tuple<(#(#ts,)*)>)
        }
    }
}

fn type_to_rs(ty: &Type) -> TokenStream {
    match ty {
        Type::Agent => quote!(<Self as Base>::Agent),
        Type::SymmetricKey => quote!(<Self as Base>::SymmetricKey),
        Type::AsymmetricKey => quote!(<Self as Base>::AsymmetricKey),
        Type::Number => quote!(<Self as Base>::Number),
        Type::Function => todo!(),
    }
}

impl ProtocolModel {
    pub fn rust(&self) -> TokenStream {
        let types = self
            .instance
            .types
            .iter()
            .map(|t| format_ident!("User_{}", t.name))
            .collect_vec();
        let types_fns = self
            .instance
            .types
            .iter()
            .map(|t| match &t.with_fn {
                Some((args, ret)) => {
                    let name = format_ident!("{}", t.name);
                    let args = args.iter().enumerate().map(|(idx, a)| {
                        let arg = format_ident!("arg_{idx}");
                        let ty = type_to_rs(a);
                        quote!(#arg: &#ty)
                    });
                    let ret = match ret {
                        Some(ty) => type_to_rs(ty),
                        None => {
                            let ret = format_ident!("User_{}", t.name);
                            quote!(Self::#ret)
                        }
                    };
                    quote! {
                        fn #name(&mut self, #(#args),*) -> #ret
                    }
                }
                None => quote!(),
            })
            .collect_vec();

        let agent_names = self
            .agents
            .iter()
            .map(|a| format_ident!("{}", a.name.to_string()))
            .collect_vec();

        let prelude = quote! {
            #![allow(non_snake_case, non_camel_case_types)]
            use std::{{collections::HashMap, marker::PhantomData}};
            use anyhow::Result;
            use omg::{{Base, Channel}};
            use omg::terms::*;

            pub trait Terms: Base {
                #(
                    type #types: Clone + std::fmt::Debug + serde::Serialize + serde::de::DeserializeOwned;
                )*
                #(
                    #types_fns;
                )*
            }

            impl Terms for () {
                #(
                    type #types = ();
                )*
                #(
                    #types_fns {}
                )*
            }

            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
            pub enum ProtocolAgent {
                #(
                    #agent_names,
                )*
            }
        };

        let message_ids = self
            .messages
            .iter()
            .map(|(id, _)| format_ident!("{}", id.to_string()))
            .collect_vec();
        let message_terms_str = self
            .messages
            .iter()
            .map(|(_, terms)| format!("{:?}", terms))
            .collect_vec();
        let message_terms_rs = self
            .messages
            .iter()
            .map(|(_, terms)| {
                let doc = terms.iter().map(|t| format!("{t:?}"));
                eprintln!("{terms:?}");
                let terms = terms.iter().map(term_to_rust_ty);
                quote!(#(
                    #[doc = #doc]
                    pub #terms
                ),*)
            })
            .collect_vec();

        let messages = quote! {
            /// All messages in the protocol
            #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
            #[serde(bound = "")]
            pub enum Message<T: Terms> {
                #( #message_ids(#message_ids<T>) ),*
            }

            #(
                #[doc = #message_terms_str]
                #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
                #[serde(bound = "")]
                pub struct #message_ids<T: Terms>(#message_terms_rs);
            )*
        };

        let agents = self.agents.iter().map(|a| a.rust());

        quote! {
            #prelude
            #messages
            #( #agents )*
        }
    }
}

fn quote_agent_name(name: &AgentName) -> impl ToTokens {
    format_ident!("{}", name.to_string())
}
fn msg_nr(nr: MessageNr) -> impl ToTokens {
    format_ident!("{}", nr.to_string())
}
fn msg_id(id: MessageId) -> impl ToTokens {
    format_ident!("{}", id.to_string())
}

impl AgentModel {
    fn compute_initial_knowledge_fn_ty(&self) -> Option<TokenStream> {
        if let Some(iik) = &self.initialize_knowledge_from_first_message {
            let msg = msg_nr(iik.from_msg);
            Some(quote!(
                Fn(&mut T, &#msg<T>) -> Result<InitialKnowledge<T>>
            ))
        } else {
            None
        }
    }

    fn rust(&self) -> TokenStream {
        let mut token_stream = TokenStream::new();

        let mod_name = format_ident!("agent_{}", self.name.to_string());
        let agent_name = format_ident!("{}", self.name.to_string());

        // State

        let state_comment = format!("The state for agent {agent_name}");
        let state_tokens = if let Some(ty) = self.compute_initial_knowledge_fn_ty() {
            quote! {
                #[doc = #state_comment]
                pub struct State<T: Terms, F>
                where
                    F: #ty,
                {
                    knowledge: Knowledge<T>,
                    stage: Stage,
                    compute_initial_knowledge: F,
                }
            }
        } else {
            quote! {
                #[doc = #state_comment]
                pub struct State<T: Terms> {
                    knowledge: Knowledge<T>,
                    stage: Stage,
                }
            }
        };
        token_stream.append_all(state_tokens);

        let stage_names = self.stage_names.iter().map(|n| format_ident!("{n}"));
        let first_stage = stage_names.clone().next().unwrap();
        token_stream.append_all(quote! {
            /// The stage of the protocol for agent
            pub enum Stage {
                #( #stage_names ),*
            }
            impl Default for Stage {
                fn default() -> Self {
                    Self::#first_stage
                }
            }
        });

        token_stream.append_all(quote! {
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
        });

        let listen_ports = self
            .listen_ports
            .iter()
            .map(|p| format_ident!("{p}"))
            .collect_vec();
        token_stream.append_all(quote! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
            pub enum ListenPort {
                #(#listen_ports),*
            }
            impl From<ListenPort> for ProtocolAgent {
                fn from(value: ListenPort) -> Self {
                    match value {
                        #(ListenPort::#listen_ports => Self::#listen_ports),*
                    }
                }
            }
        });

        let initial_knowledge = self
            .initial_knowledge
            .iter()
            .map(|(_, t)| term_to_rust_ty(t));
        token_stream
            .append_all(quote!(pub type InitialKnowledge<T> = (#( #initial_knowledge ),*);));

        let stage_transitions = self
            .stage_names
            .iter()
            .zip(self.stage_names.iter().skip(1))
            .map(|(id, next)| {
                let id = format_ident!("{id}");
                let next = format_ident!("{next}");
                quote!((Stage::#id, Ingoing::#id(_)) => self.stage = Stage::#next)
            });
        let stage_transition = quote!(
            match (&self.stage, &msg) {
                #(#stage_transitions),*,
                _ => anyhow::bail!("out of order!"),
            }
        );

        let state_impl = if let Some(ty) = self.compute_initial_knowledge_fn_ty() {
            quote! {
                impl<T: Terms, F> State<T, F>
                where
                    F: #ty,
                {
                    pub fn init(compute_initial_knowledge: F) -> Self {
                        State {
                            knowledge: Default::default(),
                            stage: Default::default(),
                            compute_initial_knowledge,
                        }
                    }
                    pub fn handle_message(&mut self, base: &mut T, msg: Ingoing<T>) -> Result<Response<T>> {
                        #stage_transition

                        progress(
                            base,
                            &mut self.compute_initial_knowledge,
                            &mut self.knowledge,
                            msg,
                        )
                    }
                }
            }
        } else {
            quote! {
                impl<T: Terms> State<T> {
                    pub fn init() -> Self {
                        State {
                            knowledge: Default::default(),
                            stage: Default::default(),
                        }
                    }
                    pub fn handle_message(&mut self, base: &mut T, msg: Ingoing<T>) -> Result<Response<T>> {
                        #stage_transition

                        progress(
                            base,
                            &mut self.knowledge,
                            msg,
                        )
                    }
                }
            }
        };
        token_stream.append_all(state_impl);

        // Registers

        let knowledge_comment = self.registers.iter().map(|(_, t)| format!("{t:?}"));
        let knowledge_name = self.registers.iter().map(|(i, _)| i).collect_vec();
        let knowledge_ty = self.registers.iter().map(|(_, t)| term_to_rust_ty(t));
        let knowledge = quote! {
            /// The knowledge required for agent
            pub struct Knowledge<T: Terms> {
                #(
                    #[doc = #knowledge_comment]
                    #knowledge_name: Option<#knowledge_ty>,
                )*
            }
            impl<T: Terms> Default for Knowledge<T> {
                fn default() -> Self {
                    Self {
                        #( #knowledge_name: None, )*
                    }
                }
            }
        };
        token_stream.append_all(knowledge);

        // Ingoing

        let ingoing_comment = self
            .ingoing
            .iter()
            .map(|(_, ts)| format!("{:?}", ts.iter().format(", ")));
        let ingoing_msg = self.ingoing.iter().map(|(m, _)| msg_id(*m)).collect_vec();
        let ingoing = quote! {
            /// Ingoing messages for agent
            #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
            #[serde(bound = "")]
            pub enum Ingoing<T: Terms> {
                #(
                    #[doc = #ingoing_comment]
                    #ingoing_msg(#ingoing_msg<T>),
                )*
            }
            impl<T: Terms> From<Ingoing<T>> for Message<T> {
                fn from(v: Ingoing<T>) -> Self {
                    match v {
                        #(
                            Ingoing::#ingoing_msg(m) => Self::#ingoing_msg(m),
                        )*
                    }
                }
            }
            impl<T: Terms> TryFrom<Message<T>> for Ingoing<T> {
                type Error = ();
                fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
                    match v {
                        #(
                            Message::#ingoing_msg(m) => Ok(Self::#ingoing_msg(m)),
                        )*
                        _ => Err(())
                    }
                }
            }
        };
        token_stream.append_all(ingoing);

        // Outgoing

        let outgoing_comment = self
            .outgoing
            .iter()
            .map(|(_, ts)| format!("{:?}", ts.iter().format(", ")));
        let outgoing_msg = self.outgoing.iter().map(|(m, _)| msg_nr(*m)).collect_vec();
        let outgoing = quote! {
            /// Outgoing messages for agent
            #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
            pub enum Outgoing<T: Terms> {
                #(
                    #[doc = #outgoing_comment]
                    #outgoing_msg(#outgoing_msg<T>),
                )*
            }
            impl<T: Terms> From<Outgoing<T>> for Message<T> {
                fn from(v: Outgoing<T>) -> Self {
                    match v {
                        #(
                            Outgoing::#outgoing_msg(m) => Self::#outgoing_msg(m),
                        )*
                    }
                }
            }
            impl<T: Terms> TryFrom<Message<T>> for Outgoing<T> {
                type Error = ();
                fn try_from(v: Message<T>) -> std::result::Result<Self, ()> {
                    match v {
                        #(
                            Message::#outgoing_msg(m) => Ok(Self::#outgoing_msg(m)),
                        )*
                        _ => Err(())
                    }
                }
            }
        };
        token_stream.append_all(outgoing);

        if let Some(f) = &self.initialize_knowledge_from_first_message {
            let initial_msg = msg_nr(f.from_msg);
            token_stream.append_all(quote!(pub type InitialMsg<T> = #initial_msg<T>;));

            let initial_agent = format_ident!("{}", f.from_agent.to_string());

            token_stream.append_all(quote! {
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

                    let mut recv_on: ProtocolAgent = ProtocolAgent::#initial_agent;

                    loop {
                        let channel = if channels.contains_key(&recv_on) {
                            channels.get_mut(&recv_on).unwrap()
                        } else {
                            let channel = C::listen(ports(ListenPort::#initial_agent))?;
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
            })
        } else {
            token_stream.append_all(quote! {
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
            });
        };

        // Perform

        let progress = {
            let compute_initial_knowledge_arg =
                if let Some(ty) = &self.compute_initial_knowledge_fn_ty() {
                    quote!(compute_initial_knowledge: &mut impl #ty,)
                } else {
                    quote!()
                };

            let messages = self.messages.iter().map(|msg| {
                let id = msg_id(msg.id);
                let instructions = msg.instructions.iter().map(|inst| {
                    // TODO: Figure out how to add normal comments
                    // let comment = format!("{inst:?}");
                    quote!(
                        // #[doc = #comment]
                        #inst
                    )
                });

                let save_connection_as = msg.response
                .save_connection_as
                .as_ref()
                .map(quote_agent_name)
                .map(|a| quote!(Some(ProtocolAgent::#a)))
                .unwrap_or_else(|| quote!(None));
                let send = match &msg.response.send {
                    SendMessage::Nothing => quote!(SendMessage::Nothing),
                    SendMessage::Send {
                        nr,
                        body,
                        connect,
                        to: (name, to),
                    } => {
                        let nr = msg_nr(*nr);
                        let name = format_ident!("{}", name.to_string());
                        let body = body.iter().map(|i| quote!(knowledge.#i.clone().unwrap()));

                        quote!(
                            SendMessage::Send {
                                msg: Outgoing::#nr(#nr(#( #body ),*)),
                                connect: #connect,
                                to: (ProtocolAgent::#name, knowledge.#to.clone().unwrap().0),
                            }
                        )
                    }
                };
                let action = match &msg.response.action {
                    NextAction::Terminate => quote!(NextAction::Terminate),
                    NextAction::RecvFrom(p, a) => {
                        let p = quote_agent_name(p);
                        quote!(NextAction::RecvFrom(ProtocolAgent::#p, knowledge.#a.clone().unwrap().0))
                    }
                    NextAction::ListenOn(p, a) => {
                        let p = quote_agent_name(p);
                        quote!(
                            NextAction::ListenOn(ListenPort::#p, knowledge.#a.clone().unwrap().0)
                        )
                    }
                };
                quote! {
                    Ingoing::#id(m) => {
                        let scope = tracing::span!(tracing::Level::INFO, stringify!(#id));
                        let _enter = scope.enter();
                        #( #instructions )*
                        Ok(Response {
                            save_connection_as: #save_connection_as,
                            send: #send,
                            action: #action
                        })
                    }
                }
            });

            quote! {
                fn progress<T: Terms>(
                    base: &mut T,
                    #compute_initial_knowledge_arg
                    knowledge: &mut Knowledge<T>,
                    msg: Ingoing<T>,
                ) -> Result<Response<T>> {
                    match msg {
                        #( #messages )*
                    }
                }
            }
        };

        token_stream.append_all(progress);

        quote! {
            pub mod #mod_name {
                use super::*;
                #token_stream
            }
        }
    }
}

impl ToTokens for Instruction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let t = match self {
            Instruction::Retrieve { index, id } => {
                quote!(knowledge.#id = Some(m.#index);)
            }
            Instruction::Generate(ty, init) => {
                let (f, wrapper) = match ty {
                    GenerateTy::Nonce => (quote!(generate_nonce), quote!(Number)),
                    GenerateTy::SymKey => (quote!(generate_sym_key), quote!(SymmetricKey)),
                };
                quote!(knowledge.#init = Some(#wrapper(base.#f()));)
            }
            Instruction::DecryptSymmetric { term, key, into } => {
                quote!(knowledge.#into = Some(base.symmetric_dencrypt(&knowledge.#term.as_ref().unwrap().0, knowledge.#key.as_ref().unwrap())?);)
            }
            Instruction::DecryptAsymmetric { term, key, into } => {
                quote!(knowledge.#into = Some(base.asymmetric_dencrypt(&knowledge.#term.as_ref().unwrap().0, &knowledge.#key.as_ref().unwrap().0)?);)
            }
            Instruction::Compare { trusted, new } => {
                quote!(assert_eq!(knowledge.#trusted, knowledge.#new);)
            }
            Instruction::Extract { from, index, into } => {
                quote!(knowledge.#into = Some(knowledge.#from.as_ref().unwrap().0 .#index.clone());)
            }
            Instruction::CreateTuple { from, into } => {
                let ts = from.iter().map(|i| quote!(knowledge.#i.clone().unwrap()));
                quote!(knowledge.#into = Some(Tuple((#(#ts,)*)));)
            }
            Instruction::SymEnc { body, key, into } => {
                quote!(knowledge.#into = Some(SymEnc(base.symmetric_encrypt(knowledge.#body.as_ref().unwrap(), knowledge.#key.as_ref().unwrap())?, PhantomData));)
            }
            Instruction::AsymEnc { body, key, into } => {
                quote!(knowledge.#into = Some(AsymEnc(base.asymmetric_encrypt(knowledge.#body.as_ref().unwrap(), &knowledge.#key.as_ref().unwrap().0)?, PhantomData));)
            }
            Instruction::ComputeInitialKnowledgeFrom {
                number_given: _,
                into,
            } => {
                let into_vars = into.iter().enumerate().map(|(i, _)| format_ident!("k{i}"));
                let update = into.iter().enumerate().map(|(idx, i)| {
                    let k = format_ident!("k{idx}");
                    quote!(knowledge.#i = Some(#k);)
                });
                quote!(
                    let (#(#into_vars),*) = compute_initial_knowledge(base, &m)?;
                    #(#update);*
                )
            }
            Instruction::Evaluate { func, args, into } => {
                let func = format_ident!("{func}");
                let args = args
                    .iter()
                    .map(|i| quote!(&knowledge.#i.as_ref().unwrap().0));

                quote! {
                    knowledge.#into = Some(Func(<T as Terms>::#func(base, #( #args ),*), PhantomData));
                }
            }
        };

        tokens.append_all(t);
    }
}

impl ToTokens for RegisterIndex {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        let index = syn::Index::from(self.0);
        tokens.append_all(quote!(#index));
    }
}
impl ToTokens for InfoId {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(format_ident!("{}", self.to_string()))
    }
}
