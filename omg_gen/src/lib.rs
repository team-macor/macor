use std::collections::HashMap;

use itertools::Itertools;
use macor::{
    protocol::{Constant, Direction, Func, Protocol, ProtocolAgent, Term, Variable},
    typing::Type,
};
use macor_parse::ast::{Document, Ident};

mod dolev_yao;

pub struct Omg<'a> {
    pub src: &'a str,
    pub doc: Document<&'a str>,
    pub protocol: Protocol,
}

impl<'a> Omg<'a> {
    pub fn new(src: &'a str, doc: Document<&'a str>) -> anyhow::Result<Omg<'a>> {
        let protocol =
            Protocol::new(src.to_string(), doc.clone()).map_err(|x| x.first().cloned().unwrap())?;

        Ok(Self { src, doc, protocol })
    }
    // fn functions(&self) -> Vec<(Name, Args, Type)> {}
    pub fn generate_for_agent(&self, agent: &ProtocolAgent) -> Result<String, std::fmt::Error> {
        use std::fmt::Write;

        let mut buf = String::new();

        writeln!(buf, "pub struct Agent_{}<T: Terms> {{", agent.name)?;
        writeln!(buf, "\twaiting_for: Option<Agent_{}_Msg<()>>,", agent.name)?;
        writeln!(buf, "\tknowledge: dolev_yao::Knowledge<T>,")?;
        writeln!(buf, "}}")?;
        writeln!(
            buf,
            "#[derive(Debug, serde::Serialize, serde::Deserialize)]"
        )?;
        writeln!(buf, "pub enum Agent_{}_Msg<T: Terms> {{", agent.name)?;
        for msg in &agent.messages {
            if msg.direction == Direction::Ingoing {
                let terms = msg
                    .message
                    .iter()
                    .map(|t| self.concrete_name(t))
                    .format(", ");
                writeln!(buf, "\tS{}({}),", msg.nr, terms)?;
            }
        }
        writeln!(buf, "}}")?;
        writeln!(
            buf,
            "impl Default for Agent_{}_Msg<()> {{ fn default() -> Self {{",
            agent.name,
        )?;
        writeln!(
            buf,
            "\t Agent_{}_Msg::{} }} }}",
            agent.name,
            agent
                .messages
                .iter()
                .find_map(|msg| if msg.direction == Direction::Ingoing {
                    Some(format!(
                        "S{}({})",
                        msg.nr,
                        msg.message.iter().map(|_| "()").format(",")
                    ))
                } else {
                    None
                })
                .unwrap()
        )?;

        writeln!(buf, "impl<T: Terms> Agent_{}_Msg<T> {{", agent.name)?;
        writeln!(
            buf,
            "fn same_discriminant<S: Terms>(&self, b: &Agent_{}_Msg<S>) -> bool {{",
            agent.name,
        )?;
        writeln!(buf, "    match (self, b) {{")?;

        for msg in &agent.messages {
            if msg.direction == Direction::Ingoing {
                let args = msg.message.iter().map(|_| "_").join(", ");
                writeln!(
                    buf,
                    "        (Agent_{}_Msg::S{}({args}), Agent_{}_Msg::S{}({args})) => true,",
                    agent.name, msg.nr, agent.name, msg.nr,
                )?;
            }
        }
        writeln!(buf, "        _ => false,")?;
        writeln!(buf, "    }}")?;
        writeln!(buf, "}}")?;
        writeln!(buf, "}}")?;

        writeln!(
            buf,
            "pub struct Agent_{}_InitialKnowledge<T: Terms> {{",
            agent.name
        )?;
        writeln!(
            buf,
            "\t{}",
            agent
                .initial_knowledge
                .iter()
                .map(|arg| format!(
                    "pub {}: {}",
                    self.canonical_name(arg),
                    self.concrete_name(arg)
                ))
                .format(",\n\t")
        )?;
        writeln!(buf, "}}")?;
        writeln!(
            buf,
            "impl<T: Terms> Agent_{}_InitialKnowledge<T> {{",
            agent.name
        )?;
        writeln!(
            buf,
            "\tfn knowledge(self, base: &mut T) -> Result<dolev_yao::Knowledge<T>> {{"
        )?;
        let terms = agent
            .initial_knowledge
            .iter()
            .map(|t| {
                format!(
                    "({}, {})",
                    self.dyn_pattern_for_term(t),
                    self.make_concrete_dynamic(t, &format!("self.{}", self.canonical_name(t)))
                )
            })
            .format(",");
        writeln!(
            buf,
            "Ok(dolev_yao::Knowledge([{}].into_iter().collect()))",
            terms
        )?;
        writeln!(buf, "}}")?;
        writeln!(buf, "}}")?;
        writeln!(buf, "impl<T: Terms> Agent_{}<T> {{", agent.name)?;
        if let Some(msg) = agent.initiated_by_message() {
            let args = msg
                .message
                .terms
                .iter()
                .map(|t| format!("{}", self.associated_type("T", &t.ty())))
                .format(", ");
            let ref_args = msg
                .message
                .terms
                .iter()
                .map(|t| format!("&{}", self.associated_type("T", &t.ty())))
                .format(", ");
            writeln!(
                buf,
                "\tpub fn new(base: &mut T, f: impl Fn({}) -> Agent_{}_InitialKnowledge<T>, msg: ({})) -> Result<(Self, Effect<T>)> {{",
                ref_args,
                agent.name,
                args,
            )?;
            writeln!(
                buf,
                "\t\tlet mut agent = Self {{ waiting_for: Some(Default::default()), knowledge: f({}).knowledge(base)? }};",
                msg.message
                    .terms
                    .iter()
                    .enumerate()
                    .map(|(arg_i, _)| format!("&msg.{arg_i}"))
                    .format(", ")
            )?;
            writeln!(
                buf,
                "\t\tlet effect = agent.pump(base, Agent_{}_Msg::S{}({}))?;",
                agent.name,
                msg.nr,
                msg.message
                    .terms
                    .iter()
                    .enumerate()
                    .map(|(arg_i, _)| format!("msg.{arg_i}"))
                    .format(", ")
            )?;
            writeln!(buf, "\t\tOk((agent, effect))")?;
            writeln!(buf, "\t}}")?;
        } else {
            writeln!(
                buf,
                "\tpub fn new(base: &mut T, initial_knowledge: Agent_{}_InitialKnowledge<T>) -> Result<Self> {{",
                agent.name,
            )?;
            writeln!(
                buf,
                "\t\tOk(Self {{ waiting_for: Some(Default::default()), knowledge: initial_knowledge.knowledge(base)? }})"
            )?;
            writeln!(buf, "\t}}")?;
        }
        writeln!(
            buf,
            "\tpub fn pump(&mut self, base: &mut T, msg: Agent_{}_Msg<T>) -> Result<Effect<T>> {{ println!(\"{{msg:?}}\");",
            agent.name
        )?;
        writeln!(
            buf,
            "\t\tlet waiting_for = if let Some(waiting_for) = &self.waiting_for {{ waiting_for }} else {{ bail!(\"My session has ended!\") }};"
        )?;
        writeln!(buf, "\t\tif !msg.same_discriminant(&waiting_for) {{")?;
        writeln!(
            buf,
            "\t\t\tbail!(\"Oh noes! Message received out of order!\");"
        )?;
        writeln!(buf, "\t\t}}")?;
        writeln!(buf, "\t\tmatch msg {{")?;
        for (idx, msg) in agent.messages.iter().enumerate() {
            if msg.direction == Direction::Ingoing {
                println!("ingoing: {msg:?}");
                let terms = msg
                    .message
                    .iter()
                    .enumerate()
                    .map(|(arg_i, _)| format!("arg{arg_i}"))
                    .format(", ");
                writeln!(
                    buf,
                    "\t\t\tAgent_{}_Msg::S{}({}) => {{",
                    agent.name, msg.nr, terms
                )?;
                for (arg_i, arg) in msg.message.iter().enumerate() {
                    writeln!(buf, "\t\t\t\t// Check that arg_{arg_i} matches {arg:?}")?;
                    writeln!(
                        buf,
                        "\t\t\t\tself.knowledge.register({}, {})?;",
                        self.dyn_pattern_for_term(arg),
                        self.make_concrete_dynamic(arg, &format!("arg{arg_i}")),
                    )?;
                }

                writeln!(buf, "\t\t\t\tself.knowledge.decompose(base)?;")?;

                if let Some(ingoing) = agent.messages.get(idx + 2) {
                    let args = ingoing.message.terms.iter().map(|_| "()").format(", ");
                    writeln!(
                        buf,
                        "self.waiting_for = Some(Agent_{}_Msg::S{}({args}));",
                        agent.name, ingoing.nr
                    )?;
                } else {
                    writeln!(buf, "self.waiting_for = None;")?;
                }

                if let Some(outgoing) = agent.messages.get(idx + 1) {
                    let args = outgoing
                        .message
                        .terms
                        .iter()
                        .map(|arg| {
                            let a = format!(
                                "let term = self.knowledge.construct(base, {})?;",
                                self.dyn_pattern_for_term(arg)
                            );
                            let b = self.make_dynamic_concrete(arg, &"term");
                            format!("{{ {a}\n{b} }}")
                        })
                        .format(",");
                    writeln!(
                        buf,
                        "\t\t\t\tlet outgoing = Agent_{}_Msg::<T>::S{}({});",
                        outgoing.to, outgoing.nr, args
                    )?;

                    writeln!(
                        buf,
                        "let receiver = self.knowledge.construct(base, DynTerm::Agent(\"{}\".to_string()))?;",
                        outgoing.to
                    )?;
                    let receiver = format!("base.concrete_agent(&receiver)?");
                    let body = format!("Message::Agent_{}(outgoing)", outgoing.to);
                    writeln!(
                        buf,
                        "\t\t\t\tOk(Effect::Send {{ receiver: {receiver}, body: base.serialize({body})? }})"
                    )?;
                    // writeln!(buf, "\t\t\t\ttodo!(\"{{outgoing:?}}\");")?;
                } else {
                    writeln!(buf, "Ok(Effect::Finished)")?;
                }

                writeln!(buf, "\t\t\t}}")?;
            }
        }
        writeln!(buf, "\t\t}}")?;
        writeln!(buf, "\t}}")?;
        writeln!(buf, "}}")?;

        Ok(buf)
    }

    pub fn make_concrete_dynamic(&self, t: &Term, arg: &str) -> String {
        match t {
            Term::Variable(v) => match v {
                Variable::Agent(_) => format!("T::dyn_agent({arg})"),
                Variable::SymmetricKey(_) => format!("T::dyn_symmetric_key({arg})"),
                Variable::Number(_) => format!("T::dyn_number({arg})"),
            },
            Term::Constant(c) => match c {
                Constant::Intruder => todo!(),
                Constant::Agent(_) => format!("T::dyn_agent({arg})"),
                Constant::Function(_) => todo!(),
                Constant::Nonce(_) => format!("T::dyn_number({arg})"),
            },
            Term::Composition { func, args } => {
                let func = match func {
                    Func::SymEnc => return format!("DynTerm::SymEnc({arg})"),
                    Func::AsymEnc => return format!("DynTerm::AsymEnc({arg})"),
                    Func::Exp => format!("Func::Exp"),
                    Func::Inv => format!("Func::Inv"),
                    Func::User(f) => format!("Func::User(\"{f}\".to_string())"),
                };

                return format!("DynTerm::Custom(base.serialize({arg})?)");

                format!(
                    "DynTerm::Composition {{ func: {func}, args: vec![{}] }}",
                    args.iter()
                        .enumerate()
                        .map(|(arg_i, a)| self.make_concrete_dynamic(a, &format!("{arg}[{arg_i}]")))
                        .format(", ")
                )
            }
            Term::Tuple(ts) => format!(
                "DynTerm::Tuple(vec![{}])",
                ts.iter()
                    .enumerate()
                    .map(|(arg_i, a)| self.make_concrete_dynamic(a, &format!("{arg}.{arg_i}")))
                    .format(", ")
            ),
        }
    }
    pub fn make_dynamic_concrete(&self, t: &Term, arg: &str) -> String {
        match t {
            Term::Variable(v) => match v {
                Variable::Agent(_) => format!("base.concrete_agent(&{arg})?"),
                Variable::SymmetricKey(_) => format!("base.concrete_symmetric_key(&{arg})?"),
                Variable::Number(_) => format!("base.concrete_number(&{arg})?"),
            },
            Term::Constant(c) => match c {
                Constant::Intruder => todo!(),
                Constant::Agent(_) => format!("base.concrete_agent(&{arg})?"),
                Constant::Function(_) => todo!(),
                Constant::Nonce(_) => format!("base.concrete_number(&{arg})?"),
            },
            Term::Composition { func, args } => {
                let func = match func {
                    Func::SymEnc => return format!("base.concrete_sym_enc(&{arg})?"),
                    Func::AsymEnc => return format!("base.concrete_asym_enc(&{arg})?"),
                    Func::Exp => format!("base.concrete_exp(&{arg})?"),
                    Func::Inv => format!("base.concrete_inv(&{arg})?"),
                    Func::User(f) => format!("base.concrete_user_func(\"{f}\", {arg})?"),
                };
                format!(
                    "DynTerm::Composition {{ func: {func}, args: vec![{}] }}",
                    args.iter()
                        .enumerate()
                        .map(|(arg_i, a)| self.make_concrete_dynamic(a, &format!("{arg}[{arg_i}]")))
                        .format(", ")
                )
            }
            Term::Tuple(ts) => format!(
                "DynTerm::Tuple(vec![{}])",
                ts.iter()
                    .enumerate()
                    .map(|(arg_i, a)| self.make_concrete_dynamic(a, &format!("{arg}.{arg_i}")))
                    .format(", ")
            ),
        }
    }

    pub fn dyn_pattern_for_term(&self, t: &Term) -> String {
        match t {
            Term::Variable(v) => match v {
                Variable::Agent(a) => format!("DynTerm::Agent(\"{a}\".to_string())"),
                Variable::SymmetricKey(s) => format!("DynTerm::SymmetricKey(\"{s}\".to_string())"),
                Variable::Number(n) => format!("DynTerm::Number(\"{n}\".to_string())"),
            },
            Term::Constant(c) => match c {
                Constant::Intruder => unreachable!(),
                Constant::Agent(a) => format!("DynTerm::Agent(\"{a}\".to_string())"),
                Constant::Function(_) => todo!(),
                Constant::Nonce(_) => todo!(),
            },
            Term::Composition { func, args } => {
                let func = match func {
                    Func::SymEnc => format!("Func::SymEnc"),
                    Func::AsymEnc => format!("Func::AsymEnc"),
                    Func::Exp => format!("Func::Exp"),
                    Func::Inv => format!("Func::Inv"),
                    Func::User(f) => format!("Func::User(\"{f}\".to_string())"),
                };
                format!(
                    "DynTerm::Composition {{ func: {func}, args: vec![{}], }}",
                    args.iter()
                        .map(|arg| self.dyn_pattern_for_term(arg))
                        .format(", ")
                )
            }
            Term::Tuple(ts) => format!(
                "DynTerm::Tuple(vec![{}])",
                ts.iter().map(|t| self.dyn_pattern_for_term(t)).format(", ")
            ),
        }
    }

    pub fn fn_signature(&self, name: &str, args: &[Type]) -> String {
        format!(
            "fn {}(&mut self, {}) -> Self::User_{}",
            name,
            args.iter()
                .enumerate()
                .map(|(arg_i, arg)| {
                    let ty = self.associated_type("Self", arg);
                    format!("arg_{arg_i}: {ty}")
                })
                .format(", "),
            name
        )
    }
    pub fn associated_type(&self, parent: &str, ty: &Type) -> String {
        match ty {
            Type::Agent => format!("{parent}::Agent"),
            Type::SymmetricKey => format!("{parent}::SymmetricKey"),
            Type::Number => format!("{parent}::Number"),
            Type::Function => todo!(),
        }
    }
    fn canonical_name(&self, t: &Term) -> String {
        let ಠ_ಠ = |s: String| {
            s.chars()
                .map(|c| match c {
                    '(' => '_',
                    ')' => '_',
                    c => c,
                })
                .collect()
        };

        match t {
            Term::Variable(var) => ಠ_ಠ(format!("{var:?}")),
            Term::Constant(c) => ಠ_ಠ(format!("{c:?}")),
            Term::Composition { func, args } => ಠ_ಠ(format!(
                "{func:?}({})",
                args.iter().map(|arg| self.canonical_name(arg)).format("_")
            )),
            Term::Tuple(tups) => ಠ_ಠ(tups.iter().map(|arg| self.canonical_name(arg)).join("_")),
        }
    }
    fn concrete_name(&self, t: &Term) -> String {
        match t {
            Term::Variable(v) => match v {
                Variable::Agent(_) => format!("T::Agent"),
                Variable::SymmetricKey(_) => format!("T::SymmetricKey"),
                Variable::Number(_) => format!("T::Number"),
            },
            Term::Constant(c) => match c {
                Constant::Intruder => format!("T::Agent"),
                Constant::Agent(_) => format!("T::Agent"),
                Constant::Function(_) => todo!(),
                Constant::Nonce(_) => format!("T::Number"),
            },
            Term::Composition { func, args } => match func {
                Func::SymEnc => format!("T::SymEnc"),
                Func::AsymEnc => format!("T::AsymEnc"),
                Func::Exp => format!("T::Exp"), // Should this even be sent over the protocol?
                Func::Inv => format!("T::Inv"), // Should this even be sent over the protocol?
                Func::User(f) => format!("T::User_{f}"),
                // trait Base {
                //     type Var;
                //     type Const;
                //     type SymEnc<T>;
                //     type AsymEnc<T>;
                //     type Exp;
                //     type Inv<K>;
                // }
                // trait Terms: Base {
                //     // Custom
                //     type User_f;
                // }
            },
            Term::Tuple(tups) => format!(
                "({})",
                tups.iter().map(|t| self.concrete_name(t)).format(", ")
            ),
        }
    }
}
