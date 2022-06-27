use indexmap::IndexMap;
use macor_parse::ast::Ident;
use smol_str::SmolStr;

use crate::{
    messages::{ConstantId, Kind, Term, TermId, Unifier},
    protocol::{self, AgentName, Func, SessionId},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ForWho {
    Intruder(SessionId),
    Agent(SessionId, AgentName),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct VariableKey {
    for_who: ForWho,
    variable: SmolStr,
}

#[derive(Debug)]
pub struct LoweringContext<'a> {
    pub unifier: &'a mut Unifier,
    pub mappings: &'a mut Mappings,
}

#[derive(Debug, Default, Clone)]
pub struct Mappings {
    global_agent_table: IndexMap<AgentName, TermId>,
    agent_table: IndexMap<(ForWho, SmolStr), TermId>,
    func_table: IndexMap<Func, TermId>,
    global_constant_table: IndexMap<SmolStr, TermId>,
    constant_table: IndexMap<VariableKey, TermId>,
    variable_table: IndexMap<VariableKey, TermId>,
}

impl<'a> LoweringContext<'a> {
    pub fn new(unifier: &'a mut Unifier, mappings: &'a mut Mappings) -> Self {
        Self { unifier, mappings }
    }

    pub fn get_agent(&mut self, ctx: &ForWho, agent_to_get: &AgentName) -> TermId {
        if agent_to_get.0.is_constant() {
            return *self
                .mappings
                .global_agent_table
                .entry(agent_to_get.clone())
                .or_insert_with(|| {
                    self.unifier
                        .register_new_constant(Some(agent_to_get.0.clone()), Kind::Agent)
                });
        }

        const AGENTS_FIX_TO_THEMSELVES: bool = false;

        match ctx {
            ForWho::Intruder(session_id) => self.get_agent(
                &ForWho::Agent(*session_id, agent_to_get.clone()),
                agent_to_get,
            ),
            ForWho::Agent(session_id, agent) => *self
                .mappings
                .agent_table
                .entry((ctx.clone(), agent_to_get.0.clone().into()))
                .or_insert_with(|| {
                    if agent_to_get.0.is_constant() {
                        self.unifier
                            .register_new_constant(Some(agent_to_get.0.clone()), Kind::Agent)
                    } else if agent_to_get == agent && AGENTS_FIX_TO_THEMSELVES {
                        self.unifier.register_new_constant(
                            Some(format!("{:?}_{:?}", agent_to_get.0.clone(), session_id.0)),
                            Kind::Agent,
                        )
                    } else {
                        self.unifier.register_new_variable(
                            Some(format!("{}@{}:{}", agent.0, session_id.0, agent_to_get.0)),
                            Kind::Agent,
                        )
                    }
                }),
        }
    }
    pub fn get_function_constant(&mut self, func: Func) -> TermId {
        match func {
            Func::SymEnc | Func::AsymEnc | Func::Exp | Func::Inv => *self
                .mappings
                .func_table
                .entry(func.clone())
                .or_insert_with(|| {
                    self.unifier
                        .register_new_constant(Some(format!("{:?}", func)), Kind::Other)
                }),
            Func::User(c) => self.register_global_constant_term(c.as_str()),
        }
    }

    pub fn register_global_constant(&mut self, constant_name: &str) -> ConstantId {
        let term = self.register_global_constant_term(constant_name);
        match self.unifier.probe_value(term) {
            Term::Constant(c, _) => c,
            _ => unreachable!(),
        }
    }
    pub fn register_global_constant_term(&mut self, constant_name: &str) -> TermId {
        *self
            .mappings
            .global_constant_table
            .entry(constant_name.into())
            .or_insert_with(|| {
                self.unifier
                    .register_new_constant(Some(constant_name), Kind::Other)
            })
    }
    pub fn register_constant(
        &mut self,
        for_who: &ForWho,
        constant_name: &Ident<SmolStr>,
    ) -> TermId {
        *self
            .mappings
            .constant_table
            .entry(VariableKey {
                for_who: for_who.clone(),
                variable: constant_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Agent(session_id, agent) => self.unifier.register_new_constant(
                    Some(format!("{}@{}:{}", &agent.0, session_id.0, constant_name)),
                    Kind::Other,
                ),
            })
    }
    pub fn register_variable(
        &mut self,
        for_who: &ForWho,
        variable_name: &Ident<SmolStr>,
    ) -> TermId {
        *self
            .mappings
            .variable_table
            .entry(VariableKey {
                for_who: for_who.clone(),
                variable: variable_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Agent(session_id, agent) => self.unifier.register_new_variable(
                    Some(format!("{}@{}:{}", agent.0, session_id.0, variable_name)),
                    Kind::Other,
                ),
            })
    }

    pub fn initiate_typed_variable(
        &mut self,
        for_who: &ForWho,
        initiators: &IndexMap<protocol::Variable, AgentName>,
        var: &protocol::Variable,
    ) -> TermId {
        match var {
            protocol::Variable::Agent(a) => self.get_agent(for_who, a),
            protocol::Variable::SymmetricKey(n) | protocol::Variable::Number(n) => {
                match (initiators.get(var), for_who) {
                    (Some(initiator), ForWho::Agent(_, agent)) if initiator == agent => {
                        self.register_constant(for_who, &n.convert())
                    }
                    (Some(_), ForWho::Agent(_, _)) => self.register_variable(for_who, &n.convert()),
                    (Some(initiator), ForWho::Intruder(session_id)) => self.register_constant(
                        &ForWho::Agent(*session_id, initiator.clone()),
                        &n.convert(),
                    ),
                    _ => todo!("Variable {:?} ({:?})", var, initiators),
                }
            }
        }
    }
    pub fn register_typed_term(
        &mut self,
        for_who: &ForWho,
        initiations: &IndexMap<protocol::Variable, AgentName>,
        term: protocol::Term,
    ) -> TermId {
        match term {
            protocol::Term::Variable(var) => {
                self.initiate_typed_variable(for_who, initiations, &var)
            }
            protocol::Term::Constant(c) => match c {
                protocol::Constant::Agent(a) => self.get_agent(for_who, &a),
                protocol::Constant::Function(f) => self.get_function_constant(f),
                protocol::Constant::Intruder => self.unifier.intruder(),
                protocol::Constant::Nonce(_) => todo!(),
            },
            protocol::Term::Composition { func, args } => {
                let term = Term::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant_term(u.as_str())),
                    },
                    args.into_iter()
                        .map(|arg| self.register_typed_term(for_who, initiations, arg))
                        .collect(),
                );
                self.unifier.register_term(term)
            }
            protocol::Term::Tuple(ts) => {
                let term = Term::Tuple(
                    ts.into_iter()
                        .map(|t| self.register_typed_term(for_who, initiations, t))
                        .collect(),
                );
                self.unifier.register_term(term)
            }
        }
    }
    pub fn register_ast_term(
        &mut self,
        term: protocol::Term<crate::typing::UntypedStage>,
    ) -> TermId {
        match term {
            protocol::Term::Variable(v) => self.register_global_constant_term(v.as_str()),
            protocol::Term::Constant(_) => unreachable!(),
            protocol::Term::Composition { func, args } => {
                let term = Term::Composition(
                    match func {
                        Func::SymEnc => Func::SymEnc,
                        Func::AsymEnc => Func::AsymEnc,
                        Func::Exp => Func::Exp,
                        Func::Inv => Func::Inv,
                        Func::User(u) => Func::User(self.register_global_constant_term(u.as_str())),
                    },
                    args.into_iter()
                        .map(|t| self.register_ast_term(t))
                        .collect(),
                );

                self.unifier.register_term(term)
            }
            protocol::Term::Tuple(ts) => {
                let term = Term::Tuple(ts.into_iter().map(|t| self.register_ast_term(t)).collect());
                self.unifier.register_term(term)
            }
        }
    }
}
