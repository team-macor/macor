use indexmap::IndexMap;
use macor_parse::ast::Ident;
use smol_str::SmolStr;

use crate::{
    protocol::{self, AgentName, Func, SessionId},
    terms::{Kind, TermId, Unifier},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ForWho {
    Intruder(SessionId),
    Agent(SessionId, AgentName),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct TableKey {
    for_who: ForWho,
    key: SmolStr,
}

#[derive(Debug)]
pub struct LoweringContext<'a> {
    pub unifier: &'a mut Unifier,
    pub mappings: &'a mut Mappings,
}

#[derive(Debug, Default, Clone)]
pub struct Mappings {
    global_agent_table: IndexMap<AgentName, TermId>,
    agent_table: IndexMap<TableKey, TermId>,
    func_table: IndexMap<Func, TermId>,
    global_constant_table: IndexMap<SmolStr, TermId>,
    constant_table: IndexMap<TableKey, TermId>,
    variable_table: IndexMap<TableKey, TermId>,

    term_cache: IndexMap<(ForWho, protocol::Term), TermId>,
}

impl<'a> LoweringContext<'a> {
    pub fn new(unifier: &'a mut Unifier, mappings: &'a mut Mappings) -> Self {
        Self { unifier, mappings }
    }

    pub fn get_agent(&mut self, for_who: &ForWho, agent_to_get: &AgentName) -> TermId {
        if agent_to_get.is_constant() {
            return *self
                .mappings
                .global_agent_table
                .entry(agent_to_get.clone())
                .or_insert_with(|| {
                    self.unifier
                        .register_new_constant(Some(agent_to_get.0.to_string()), Kind::Agent)
                });
        }

        match for_who {
            ForWho::Intruder(session_id) => self.get_agent(
                &ForWho::Agent(*session_id, agent_to_get.clone()),
                agent_to_get,
            ),
            ForWho::Agent(session_id, agent) => *self
                .mappings
                .agent_table
                .entry(TableKey {
                    for_who: for_who.clone(),
                    key: agent_to_get.0.clone().into(),
                })
                .or_insert_with(|| {
                    if agent_to_get.is_constant() {
                        self.unifier
                            .register_new_constant(Some(agent_to_get.0.to_string()), Kind::Agent)
                    } else {
                        self.unifier.register_new_variable(
                            Some(format!("{}@{}:{}", agent.0, session_id.0, agent_to_get.0)),
                            Kind::Agent,
                        )
                    }
                }),
        }
    }
    fn get_function_constant(&mut self, func: Func) -> TermId {
        match func {
            Func::SymEnc | Func::AsymEnc | Func::Exp | Func::Inv => *self
                .mappings
                .func_table
                .entry(func.clone())
                .or_insert_with(|| {
                    self.unifier
                        .register_new_constant(Some(format!("{:?}", func)), Kind::Other)
                }),
            Func::AsymKey(c) => self.get_global_constant(c.as_str()),
            Func::User(c) => self.get_global_constant(c.as_str()),
        }
    }
    fn get_global_constant(&mut self, constant_name: &str) -> TermId {
        *self
            .mappings
            .global_constant_table
            .entry(constant_name.into())
            .or_insert_with(|| {
                self.unifier
                    .register_new_constant(Some(constant_name), Kind::Other)
            })
    }
    fn get_constant(&mut self, for_who: &ForWho, constant_name: &Ident<SmolStr>) -> TermId {
        *self
            .mappings
            .constant_table
            .entry(TableKey {
                for_who: for_who.clone(),
                key: constant_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Agent(session_id, agent) => self.unifier.register_new_constant(
                    Some(format!("{}@{}:{}", &agent.0, session_id.0, constant_name)),
                    Kind::Other,
                ),
            })
    }
    fn get_variable(&mut self, for_who: &ForWho, variable_name: &Ident<SmolStr>) -> TermId {
        *self
            .mappings
            .variable_table
            .entry(TableKey {
                for_who: for_who.clone(),
                key: variable_name.into(),
            })
            .or_insert_with(|| match for_who {
                ForWho::Intruder(_) => todo!(),
                ForWho::Agent(session_id, agent) => self.unifier.register_new_variable(
                    Some(format!("{}@{}:{}", agent.0, session_id.0, variable_name)),
                    Kind::Other,
                ),
            })
    }

    pub fn lower_variable(
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
                        self.get_constant(for_who, &n.convert())
                    }
                    (Some(_), ForWho::Agent(_, _)) => self.get_variable(for_who, &n.convert()),
                    (Some(initiator), ForWho::Intruder(session_id)) => self
                        .get_constant(&ForWho::Agent(*session_id, initiator.clone()), &n.convert()),
                    _ => todo!("Variable {:?} ({:?})", var, initiators),
                }
            }
        }
    }
    pub fn lower_term(
        &mut self,
        for_who: &ForWho,
        initiations: &IndexMap<protocol::Variable, AgentName>,
        term: protocol::Term,
    ) -> TermId {
        let key = (for_who.clone(), term.clone());
        if let Some(id) = self.mappings.term_cache.get(&key) {
            return *id;
        }

        let id = match term {
            protocol::Term::Variable(var) => self.lower_variable(for_who, initiations, &var),
            protocol::Term::Constant(c) => match c {
                protocol::Constant::Agent(a) => self.get_agent(for_who, &a),
                protocol::Constant::Function(f) => self.get_function_constant(f),
                protocol::Constant::Intruder => self.unifier.intruder(),
                protocol::Constant::Nonce(_) => todo!(),
            },
            protocol::Term::Composition { func, args } => {
                let func = func.map(|u| self.get_global_constant(u.as_str()));
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_term(for_who, initiations, arg))
                    .collect();
                self.unifier.register_new_composition(func, args)
            }
            protocol::Term::Tuple(ts) => {
                let terms = ts
                    .into_iter()
                    .map(|t| self.lower_term(for_who, initiations, t))
                    .collect();
                self.unifier.register_new_tuple(terms)
            }
        };

        self.mappings.term_cache.insert(key, id);

        id
    }
    pub fn lower_ast_term(&mut self, term: protocol::Term<crate::typing::UntypedStage>) -> TermId {
        match term {
            protocol::Term::Variable(v) => self.get_global_constant(v.as_str()),
            protocol::Term::Constant(_) => unreachable!(),
            protocol::Term::Composition { func, args } => {
                let func = func.map(|u| self.get_global_constant(u.as_str()));
                let args = args.into_iter().map(|t| self.lower_ast_term(t)).collect();
                self.unifier.register_new_composition(func, args)
            }
            protocol::Term::Tuple(ts) => {
                let terms = ts.into_iter().map(|t| self.lower_ast_term(t)).collect();
                self.unifier.register_new_tuple(terms)
            }
        }
    }
}
