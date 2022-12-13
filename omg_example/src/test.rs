#![allow(non_camel_case_types, non_snake_case)]

use anyhow::{bail, Result};
use omg::{Base, DynTerm, Effect, Func, Pattern};
use std::collections::HashMap;

pub trait Terms: Base {
    type User_sk: serde::Serialize + serde::de::DeserializeOwned;
    fn sk(&mut self, arg_0: Self::Agent, arg_1: Self::Agent) -> Self::User_sk;
}
impl Terms for () {
    type User_sk = ();
    fn sk(&mut self, arg_0: Self::Agent, arg_1: Self::Agent) -> Self::User_sk {
        ()
    }
}
impl Terms for Pattern {
    type User_sk = DynTerm<Self>;
    fn sk(&mut self, arg_0: Self::Agent, arg_1: Self::Agent) -> Self::User_sk {
        todo!()
    }
}
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub enum Message<T: Terms> {
    Agent_A(Agent_A_Msg<T>),
    Agent_B(Agent_B_Msg<T>),
    Agent_s(Agent_s_Msg<T>),
}
pub struct Agent_A<T: Terms> {
    waiting_for: Option<Agent_A_Msg<()>>,
    knowledge: dolev_yao::Knowledge<T>,
}
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Agent_A_Msg<T: Terms> {
    S0(T::Agent, T::Agent, T::Number),
    S2(T::SymEnc, T::SymEnc),
}
impl Default for Agent_A_Msg<()> {
    fn default() -> Self {
        Agent_A_Msg::S0((), (), ())
    }
}
impl<T: Terms> Agent_A_Msg<T> {
    fn same_discriminant<S: Terms>(&self, b: &Agent_A_Msg<S>) -> bool {
        match (self, b) {
            (Agent_A_Msg::S0(_, _, _), Agent_A_Msg::S0(_, _, _)) => true,
            (Agent_A_Msg::S2(_, _), Agent_A_Msg::S2(_, _)) => true,
            _ => false,
        }
    }
}
pub struct Agent_A_InitialKnowledge<T: Terms> {
    pub Agent_A_: T::Agent,
    pub Agent_B_: T::Agent,
    pub Agent_s_: T::Agent,
    pub User_sk__Agent_A__Agent_s__: T::User_sk,
}
impl<T: Terms> Agent_A_InitialKnowledge<T> {
    fn knowledge(self, base: &mut T) -> Result<dolev_yao::Knowledge<T>> {
        Ok(dolev_yao::Knowledge(
            [
                (DynTerm::Agent("A".to_string()), T::dyn_agent(self.Agent_A_)),
                (DynTerm::Agent("B".to_string()), T::dyn_agent(self.Agent_B_)),
                (DynTerm::Agent("s".to_string()), T::dyn_agent(self.Agent_s_)),
                (
                    DynTerm::Composition {
                        func: Func::User("sk".to_string()),
                        args: vec![
                            DynTerm::Agent("A".to_string()),
                            DynTerm::Agent("s".to_string()),
                        ],
                    },
                    DynTerm::Custom(base.serialize(self.User_sk__Agent_A__Agent_s__)?),
                ),
            ]
            .into_iter()
            .collect(),
        ))
    }
}
impl<T: Terms> Agent_A<T> {
    pub fn new(
        base: &mut T,
        f: impl Fn(&T::Agent, &T::Agent, &T::Number) -> Agent_A_InitialKnowledge<T>,
        msg: (T::Agent, T::Agent, T::Number),
    ) -> Result<(Self, Effect<T>)> {
        let mut agent = Self {
            waiting_for: Some(Default::default()),
            knowledge: f(&msg.0, &msg.1, &msg.2).knowledge(base)?,
        };
        let effect = agent.pump(base, Agent_A_Msg::S0(msg.0, msg.1, msg.2))?;
        Ok((agent, effect))
    }
    pub fn pump(&mut self, base: &mut T, msg: Agent_A_Msg<T>) -> Result<Effect<T>> {
        println!("{msg:?}");
        let waiting_for = if let Some(waiting_for) = &self.waiting_for {
            waiting_for
        } else {
            bail!("My session has ended!")
        };
        if !msg.same_discriminant(&waiting_for) {
            bail!("Oh noes! Message received out of order!");
        }
        match msg {
            Agent_A_Msg::S0(arg0, arg1, arg2) => {
                // Check that arg_0 matches @Agent(A)
                self.knowledge
                    .register(DynTerm::Agent("A".to_string()), T::dyn_agent(arg0))?;
                // Check that arg_1 matches @Agent(B)
                self.knowledge
                    .register(DynTerm::Agent("B".to_string()), T::dyn_agent(arg1))?;
                // Check that arg_2 matches @Number(NB)
                self.knowledge
                    .register(DynTerm::Number("NB".to_string()), T::dyn_number(arg2))?;
                self.knowledge.decompose(base)?;
                self.waiting_for = Some(Agent_A_Msg::S2((), ()));
                let outgoing = Agent_s_Msg::<T>::S1(
                    {
                        let term = self
                            .knowledge
                            .construct(base, DynTerm::Agent("A".to_string()))?;
                        base.concrete_agent(&term)?
                    },
                    {
                        let term = self
                            .knowledge
                            .construct(base, DynTerm::Agent("B".to_string()))?;
                        base.concrete_agent(&term)?
                    },
                    {
                        let term = self
                            .knowledge
                            .construct(base, DynTerm::Number("NA".to_string()))?;
                        base.concrete_number(&term)?
                    },
                    {
                        let term = self
                            .knowledge
                            .construct(base, DynTerm::Number("NB".to_string()))?;
                        base.concrete_number(&term)?
                    },
                );
                let receiver = self
                    .knowledge
                    .construct(base, DynTerm::Agent("s".to_string()))?;
                Ok(Effect::Send {
                    receiver: base.concrete_agent(&receiver)?,
                    body: base.serialize(Message::Agent_s(outgoing))?,
                })
            }
            Agent_A_Msg::S2(arg0, arg1) => {
                // Check that arg_0 matches {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s))
                self.knowledge.register(
                    DynTerm::Composition {
                        func: Func::SymEnc,
                        args: vec![
                            DynTerm::Tuple(vec![
                                DynTerm::SymmetricKey("KAB".to_string()),
                                DynTerm::Agent("B".to_string()),
                                DynTerm::Number("NA".to_string()),
                            ]),
                            DynTerm::Composition {
                                func: Func::User("sk".to_string()),
                                args: vec![
                                    DynTerm::Agent("A".to_string()),
                                    DynTerm::Agent("s".to_string()),
                                ],
                            },
                        ],
                    },
                    DynTerm::SymEnc(arg0),
                )?;
                // Check that arg_1 matches {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s)) |}sk(@Agent(B), #Agent(s))
                self.knowledge.register(
                    DynTerm::Composition {
                        func: Func::SymEnc,
                        args: vec![
                            DynTerm::Tuple(vec![
                                DynTerm::SymmetricKey("KAB".to_string()),
                                DynTerm::Agent("A".to_string()),
                                DynTerm::Number("NB".to_string()),
                                DynTerm::Agent("s".to_string()),
                            ]),
                            DynTerm::Composition {
                                func: Func::User("sk".to_string()),
                                args: vec![
                                    DynTerm::Agent("B".to_string()),
                                    DynTerm::Agent("s".to_string()),
                                ],
                            },
                        ],
                    },
                    DynTerm::SymEnc(arg1),
                )?;
                self.knowledge.decompose(base)?;
                self.waiting_for = None;
                let outgoing = Agent_B_Msg::<T>::S3({
                    let term = self.knowledge.construct(
                        base,
                        DynTerm::Composition {
                            func: Func::SymEnc,
                            args: vec![
                                DynTerm::Tuple(vec![
                                    DynTerm::SymmetricKey("KAB".to_string()),
                                    DynTerm::Agent("A".to_string()),
                                    DynTerm::Number("NB".to_string()),
                                    DynTerm::Agent("s".to_string()),
                                ]),
                                DynTerm::Composition {
                                    func: Func::User("sk".to_string()),
                                    args: vec![
                                        DynTerm::Agent("B".to_string()),
                                        DynTerm::Agent("s".to_string()),
                                    ],
                                },
                            ],
                        },
                    )?;
                    base.concrete_sym_enc(&term)?
                });
                let receiver = self
                    .knowledge
                    .construct(base, DynTerm::Agent("B".to_string()))?;
                Ok(Effect::Send {
                    receiver: base.concrete_agent(&receiver)?,
                    body: base.serialize(Message::Agent_B(outgoing))?,
                })
            }
        }
    }
}

pub struct Agent_B<T: Terms> {
    waiting_for: Option<Agent_B_Msg<()>>,
    knowledge: dolev_yao::Knowledge<T>,
}
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Agent_B_Msg<T: Terms> {
    S3(T::SymEnc),
}
impl Default for Agent_B_Msg<()> {
    fn default() -> Self {
        Agent_B_Msg::S3(())
    }
}
impl<T: Terms> Agent_B_Msg<T> {
    fn same_discriminant<S: Terms>(&self, b: &Agent_B_Msg<S>) -> bool {
        match (self, b) {
            (Agent_B_Msg::S3(_), Agent_B_Msg::S3(_)) => true,
            _ => false,
        }
    }
}
pub struct Agent_B_InitialKnowledge<T: Terms> {
    pub Agent_A_: T::Agent,
    pub Agent_B_: T::Agent,
    pub Agent_s_: T::Agent,
    pub User_sk__Agent_B__Agent_s__: T::User_sk,
}
impl<T: Terms> Agent_B_InitialKnowledge<T> {
    fn knowledge(self, base: &mut T) -> Result<dolev_yao::Knowledge<T>> {
        Ok(dolev_yao::Knowledge(
            [
                (DynTerm::Agent("A".to_string()), T::dyn_agent(self.Agent_A_)),
                (DynTerm::Agent("B".to_string()), T::dyn_agent(self.Agent_B_)),
                (DynTerm::Agent("s".to_string()), T::dyn_agent(self.Agent_s_)),
                (
                    DynTerm::Composition {
                        func: Func::User("sk".to_string()),
                        args: vec![
                            DynTerm::Agent("B".to_string()),
                            DynTerm::Agent("s".to_string()),
                        ],
                    },
                    DynTerm::Custom(base.serialize(self.User_sk__Agent_B__Agent_s__)?),
                ),
            ]
            .into_iter()
            .collect(),
        ))
    }
}
impl<T: Terms> Agent_B<T> {
    pub fn new(base: &mut T, initial_knowledge: Agent_B_InitialKnowledge<T>) -> Result<Self> {
        Ok(Self {
            waiting_for: Some(Default::default()),
            knowledge: initial_knowledge.knowledge(base)?,
        })
    }
    pub fn pump(&mut self, base: &mut T, msg: Agent_B_Msg<T>) -> Result<Effect<T>> {
        println!("{msg:?}");
        let waiting_for = if let Some(waiting_for) = &self.waiting_for {
            waiting_for
        } else {
            bail!("My session has ended!")
        };
        if !msg.same_discriminant(&waiting_for) {
            bail!("Oh noes! Message received out of order!");
        }
        match msg {
            Agent_B_Msg::S3(arg0) => {
                // Check that arg_0 matches {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s)) |}sk(@Agent(B), #Agent(s))
                self.knowledge.register(
                    DynTerm::Composition {
                        func: Func::SymEnc,
                        args: vec![
                            DynTerm::Tuple(vec![
                                DynTerm::SymmetricKey("KAB".to_string()),
                                DynTerm::Agent("A".to_string()),
                                DynTerm::Number("NB".to_string()),
                                DynTerm::Agent("s".to_string()),
                            ]),
                            DynTerm::Composition {
                                func: Func::User("sk".to_string()),
                                args: vec![
                                    DynTerm::Agent("B".to_string()),
                                    DynTerm::Agent("s".to_string()),
                                ],
                            },
                        ],
                    },
                    DynTerm::SymEnc(arg0),
                )?;
                self.knowledge.decompose(base)?;
                self.waiting_for = None;
                Ok(Effect::Finished)
            }
        }
    }
}

pub struct Agent_s<T: Terms> {
    waiting_for: Option<Agent_s_Msg<()>>,
    knowledge: dolev_yao::Knowledge<T>,
}
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Agent_s_Msg<T: Terms> {
    S1(T::Agent, T::Agent, T::Number, T::Number),
}
impl Default for Agent_s_Msg<()> {
    fn default() -> Self {
        Agent_s_Msg::S1((), (), (), ())
    }
}
impl<T: Terms> Agent_s_Msg<T> {
    fn same_discriminant<S: Terms>(&self, b: &Agent_s_Msg<S>) -> bool {
        match (self, b) {
            (Agent_s_Msg::S1(_, _, _, _), Agent_s_Msg::S1(_, _, _, _)) => true,
            _ => false,
        }
    }
}
pub struct Agent_s_InitialKnowledge<T: Terms> {
    pub Agent_A_: T::Agent,
    pub Agent_B_: T::Agent,
    pub Agent_s_: T::Agent,
    pub User_sk__Agent_A__Agent_s__: T::User_sk,
    pub User_sk__Agent_B__Agent_s__: T::User_sk,
}
impl<T: Terms> Agent_s_InitialKnowledge<T> {
    fn knowledge(self, base: &mut T) -> Result<dolev_yao::Knowledge<T>> {
        Ok(dolev_yao::Knowledge(
            [
                (DynTerm::Agent("A".to_string()), T::dyn_agent(self.Agent_A_)),
                (DynTerm::Agent("B".to_string()), T::dyn_agent(self.Agent_B_)),
                (DynTerm::Agent("s".to_string()), T::dyn_agent(self.Agent_s_)),
                (
                    DynTerm::Composition {
                        func: Func::User("sk".to_string()),
                        args: vec![
                            DynTerm::Agent("A".to_string()),
                            DynTerm::Agent("s".to_string()),
                        ],
                    },
                    DynTerm::Custom(base.serialize(self.User_sk__Agent_A__Agent_s__)?),
                ),
                (
                    DynTerm::Composition {
                        func: Func::User("sk".to_string()),
                        args: vec![
                            DynTerm::Agent("B".to_string()),
                            DynTerm::Agent("s".to_string()),
                        ],
                    },
                    DynTerm::Custom(base.serialize(self.User_sk__Agent_B__Agent_s__)?),
                ),
            ]
            .into_iter()
            .collect(),
        ))
    }
}
impl<T: Terms> Agent_s<T> {
    pub fn new(
        base: &mut T,
        f: impl Fn(&T::Agent, &T::Agent, &T::Number, &T::Number) -> Agent_s_InitialKnowledge<T>,
        msg: (T::Agent, T::Agent, T::Number, T::Number),
    ) -> Result<(Self, Effect<T>)> {
        let mut agent = Self {
            waiting_for: Some(Default::default()),
            knowledge: f(&msg.0, &msg.1, &msg.2, &msg.3).knowledge(base)?,
        };
        let effect = agent.pump(base, Agent_s_Msg::S1(msg.0, msg.1, msg.2, msg.3))?;
        Ok((agent, effect))
    }
    pub fn pump(&mut self, base: &mut T, msg: Agent_s_Msg<T>) -> Result<Effect<T>> {
        println!("{msg:?}");
        let waiting_for = if let Some(waiting_for) = &self.waiting_for {
            waiting_for
        } else {
            bail!("My session has ended!")
        };
        if !msg.same_discriminant(&waiting_for) {
            bail!("Oh noes! Message received out of order!");
        }
        match msg {
            Agent_s_Msg::S1(arg0, arg1, arg2, arg3) => {
                // Check that arg_0 matches @Agent(A)
                self.knowledge
                    .register(DynTerm::Agent("A".to_string()), T::dyn_agent(arg0))?;
                // Check that arg_1 matches @Agent(B)
                self.knowledge
                    .register(DynTerm::Agent("B".to_string()), T::dyn_agent(arg1))?;
                // Check that arg_2 matches @Number(NA)
                self.knowledge
                    .register(DynTerm::Number("NA".to_string()), T::dyn_number(arg2))?;
                // Check that arg_3 matches @Number(NB)
                self.knowledge
                    .register(DynTerm::Number("NB".to_string()), T::dyn_number(arg3))?;
                self.knowledge.decompose(base)?;
                self.waiting_for = None;
                let outgoing = Agent_A_Msg::<T>::S2(
                    {
                        let term = self.knowledge.construct(
                            base,
                            DynTerm::Composition {
                                func: Func::SymEnc,
                                args: vec![
                                    DynTerm::Tuple(vec![
                                        DynTerm::SymmetricKey("KAB".to_string()),
                                        DynTerm::Agent("B".to_string()),
                                        DynTerm::Number("NA".to_string()),
                                    ]),
                                    DynTerm::Composition {
                                        func: Func::User("sk".to_string()),
                                        args: vec![
                                            DynTerm::Agent("A".to_string()),
                                            DynTerm::Agent("s".to_string()),
                                        ],
                                    },
                                ],
                            },
                        )?;
                        base.concrete_sym_enc(&term)?
                    },
                    {
                        let term = self.knowledge.construct(
                            base,
                            DynTerm::Composition {
                                func: Func::SymEnc,
                                args: vec![
                                    DynTerm::Tuple(vec![
                                        DynTerm::SymmetricKey("KAB".to_string()),
                                        DynTerm::Agent("A".to_string()),
                                        DynTerm::Number("NB".to_string()),
                                        DynTerm::Agent("s".to_string()),
                                    ]),
                                    DynTerm::Composition {
                                        func: Func::User("sk".to_string()),
                                        args: vec![
                                            DynTerm::Agent("B".to_string()),
                                            DynTerm::Agent("s".to_string()),
                                        ],
                                    },
                                ],
                            },
                        )?;
                        base.concrete_sym_enc(&term)?
                    },
                );
                let receiver = self
                    .knowledge
                    .construct(base, DynTerm::Agent("A".to_string()))?;
                Ok(Effect::Send {
                    receiver: base.concrete_agent(&receiver)?,
                    body: base.serialize(Message::Agent_A(outgoing))?,
                })
            }
        }
    }
}

mod dolev_yao {
    use std::collections::HashMap;

    use anyhow::{bail, Context};
    use omg::{Base, DynTerm, Func, Pattern};

    pub struct Knowledge<T: Base>(pub HashMap<DynTerm<Pattern>, DynTerm<T>>);

    impl<T: super::Terms> Knowledge<T> {
        pub fn register(
            &mut self,
            pattern: DynTerm<Pattern>,
            concrete: DynTerm<T>,
        ) -> anyhow::Result<()> {
            if let Some(old) = self.0.get(&pattern) {
                if old != &concrete {
                    println!("{:?}", self.0);
                    bail!("oh noooo! {:?} != {:?}", old, concrete)
                }
            } else {
                self.0.insert(pattern, concrete);
            }

            Ok(())
        }
        pub fn decompose(&mut self, base: &mut T) -> anyhow::Result<()> {
            loop {
                let mut decompositions = Vec::new();

                for (term, concrete) in self.0.clone().iter() {
                    match term {
                        DynTerm::Composition { func, args } => match func {
                            Func::SymEnc => {
                                let body = &args[0];
                                let key = &args[0];

                                if let Ok(key) = self.construct(base, key.clone()) {
                                    match concrete {
                                        DynTerm::Agent(_) => todo!(),
                                        DynTerm::SymmetricKey(_) => todo!(),
                                        DynTerm::AsymmetricKey(_) => todo!(),
                                        DynTerm::Number(_) => todo!(),
                                        DynTerm::Constant(_) => todo!(),
                                        DynTerm::SymEnc(_) => todo!(),
                                        DynTerm::AsymEnc(_) => todo!(),
                                        DynTerm::Composition { func, args } => todo!(),
                                        DynTerm::Tuple(_) => todo!(),
                                        DynTerm::Custom(_) => todo!(),
                                    }
                                    // todo!()
                                } else {
                                    continue;
                                }
                            }
                            Func::AsymEnc => todo!(),
                            Func::Exp => todo!(),
                            Func::Inv => todo!(),
                            Func::User(func) => {
                                match func.as_str() {
                                    "sk" => {
                                        continue;
                                    }
                                    _ => todo!(),
                                };
                            }
                        },
                        DynTerm::Tuple(_) => todo!(),
                        _ => {}
                    }
                }

                if decompositions.is_empty() {
                    break;
                }

                for (term, concrete) in decompositions {
                    self.0.insert(term, concrete);
                }
            }

            Ok(())
        }
        pub fn construct(
            &mut self,
            base: &mut T,
            pattern: DynTerm<Pattern>,
        ) -> anyhow::Result<DynTerm<T>> {
            if let Some(bandit) = self.0.get(&pattern).cloned() {
                return Ok(bandit);
            }

            match pattern {
                DynTerm::Number(_) => {
                    let ayo = T::dyn_number(base.generate_nonce());
                    self.0.insert(pattern, ayo.clone());
                    Ok(ayo)
                }
                DynTerm::SymmetricKey(_) => {
                    let ayo = T::dyn_symmetric_key(base.generate_sym_key());
                    self.0.insert(pattern, ayo.clone());
                    Ok(ayo)
                }
                DynTerm::Composition { func, args } => match func {
                    Func::SymEnc => {
                        let body = &args[0];
                        let key = &args[1];

                        let body = self.construct(base, body.clone())?;
                        let key = self.construct(base, key.clone())?;

                        let body = base.serialize(body)?;
                        let key = match key {
                            DynTerm::Custom(key) => key,
                            key => base.serialize(key)?,
                        };

                        Ok(DynTerm::SymEnc(base.symmetric_encrypt(body, key)?))
                    }
                    Func::AsymEnc => todo!(),
                    Func::Exp => todo!(),
                    Func::Inv => todo!(),
                    Func::User(_) => todo!(),
                },
                DynTerm::Tuple(ts) => {
                    let ts: anyhow::Result<Vec<_>> =
                        ts.into_iter().map(|t| self.construct(base, t)).collect();
                    let ts = ts?;

                    Ok(DynTerm::Tuple(ts))
                }
                _ => {
                    todo!("Construct {:#?} using {:#?}", pattern, self.0)
                }
            }
        }
    }
}
