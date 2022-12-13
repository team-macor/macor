use std::collections::HashMap;

use anyhow::{bail, Context};
use omg::{Base, DynTerm, Func, Pattern};

pub struct Knowledge<T: Base>(pub HashMap<DynTerm<Pattern>, DynTerm<T>>);

impl<T: Base> Knowledge<T> {
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
                            todo!("TEMPLATE FOR FUNCTION COMPOSITION");
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
