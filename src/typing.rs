use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use macor_parse::ast::Ident;
use miette::SourceSpan;

use crate::protocol::{AgentName, Constant, Func, Term, Variable};

pub trait Stage: PartialEq + Eq + std::fmt::Debug + Clone + PartialOrd + Ord + Hash {
    type Constant: PartialEq + Eq + std::fmt::Debug + Clone + PartialOrd + Ord + Hash;
    type Variable: PartialEq + Eq + std::fmt::Debug + Clone + PartialOrd + Ord + Hash;
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct UntypedStage<'s> {
    _data: PhantomData<&'s ()>,
}

impl<'a> Stage for UntypedStage<'a> {
    type Constant = !;
    type Variable = Ident<&'a str>;
}

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct TypedStage;

impl Stage for TypedStage {
    type Constant = Constant;
    type Variable = Variable;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Agent,
    SymmetricKey,
    Number,
    Function,
}

#[derive(Debug, PartialEq, Eq, thiserror::Error, miette::Diagnostic, Clone)]
pub enum TypingError {
    #[error("Symmetric key '{name}' cannot be constant")]
    #[diagnostic()]
    ConstantSymmetricKey {
        #[source_code]
        src: String,
        name: String,
        #[label("This symmetric key must have an uppercase name")]
        err_span: SourceSpan,
    },
    #[error("Nonce '{name}' cannot be constant")]
    #[diagnostic()]
    ConstantNonce {
        #[source_code]
        src: String,
        name: String,
        #[label("This nonce must have an uppercase name")]
        err_span: SourceSpan,
    },
    #[error("Function '{name}' cannot be variable")]
    #[diagnostic()]
    FunctionAsVariable {
        #[source_code]
        src: String,
        name: String,
        #[label("This function must be lowercase")]
        err_span: SourceSpan,
    },
    #[error(
        "The {} '{name}' has not been given a type",
        if name.starts_with(|c: char| c.is_lowercase()) { "constant" } else { "variable" }
    )]
    #[diagnostic()]
    NotDeclared {
        #[source_code]
        src: String,
        name: String,
        #[label("All variables and constant must have been given a type")]
        err_span: SourceSpan,
    },
    #[error("Function {func} is not a function")]
    #[diagnostic()]
    NotAFunction {
        #[source_code]
        src: String,
        func: String,
        actual_ty: Type,
        #[label("The actual type is {actual_ty:?}")]
        err_span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypingContext {
    pub src: String,
    pub types: HashMap<String, Type>,
    pub errors: Vec<TypingError>,
}
impl TypingContext {
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.types.get(name).cloned()
    }

    fn src(&self) -> String {
        format!("{}\n", self.src)
    }
}

impl Term<UntypedStage<'_>> {
    pub fn to_typed(&self, ctx: &mut TypingContext) -> Term {
        match self {
            Term::Variable(name) => {
                // NOTE: Built-ins
                match name.as_str() {
                    "inv" => {
                        // TODO
                        return Term::Constant(Constant::Function(Func::Inv));
                    }
                    "exp" => {
                        // TODO
                        return Term::Constant(Constant::Function(Func::Exp));
                    }
                    _ => {}
                }

                let is_constant = name.is_constant();

                if let Some(ty) = ctx.lookup(name.as_str()) {
                    match ty {
                        Type::Agent => {
                            if is_constant {
                                Term::Constant(Constant::Agent(AgentName(name.convert())))
                            } else {
                                Term::Variable(Variable::Agent(AgentName(name.convert())))
                            }
                        }
                        Type::SymmetricKey => {
                            if name.is_constant() {
                                ctx.errors.push(TypingError::ConstantSymmetricKey {
                                    src: ctx.src(),
                                    name: name.to_string(),
                                    err_span: name.span(),
                                });
                            }
                            Term::Variable(Variable::SymmetricKey(name.convert()))
                        }
                        Type::Number => {
                            if name.is_constant() {
                                ctx.errors.push(TypingError::ConstantNonce {
                                    src: ctx.src(),
                                    name: name.to_string(),
                                    err_span: name.span(),
                                });
                            }
                            Term::Variable(Variable::Number(name.convert()))
                        }
                        Type::Function => {
                            if name.is_variable() {
                                ctx.errors.push(TypingError::FunctionAsVariable {
                                    src: ctx.src(),
                                    name: name.to_string(),
                                    err_span: name.span(),
                                });
                            }
                            Term::Constant(Constant::Function(Func::User(name.convert())))
                        }
                    }
                } else {
                    // panic!("name '{name}' not found in {ctx:?}");
                    ctx.errors.push(TypingError::NotDeclared {
                        src: ctx.src(),
                        name: name.to_string(),
                        err_span: name.span(),
                    });
                    Term::Variable(Variable::Agent(AgentName(name.convert())))
                }
            }
            Term::Constant(_) => unreachable!("Constant in untyped contains ! type"),
            // TODO: Check that func is defined as a function
            // TODO: Check that it is implemented correctly :)
            Term::Composition { func, args } => match func {
                Func::SymEnc | Func::AsymEnc | Func::Exp => {
                    if args.len() != 2 {
                        todo!()
                    }

                    Term::Composition {
                        func: func.clone(),
                        args: args.iter().map(|x| x.to_typed(ctx)).collect(),
                    }
                }
                Func::Inv => {
                    if args.len() != 1 {
                        todo!("{args:?}")
                    }

                    Term::Composition {
                        func: func.clone(),
                        args: args.iter().map(|x| x.to_typed(ctx)).collect(),
                    }
                }
                Func::User(name) => {
                    if let Some(ty) = ctx.lookup(name.as_str()) {
                        match ty {
                            Type::Function => {}
                            _ => {
                                ctx.errors.push(TypingError::NotAFunction {
                                    src: ctx.src(),
                                    func: name.to_string(),
                                    actual_ty: ty,
                                    err_span: name.span(),
                                });
                            }
                        }
                    } else {
                        ctx.errors.push(TypingError::NotDeclared {
                            src: ctx.src(),
                            name: name.to_string(),
                            err_span: name.span(),
                        });
                    }

                    Term::Composition {
                        func: Func::User(name.clone()),
                        args: args.iter().map(|x| x.to_typed(ctx)).collect(),
                    }
                }
            },
            Term::Tuple(xs) => Term::Tuple(xs.iter().map(|x| x.to_typed(ctx)).collect()),
        }
    }
}
