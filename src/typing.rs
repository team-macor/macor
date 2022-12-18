use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
    marker::PhantomData,
};

use itertools::Itertools;
use macor_parse::ast::{self, Document, Ident};
use miette::SourceSpan;
use smol_str::SmolStr;

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
    AsymmetricKey,
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
    #[error("Asymmetric key '{name}' cannot be constant")]
    #[diagnostic()]
    ConstantAsymmetricKey {
        src: String,
        name: String,
        #[label("This asymmetric key must have an uppercase name")]
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
    asymmetric_functions: HashSet<Ident<SmolStr>>,
    pub functions: HashMap<Ident<SmolStr>, (Vec<Type>, Option<Type>)>,
}

fn extract_asymmetric_functions<'s>(t: &ast::Term<&'s str>) -> HashSet<Ident<&'s str>> {
    match t {
        ast::Term::Var(_) => Default::default(),
        ast::Term::Fun(_, args) => args.iter().flat_map(extract_asymmetric_functions).collect(),
        ast::Term::SymEnc(body, key) => body
            .iter()
            .chain(std::iter::once(&**key))
            .flat_map(extract_asymmetric_functions)
            .collect(),
        ast::Term::AsymEnc(body, key) => {
            let mut inner: HashSet<_> = body
                .iter()
                .chain(std::iter::once(&**key))
                .flat_map(extract_asymmetric_functions)
                .collect();
            match &**key {
                ast::Term::Var(_) => {}
                ast::Term::Fun(f, _) => {
                    inner.insert(f.clone());
                }
                ast::Term::SymEnc(_, _) => {}
                ast::Term::AsymEnc(_, _) => {}
            }

            inner
        }
    }
}

impl TypingContext {
    pub fn new(src: String, doc: &Document<&str>) -> TypingContext {
        let from_knowledge = doc
            .knowledge
            .agents
            .iter()
            .flat_map(|a| a.1.iter().flat_map(extract_asymmetric_functions));
        let from_action = doc
            .actions
            .iter()
            .flat_map(|a| a.terms.iter().flat_map(extract_asymmetric_functions));

        Self {
            types: doc
                .types
                .iter()
                .flat_map(|(key, xs)| {
                    let ty = match key {
                        ast::TypesKey::Agent => Type::Agent,
                        ast::TypesKey::Number => Type::Number,
                        ast::TypesKey::SymmetricKey => Type::SymmetricKey,
                        ast::TypesKey::PublicKey => todo!(),
                        ast::TypesKey::Function => Type::Function,
                    };

                    xs.iter().map(move |x| (x.to_string(), ty))
                })
                .collect(),
            errors: Vec::new(),
            asymmetric_functions: from_knowledge
                .chain(from_action)
                .map(|s| s.map(Into::into))
                .collect(),
            functions: HashMap::new(),
            src,
        }
    }
    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.types.get(name).cloned()
    }

    fn src(&self) -> String {
        format!("{}\n", self.src)
    }

    pub fn functions_with_ret(&self) -> HashMap<Ident<SmolStr>, (Vec<Type>, Option<Type>)> {
        self.functions
            .iter()
            .map(|(name, (args, ret))| {
                if self.asymmetric_functions.contains(name) {
                    (name.clone(), (args.clone(), Some(Type::AsymmetricKey)))
                } else {
                    (name.clone(), (args.clone(), ret.clone()))
                }
            })
            .collect()
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
                        Type::AsymmetricKey => {
                            if name.is_constant() {
                                ctx.errors.push(TypingError::ConstantAsymmetricKey {
                                    src: ctx.src(),
                                    name: name.to_string(),
                                    err_span: name.span(),
                                });
                            }
                            todo!()
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

                    let args = args.iter().map(|x| x.to_typed(ctx)).collect_vec();

                    Term::Composition {
                        func: func.clone(),
                        args,
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
                Func::AsymKey(_) => unreachable!(),
                Func::User(name) => {
                    if let Some(ty) = ctx.lookup(name.as_str()) {
                        match ty {
                            Type::Function => {
                                // todo!("{name:?} as args: {args:?}");
                                match ctx.functions.get(&name) {
                                    Some(t) => {
                                        let (prev_args, _) = t.clone();
                                        if args.len() != prev_args.len() {
                                            todo!("args are of different length")
                                        }
                                        for (a, b) in args.iter().zip_eq(prev_args) {
                                            let ty_of_a = a.to_typed(ctx);

                                            if ty_of_a.ty() != b {
                                                todo!("args of different type")
                                            }
                                        }
                                    }
                                    None => {
                                        let args =
                                            args.iter().map(|arg| arg.to_typed(ctx).ty()).collect();
                                        ctx.functions.insert(name.clone(), (args, None));
                                    }
                                }
                            }
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

                    if ctx.asymmetric_functions.contains(name) {
                        Term::Composition {
                            func: Func::AsymKey(name.clone()),
                            args: args.iter().map(|x| x.to_typed(ctx)).collect(),
                        }
                    } else {
                        Term::Composition {
                            func: Func::User(name.clone()),
                            args: args.iter().map(|x| x.to_typed(ctx)).collect(),
                        }
                    }
                }
            },
            Term::Tuple(xs) => Term::Tuple(xs.iter().map(|x| x.to_typed(ctx)).collect()),
        }
    }
}
