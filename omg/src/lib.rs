use anyhow::Result;

#[derive(
    PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum Func {
    SymEnc,
    AsymEnc,
    Exp,
    Inv,
    User(String),
}

#[derive(PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub enum DynTerm<T: Base> {
    Agent(T::Agent),
    SymmetricKey(T::SymmetricKey),
    AsymmetricKey(T::AsymmetricKey),
    Number(T::Number),
    Constant(T::Const),
    SymEnc(T::SymEnc),
    AsymEnc(T::AsymEnc),
    Composition { func: Func, args: Vec<DynTerm<T>> },
    Tuple(Vec<DynTerm<T>>),
    Custom(T::Payload),
}

impl<T: Base> std::fmt::Debug for DynTerm<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Agent(arg0) => f.debug_tuple("Agent").field(arg0).finish(),
            Self::SymmetricKey(arg0) => f.debug_tuple("SymmetricKey").field(arg0).finish(),
            Self::AsymmetricKey(arg0) => f.debug_tuple("AsymmetricKey").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::Constant(arg0) => f.debug_tuple("Constant").field(arg0).finish(),
            Self::SymEnc(arg0) => f.debug_tuple("SymEnc").field(arg0).finish(),
            Self::AsymEnc(arg0) => f.debug_tuple("AsymEnc").field(arg0).finish(),
            Self::Composition { func, args } => f
                .debug_struct("Composition")
                .field("func", func)
                .field("args", args)
                .finish(),
            Self::Tuple(arg0) => f.debug_tuple("Tuple").field(arg0).finish(),
            Self::Custom(arg0) => f.debug_tuple("Custom").field(arg0).finish(),
        }
    }
}

impl<T: Base> Clone for DynTerm<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Agent(arg0) => Self::Agent(arg0.clone()),
            Self::SymmetricKey(arg0) => Self::SymmetricKey(arg0.clone()),
            Self::AsymmetricKey(arg0) => Self::AsymmetricKey(arg0.clone()),
            Self::Number(arg0) => Self::Number(arg0.clone()),
            Self::Constant(arg0) => Self::Constant(arg0.clone()),
            Self::SymEnc(arg0) => Self::SymEnc(arg0.clone()),
            Self::AsymEnc(arg0) => Self::AsymEnc(arg0.clone()),
            Self::Composition { func, args } => Self::Composition {
                func: func.clone(),
                args: args.clone(),
            },
            Self::Tuple(arg0) => Self::Tuple(arg0.clone()),
            Self::Custom(arg0) => Self::Custom(arg0.clone()),
        }
    }
}

impl<T: Base> PartialEq for DynTerm<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Agent(l0), Self::Agent(r0)) => l0 == r0,
            (Self::SymmetricKey(l0), Self::SymmetricKey(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Constant(l0), Self::Constant(r0)) => l0 == r0,
            (Self::SymEnc(l0), Self::SymEnc(r0)) => l0 == r0,
            (Self::AsymEnc(l0), Self::AsymEnc(r0)) => l0 == r0,
            (
                Self::Composition {
                    func: l_func,
                    args: l_args,
                },
                Self::Composition {
                    func: r_func,
                    args: r_args,
                },
            ) => l_func == r_func && l_args == r_args,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (Self::Custom(l0), Self::Custom(r0)) => l0 == r0,
            _ => false,
        }
    }
}
impl<T: Base> Eq for DynTerm<T> {}
impl<T: Base> std::hash::Hash for DynTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            DynTerm::Agent(x) => x.hash(state),
            DynTerm::SymmetricKey(x) => x.hash(state),
            DynTerm::AsymmetricKey(x) => x.hash(state),
            DynTerm::Number(x) => x.hash(state),
            DynTerm::Constant(x) => x.hash(state),
            DynTerm::SymEnc(x) => x.hash(state),
            DynTerm::AsymEnc(x) => x.hash(state),
            DynTerm::Composition { func, args } => {
                func.hash(state);
                args.hash(state);
            }
            DynTerm::Tuple(x) => x.hash(state),
            DynTerm::Custom(x) => x.hash(state),
        }
    }
}

pub trait Base: Sized + std::fmt::Debug {
    type Agent: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_agent(agent: Self::Agent) -> DynTerm<Self>;
    fn concrete_agent(&mut self, agent: &DynTerm<Self>) -> Result<Self::Agent>;
    type SymmetricKey: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_symmetric_key(symmetric_key: Self::SymmetricKey) -> DynTerm<Self>;
    fn concrete_symmetric_key(
        &mut self,
        symmetric_key: &DynTerm<Self>,
    ) -> Result<Self::SymmetricKey>;
    type AsymmetricKey: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_asymmetric_key(asymmetric_key: Self::AsymmetricKey) -> DynTerm<Self>;
    fn concrete_asymmetric_key(
        &mut self,
        asymmetric_key: &DynTerm<Self>,
    ) -> Result<Self::AsymmetricKey>;
    type Number: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_number(number: Self::Number) -> DynTerm<Self>;
    fn concrete_number(&mut self, number: &DynTerm<Self>) -> Result<Self::Number>;
    type Const: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_const(c: Self::Const) -> DynTerm<Self>;
    fn concrete_const(&mut self, c: &DynTerm<Self>) -> Result<Self::Const>;
    type SymEnc: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_sym_enc(c: Self::SymEnc) -> DynTerm<Self>;
    fn concrete_sym_enc(&mut self, c: &DynTerm<Self>) -> Result<Self::SymEnc>;
    type AsymEnc: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;
    fn dyn_asym_enc(c: Self::AsymEnc) -> DynTerm<Self>;
    fn concrete_asym_enc(&mut self, c: &DynTerm<Self>) -> Result<Self::AsymEnc>;
    type Exp;
    type Inv<K>;

    type Payload: Clone
        + std::fmt::Debug
        + std::hash::Hash
        + PartialEq
        + Eq
        + serde::Serialize
        + serde::de::DeserializeOwned;

    fn symmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::Payload,
    ) -> Result<Self::SymEnc>;
    fn asymmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::Payload,
    ) -> Result<Self::AsymEnc>;
    fn symmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::Payload,
    ) -> Result<T>;
    fn asymmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::Payload,
    ) -> Result<T>;

    fn generate_nonce(&mut self) -> Self::Number;
    fn generate_sym_key(&mut self) -> Self::SymmetricKey;
    fn serialize<S: serde::Serialize + serde::de::DeserializeOwned>(
        &mut self,
        msg: S,
    ) -> Result<Self::Payload>;
}

impl Base for () {
    type Agent = ();
    fn dyn_agent(agent: Self::Agent) -> DynTerm<Self> {
        DynTerm::Agent(agent)
    }
    fn concrete_agent(&mut self, agent: &DynTerm<Self>) -> Result<Self::Agent> {
        match agent {
            DynTerm::Agent(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected an Agent, got {:?}", a),
        }
    }
    type SymmetricKey = ();
    fn dyn_symmetric_key(symmetric_key: Self::SymmetricKey) -> DynTerm<Self> {
        DynTerm::SymmetricKey(symmetric_key)
    }
    fn concrete_symmetric_key(
        &mut self,
        symmetric_key: &DynTerm<Self>,
    ) -> Result<Self::SymmetricKey> {
        match symmetric_key {
            DynTerm::SymmetricKey(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected an SymmetricKey, got {:?}", a),
        }
    }
    type AsymmetricKey = ();
    fn dyn_asymmetric_key(asymmetric_key: Self::AsymmetricKey) -> DynTerm<Self> {
        DynTerm::AsymmetricKey(asymmetric_key)
    }
    fn concrete_asymmetric_key(
        &mut self,
        asymmetric_key: &DynTerm<Self>,
    ) -> Result<Self::AsymmetricKey> {
        match asymmetric_key {
            DynTerm::AsymmetricKey(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected an AsymmetricKey, got {:?}", a),
        }
    }
    type Number = ();
    fn dyn_number(number: Self::Number) -> DynTerm<Self> {
        DynTerm::Number(number)
    }
    fn concrete_number(&mut self, number: &DynTerm<Self>) -> Result<Self::Number> {
        match number {
            DynTerm::Number(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected an Number, got {:?}", a),
        }
    }
    type Const = ();
    fn dyn_const(c: Self::Const) -> DynTerm<Self> {
        DynTerm::Constant(c)
    }
    fn concrete_const(&mut self, c: &DynTerm<Self>) -> Result<Self::Const> {
        match c {
            DynTerm::Constant(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a Const, got {:?}", a),
        }
    }
    type SymEnc = ();
    fn dyn_sym_enc(c: Self::SymEnc) -> DynTerm<Self> {
        DynTerm::SymEnc(c)
    }
    fn concrete_sym_enc(&mut self, c: &DynTerm<Self>) -> Result<Self::SymEnc> {
        match c {
            DynTerm::SymEnc(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a SymEnc, got {:?}", a),
        }
    }
    type AsymEnc = ();
    fn dyn_asym_enc(c: Self::AsymEnc) -> DynTerm<Self> {
        DynTerm::AsymEnc(c)
    }
    fn concrete_asym_enc(&mut self, c: &DynTerm<Self>) -> Result<Self::AsymEnc> {
        match c {
            DynTerm::AsymEnc(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected an AsymEnc, got {:?}", a),
        }
    }
    type Exp = ();
    type Inv<K> = ();

    type Payload = ();

    fn generate_nonce(&mut self) -> Self::Number {
        ()
    }
    fn generate_sym_key(&mut self) -> Self::SymmetricKey {
        ()
    }

    fn serialize<S: serde::Serialize + serde::de::DeserializeOwned>(
        &mut self,
        msg: S,
    ) -> Result<Self::Payload> {
        Ok(())
    }

    fn symmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::SymmetricKey,
    ) -> Result<Self::SymEnc> {
        Ok(())
    }

    fn asymmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::AsymmetricKey,
    ) -> Result<Self::AsymEnc> {
        Ok(())
    }

    fn symmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::SymmetricKey,
    ) -> Result<T> {
        anyhow::bail!("cannot decrypt for () base")
    }

    fn asymmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::AsymmetricKey,
    ) -> Result<T> {
        anyhow::bail!("cannot decrypt for () base")
    }
}

#[derive(Debug)]
pub enum Effect<T: Base> {
    Send {
        receiver: T::Agent,
        body: T::Payload,
    },
    Finished,
}

#[derive(
    Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct Pattern;
impl Base for Pattern {
    type Agent = String;
    fn dyn_agent(agent: Self::Agent) -> DynTerm<Self> {
        DynTerm::Agent(agent)
    }
    fn concrete_agent(&mut self, agent: &DynTerm<Self>) -> Result<Self::Agent> {
        match agent {
            DynTerm::Agent(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'Agent', got {:?}", a),
        }
    }
    type SymmetricKey = String;
    fn dyn_symmetric_key(symmetric_key: Self::SymmetricKey) -> DynTerm<Self> {
        DynTerm::SymmetricKey(symmetric_key)
    }
    fn concrete_symmetric_key(
        &mut self,
        symmetric_key: &DynTerm<Self>,
    ) -> Result<Self::SymmetricKey> {
        match symmetric_key {
            DynTerm::SymmetricKey(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'SymmetricKey', got {:?}", a),
        }
    }
    type AsymmetricKey = String;
    fn dyn_asymmetric_key(asymmetric_key: Self::AsymmetricKey) -> DynTerm<Self> {
        DynTerm::AsymmetricKey(asymmetric_key)
    }
    fn concrete_asymmetric_key(
        &mut self,
        asymmetric_key: &DynTerm<Self>,
    ) -> Result<Self::AsymmetricKey> {
        match asymmetric_key {
            DynTerm::AsymmetricKey(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'AsymmetricKey', got {:?}", a),
        }
    }
    type Number = String;
    fn dyn_number(number: Self::Number) -> DynTerm<Self> {
        DynTerm::Number(number)
    }
    fn concrete_number(&mut self, number: &DynTerm<Self>) -> Result<Self::Number> {
        dbg!(match number {
            DynTerm::Number(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'Number', got {:?}", a),
        })
    }
    type Const = String;
    fn dyn_const(c: Self::Const) -> DynTerm<Self> {
        DynTerm::Constant(c)
    }
    fn concrete_const(&mut self, c: &DynTerm<Self>) -> Result<Self::Const> {
        match c {
            DynTerm::Constant(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'Const', got {:?}", a),
        }
    }
    type SymEnc = PatternEnc;
    fn dyn_sym_enc(c: Self::SymEnc) -> DynTerm<Self> {
        DynTerm::SymEnc(c)
    }
    fn concrete_sym_enc(&mut self, c: &DynTerm<Self>) -> Result<Self::SymEnc> {
        match c {
            DynTerm::SymEnc(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'SymEnc', got {:?}", a),
        }
    }
    type AsymEnc = PatternEnc;
    fn dyn_asym_enc(c: Self::AsymEnc) -> DynTerm<Self> {
        DynTerm::AsymEnc(c)
    }
    fn concrete_asym_enc(&mut self, c: &DynTerm<Self>) -> Result<Self::AsymEnc> {
        match c {
            DynTerm::AsymEnc(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'AsymEnc', got {:?}", a),
        }
    }
    type Exp = DynTerm<Self>;
    type Inv<K> = DynTerm<Self>;

    type Payload = ();

    fn generate_nonce(&mut self) -> Self::Number {
        todo!()
    }
    fn generate_sym_key(&mut self) -> Self::SymmetricKey {
        todo!()
    }

    fn symmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::Payload,
    ) -> Result<Self::SymEnc> {
        todo!()
    }
    fn asymmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::Payload,
    ) -> Result<Self::AsymEnc> {
        todo!()
    }

    fn symmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::Payload,
    ) -> Result<T> {
        todo!()
    }
    fn asymmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::Payload,
    ) -> Result<T> {
        todo!()
    }

    fn serialize<S: serde::Serialize + serde::de::DeserializeOwned>(
        &mut self,
        _: S,
    ) -> Result<Self::Payload> {
        todo!()
    }
}
#[derive(
    Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct PatternEnc(pub Box<(DynTerm<Pattern>, DynTerm<Pattern>)>);
