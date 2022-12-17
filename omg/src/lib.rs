#![feature(trait_alias)]

pub mod comm;
pub mod terms;

pub use comm::Channel;

use anyhow::Result;

pub trait Stuff =
    Clone + std::fmt::Debug + PartialEq + Eq + serde::Serialize + serde::de::DeserializeOwned;

pub trait AsymmetricKey<B: Base> {
    type Inv: AsymmetricKeyInv<B, Inner = Self>;
}
pub trait AsymmetricKeyInv<B: Base> {
    type Inner: AsymmetricKey<B, Inv = Self>;
}

impl AsymmetricKey<()> for () {
    type Inv = ();
}
impl AsymmetricKeyInv<()> for () {
    type Inner = ();
}

pub trait Base: Sized + std::fmt::Debug {
    type Agent: Stuff + std::hash::Hash;
    type SymmetricKey: Stuff;
    type AsymmetricKey: Stuff + AsymmetricKey<Self>;
    type AsymmetricKeyInv: Stuff + AsymmetricKeyInv<Self>;
    type Number: Stuff;
    type Const: Stuff;
    type Exp;
    type AsymEnc: Stuff;
    type SymEnc: Stuff;

    fn symmetric_encrypt<B: serde::Serialize, K: serde::Serialize + std::fmt::Debug>(
        &mut self,
        body: &B,
        key: &K,
    ) -> Result<Self::SymEnc>;
    fn asymmetric_encrypt<B: serde::Serialize>(
        &mut self,
        body: &B,
        key: &Self::AsymmetricKey,
    ) -> Result<Self::AsymEnc>;
    fn symmetric_dencrypt<K: serde::Serialize + std::fmt::Debug, P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::SymEnc,
        key: &K,
    ) -> Result<P>;
    fn asymmetric_dencrypt<P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::AsymEnc,
        key: &Self::AsymmetricKeyInv,
    ) -> Result<P>;

    fn generate_nonce(&mut self) -> Self::Number;
    fn generate_sym_key(&mut self) -> Self::SymmetricKey;
}

impl Base for () {
    type Agent = ();
    type SymmetricKey = ();
    type AsymmetricKey = ();
    type AsymmetricKeyInv = ();
    type Number = ();
    type Const = ();
    type Exp = ();
    type AsymEnc = ();
    type SymEnc = ();

    fn symmetric_encrypt<B: serde::Serialize, K: serde::Serialize>(
        &mut self,
        body: &B,
        key: &K,
    ) -> Result<Self::SymEnc> {
        todo!()
    }

    fn asymmetric_encrypt<B: serde::Serialize>(
        &mut self,
        body: &B,
        key: &Self::AsymmetricKey,
    ) -> Result<Self::AsymEnc> {
        todo!()
    }

    fn symmetric_dencrypt<K: serde::Serialize, P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::SymEnc,
        key: &K,
    ) -> Result<P> {
        todo!()
    }

    fn asymmetric_dencrypt<P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::AsymEnc,
        key: &Self::AsymmetricKeyInv,
    ) -> Result<P> {
        todo!()
    }

    fn generate_nonce(&mut self) -> Self::Number {
        todo!()
    }

    fn generate_sym_key(&mut self) -> Self::SymmetricKey {
        todo!()
    }
}
