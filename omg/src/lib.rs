#![feature(trait_alias)]

pub mod comm;
pub mod terms;

use std::fmt::Debug;

pub use comm::Channel;

use anyhow::Result;
use serde::{de::DeserializeOwned, Serialize};

pub trait Term = Clone + Debug + PartialEq + Eq + Serialize + DeserializeOwned;

pub trait AsymKey<B: Base> {
    type Inv: AsymKeyInv<B, Inner = Self>;
}
pub trait AsymKeyInv<B: Base> {
    type Inner: AsymKey<B, Inv = Self>;
}

impl AsymKey<()> for () {
    type Inv = ();
}
impl AsymKeyInv<()> for () {
    type Inner = ();
}

pub trait Base: Sized {
    type Agent: Term;
    type Number: Term;
    type SymKey: Term;
    type SymEnc: Term;
    type AsymKey: Term + AsymKey<Self>;
    type AsymKeyInv: Term + AsymKeyInv<Self>;
    type AsymEnc: Term;

    fn sym_encrypt<B: Serialize, K: Serialize + Debug>(
        &mut self,
        body: &B,
        key: &K,
    ) -> Result<Self::SymEnc>;
    fn sym_decrypt<K: Serialize + Debug, P: DeserializeOwned>(
        &mut self,
        body: &Self::SymEnc,
        key: &K,
    ) -> Result<P>;
    fn asym_encrypt<B: Serialize>(
        &mut self,
        body: &B,
        key: &Self::AsymKey,
    ) -> Result<Self::AsymEnc>;
    fn asym_decrypt<P: DeserializeOwned>(
        &mut self,
        body: &Self::AsymEnc,
        key: &Self::AsymKeyInv,
    ) -> Result<P>;

    fn gen_nonce(&mut self) -> Self::Number;
    fn gen_sym_key(&mut self) -> Self::SymKey;
}

impl Base for () {
    type Agent = ();
    type Number = ();
    type SymKey = ();
    type SymEnc = ();
    type AsymKey = ();
    type AsymKeyInv = ();
    type AsymEnc = ();

    fn sym_encrypt<B: Serialize, K: Serialize>(
        &mut self,
        body: &B,
        key: &K,
    ) -> Result<Self::SymEnc> {
        Ok(())
    }

    fn sym_decrypt<K: Serialize, P: DeserializeOwned>(
        &mut self,
        body: &Self::SymEnc,
        key: &K,
    ) -> Result<P> {
        todo!()
    }

    fn asym_encrypt<B: Serialize>(
        &mut self,
        body: &B,
        key: &Self::AsymKey,
    ) -> Result<Self::AsymEnc> {
        Ok(())
    }

    fn asym_decrypt<P: DeserializeOwned>(
        &mut self,
        body: &Self::AsymEnc,
        key: &Self::AsymKeyInv,
    ) -> Result<P> {
        todo!()
    }

    fn gen_nonce(&mut self) -> Self::Number {
        todo!()
    }

    fn gen_sym_key(&mut self) -> Self::SymKey {
        todo!()
    }
}
