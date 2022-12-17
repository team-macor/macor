use anyhow::Context;
use educe::Educe;
use omg::Base;
use omg::{AsymmetricKey, AsymmetricKeyInv};
use ring::aead;
use ring::pbkdf2;
use ring::rand::SystemRandom;
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;
use std::num::NonZeroU32;
use tracing::debug;

#[derive(Debug)]
pub struct RingBase {
    rng: SystemRandom,
    salt: [u8; 32],
}

impl RingBase {
    pub fn new() -> Self {
        let rng = SystemRandom::new();
        Self {
            rng,
            salt: [42; 32],
        }
    }

    fn derive_sym_key<K: serde::Serialize>(&self, key: &K) -> anyhow::Result<aead::UnboundKey> {
        debug!("derive sym key");
        let key_bytes = postcard::to_allocvec(key)?;

        let mut key = vec![0; 32];
        pbkdf2::derive(
            pbkdf2::PBKDF2_HMAC_SHA256,
            NonZeroU32::new(100_000).unwrap(),
            &self.salt,
            &key_bytes,
            &mut key,
        );

        let key: [u8; 32] = key.try_into().unwrap();

        Ok(aead::UnboundKey::new(&aead::AES_256_GCM, &key)
            .with_context(|| "unbound key aead::AES_256_GCM")?)
    }
}

const NUMBER_SIZE: usize = 12;
// pub trait Stuff = Clone
//   + std::fmt::Debug
//   + PartialEq
//   + Eq
//   + serde::Serialize
//   + serde::de::DeserializeOwned;
#[derive(Debug, Serialize, Deserialize, Educe)]
#[educe(PartialEq(bound = "B: Sized"))]
#[educe(Eq(bound = "B: Sized"))]
#[educe(Clone(bound = "B: Sized"))]
pub struct RsaPublicKey<B> {
    inner: rsa::RsaPublicKey,
    marker: PhantomData<B>,
}

#[derive(Debug, Serialize, Deserialize, Educe)]
#[educe(PartialEq(bound = "B: Sized"))]
#[educe(Eq(bound = "B: Sized"))]
#[educe(Clone(bound = "B: Sized"))]
pub struct RsaPrivateKey<B> {
    inner: rsa::RsaPrivateKey,
    marker: PhantomData<B>,
}

impl<B: Base> AsymmetricKey<B> for RsaPublicKey<B> {
    type Inv = RsaPrivateKey<B>;
}
impl<B: Base> AsymmetricKeyInv<B> for RsaPrivateKey<B> {
    type Inner = RsaPublicKey<B>;
}

impl Base for RingBase {
    type Agent = String;
    type SymmetricKey = [u8; 32];
    type AsymmetricKey = RsaPublicKey<Self>;
    type AsymmetricKeyInv = RsaPrivateKey<Self>;
    type Number = [u8; NUMBER_SIZE];
    type Exp = ();
    type Const = ();
    type AsymEnc = (Self::Number, Vec<u8>);
    type SymEnc = (Self::Number, Vec<u8>);

    fn generate_nonce(&mut self) -> Self::Number {
        debug!("generate nonce");
        const L: usize = NUMBER_SIZE.next_power_of_two();
        let n: [u8; L] = ring::rand::generate(&self.rng).unwrap().expose();
        n[..NUMBER_SIZE].try_into().unwrap()
    }

    fn generate_sym_key(&mut self) -> Self::SymmetricKey {
        debug!("generate sym key");
        ring::rand::generate(&self.rng).unwrap().expose()
    }

    fn symmetric_encrypt<B: serde::Serialize, K: serde::Serialize + std::fmt::Debug>(
        &mut self,
        body: &B,
        key: &K,
    ) -> anyhow::Result<Self::SymEnc> {
        let mut body = postcard::to_allocvec(body)?;
        let ukey = self.derive_sym_key(&key)?;
        debug!("symmetric encrypt with key {key:?} ({ukey:?})");

        let sym_enc = aead::LessSafeKey::new(ukey);

        let nonce_bytes = self.generate_nonce();
        let nonce = aead::Nonce::try_assume_unique_for_key(&nonce_bytes).unwrap();

        sym_enc.seal_in_place_append_tag(nonce, aead::Aad::empty(), &mut body)?;

        Ok((nonce_bytes, body))
    }

    fn asymmetric_encrypt<B: serde::Serialize>(
        &mut self,
        body: &B,
        key: &Self::AsymmetricKey,
    ) -> anyhow::Result<Self::AsymEnc> {
        todo!()
    }

    fn symmetric_dencrypt<K: serde::Serialize + std::fmt::Debug, P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::SymEnc,
        key: &K,
    ) -> anyhow::Result<P> {
        let (nonce, mut body) = body.clone();

        let ukey = self.derive_sym_key(&key)?;
        debug!("symmetric decrypt with key {key:?} ({ukey:?})");
        let sym_enc = aead::LessSafeKey::new(ukey);
        let nonce = aead::Nonce::try_assume_unique_for_key(&nonce)
            .with_context(|| "Nonce::try_assume_unique_for_key")
            .unwrap();

        let in_out = sym_enc
            .open_in_place(nonce, aead::Aad::empty(), &mut body)
            .with_context(|| "open_in_place")?;

        Ok(postcard::from_bytes(in_out)?)
    }

    fn asymmetric_dencrypt<P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::AsymEnc,
        key: &Self::AsymmetricKeyInv,
    ) -> anyhow::Result<P> {
        todo!()
    }
}
