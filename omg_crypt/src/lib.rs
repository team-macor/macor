use anyhow::Context;
use educe::Educe;
use omg::Base;
use omg::{AsymKey, AsymKeyInv};
use ring::aead;
use ring::pbkdf2;
use ring::rand::SystemRandom;
use rsa::{PaddingScheme, PublicKey};
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;
use std::num::NonZeroU32;
use tracing::debug;

#[derive(Debug)]
pub struct RingBase {
    rng: SystemRandom,
    rng_2: rsa::rand_core::OsRng,
    salt: [u8; 32],
}

impl RingBase {
    pub fn new() -> Self {
        Self {
            rng: SystemRandom::new(),
            rng_2: Default::default(),
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

    pub fn gen_key_pair() -> anyhow::Result<(RsaPublicKey<Self>, RsaPrivateKey<Self>)> {
        let mut rng = rsa::rand_core::OsRng;

        let bits = 2048;
        let private_key =
            rsa::RsaPrivateKey::new(&mut rng, bits).expect("failed to generate a key");
        let public_key = rsa::RsaPublicKey::from(&private_key);

        Ok((
            RsaPublicKey {
                inner: public_key,
                marker: PhantomData,
            },
            RsaPrivateKey {
                inner: private_key,
                marker: PhantomData,
            },
        ))
    }
}

const NUMBER_SIZE: usize = 12;

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

impl<B: Base> AsymKey<B> for RsaPublicKey<B> {
    type Inv = RsaPrivateKey<B>;
}
impl<B: Base> AsymKeyInv<B> for RsaPrivateKey<B> {
    type Inner = RsaPublicKey<B>;
}

impl Base for RingBase {
    type Agent = String;
    type Number = [u8; NUMBER_SIZE];
    type SymKey = [u8; 32];
    type SymEnc = (Self::Number, Vec<u8>);
    type AsymKey = RsaPublicKey<Self>;
    type AsymKeyInv = RsaPrivateKey<Self>;
    type AsymEnc = Vec<u8>;

    fn sym_encrypt<B: serde::Serialize, K: serde::Serialize + std::fmt::Debug>(
        &mut self,
        body: &B,
        key: &K,
    ) -> anyhow::Result<Self::SymEnc> {
        let mut body = postcard::to_allocvec(body)?;
        let ukey = self.derive_sym_key(&key)?;
        debug!("sym encrypt with key {key:?} ({ukey:?})");

        let sym_enc = aead::LessSafeKey::new(ukey);

        let nonce_bytes = self.gen_nonce();
        let nonce = aead::Nonce::try_assume_unique_for_key(&nonce_bytes).unwrap();

        sym_enc.seal_in_place_append_tag(nonce, aead::Aad::empty(), &mut body)?;

        Ok((nonce_bytes, body))
    }

    fn sym_decrypt<K: serde::Serialize + std::fmt::Debug, P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::SymEnc,
        key: &K,
    ) -> anyhow::Result<P> {
        let (nonce, mut body) = body.clone();

        let ukey = self.derive_sym_key(&key)?;
        debug!("sym decrypt with key {key:?} ({ukey:?})");
        let sym_enc = aead::LessSafeKey::new(ukey);
        let nonce = aead::Nonce::try_assume_unique_for_key(&nonce)
            .with_context(|| "Nonce::try_assume_unique_for_key")
            .unwrap();

        let in_out = sym_enc
            .open_in_place(nonce, aead::Aad::empty(), &mut body)
            .with_context(|| "open_in_place")?;

        Ok(postcard::from_bytes(in_out)?)
    }

    fn asym_encrypt<B: serde::Serialize>(
        &mut self,
        body: &B,
        key: &Self::AsymKey,
    ) -> anyhow::Result<Self::AsymEnc> {
        let body = postcard::to_allocvec(body)?;
        let padding = PaddingScheme::new_pkcs1v15_encrypt();
        let enc_body = key
            .inner
            .encrypt(&mut self.rng_2, padding, &body[..])
            .expect("failed to encrypt");

        debug!("{key:?} and {body:?} produced {enc_body:?}");

        Ok(enc_body)
    }

    fn asym_decrypt<P: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::AsymEnc,
        key: &Self::AsymKeyInv,
    ) -> anyhow::Result<P> {
        let padding = PaddingScheme::new_pkcs1v15_encrypt();
        let body = key
            .inner
            .decrypt(padding, &body)
            .with_context(|| format!("decrypting with {key:?}"))?;
        Ok(postcard::from_bytes(&body)?)
    }

    fn gen_nonce(&mut self) -> Self::Number {
        debug!("generate nonce");
        const L: usize = NUMBER_SIZE.next_power_of_two();
        let n: [u8; L] = ring::rand::generate(&self.rng).unwrap().expose();
        n[..NUMBER_SIZE].try_into().unwrap()
    }

    fn gen_sym_key(&mut self) -> Self::SymKey {
        debug!("generate sym key");
        ring::rand::generate(&self.rng).unwrap().expose()
    }
}
