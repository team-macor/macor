use std::marker::PhantomData;

use educe::Educe;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Agent<A>(pub A);
#[derive(Educe, Serialize, Deserialize)]
#[educe(Clone(bound = "F: Clone"))]
#[educe(PartialEq(bound = "F: PartialEq"))]
#[educe(Eq(bound = "F: Eq"))]
#[serde(bound = "F: serde::Serialize + serde::de::DeserializeOwned")]
pub struct Func<F, A>(pub F, pub PhantomData<A>);
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Number<N>(pub N);
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Tuple<Inner>(pub Inner);
#[derive(Educe, Serialize, Deserialize)]
#[educe(Clone(bound = "P: Clone"))]
#[educe(PartialEq(bound = "P: PartialEq"))]
#[educe(Eq(bound = "P: Eq"))]
#[serde(bound = "P: serde::Serialize + serde::de::DeserializeOwned")]
pub struct SymEnc<P, Body, Key>(pub P, pub PhantomData<(Body, Key)>);
#[derive(Educe, Serialize, Deserialize)]
#[educe(Clone(bound = "P: Clone"))]
#[educe(PartialEq(bound = "P: PartialEq"))]
#[educe(Eq(bound = "P: Eq"))]
#[serde(bound = "P: serde::Serialize + serde::de::DeserializeOwned")]
pub struct AsymEnc<P, Body, Key>(pub P, pub PhantomData<(Body, Key)>);
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SymKey<S>(pub S);

#[derive(Educe, Serialize, Deserialize)]
#[educe(Debug(bound = "K: std::fmt::Debug"))]
#[educe(Clone(bound = "K: Clone"))]
#[educe(PartialEq(bound = "K: PartialEq"))]
#[educe(Eq(bound = "K: Eq"))]
#[serde(bound = "K: serde::Serialize + serde::de::DeserializeOwned")]
pub struct AsymKey<K, A>(pub K, pub PhantomData<A>);
#[derive(Educe, Serialize, Deserialize)]
#[educe(Debug(bound = "S: std::fmt::Debug"))]
#[educe(Clone(bound = "S: Clone"))]
#[educe(PartialEq(bound = "S: PartialEq"))]
#[educe(Eq(bound = "S: Eq"))]
#[serde(bound = "S: serde::Serialize + serde::de::DeserializeOwned")]
pub struct Inv<S, A>(pub S, pub PhantomData<A>);

impl<P, Body, Key> std::fmt::Debug for SymEnc<P, Body, Key>
where
    P: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SymEnc(...)")
    }
}
impl<P, Body, Key> std::fmt::Debug for AsymEnc<P, Body, Key>
where
    P: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsymEnc(...)")
    }
}
impl<F, A> std::fmt::Debug for Func<F, A>
where
    F: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Func({:?})", self.0)
    }
}
