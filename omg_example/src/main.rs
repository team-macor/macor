use omg::DynTerm;

mod test;

impl test::Terms for SuperBase {
    type User_sk = String;

    fn sk(&mut self, arg_0: Self::Agent, arg_1: Self::Agent) -> Self::User_sk {
        todo!()
    }
}

// Example usage

#[derive(Debug)]
pub struct SuperBase {}

impl omg::Base for SuperBase {
    type Agent = String;
    type SymmetricKey = String;
    type AsymmetricKey = String;
    type Number = String;
    type Const = String;
    type SymEnc = String;
    type AsymEnc = String;
    type Exp = u128;
    type Inv<K> = K;

    fn dyn_agent(agent: Self::Agent) -> omg::DynTerm<Self> {
        omg::DynTerm::Agent(agent)
    }
    fn concrete_agent(&mut self, agent: &omg::DynTerm<Self>) -> anyhow::Result<Self::Agent> {
        match agent {
            DynTerm::Agent(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'Agent', got a {:?}", a),
        }
    }

    fn dyn_symmetric_key(symmetric_key: Self::SymmetricKey) -> omg::DynTerm<Self> {
        omg::DynTerm::SymmetricKey(symmetric_key)
    }
    fn concrete_symmetric_key(
        &mut self,
        symmetric_key: &omg::DynTerm<Self>,
    ) -> anyhow::Result<Self::SymmetricKey> {
        match symmetric_key {
            DynTerm::SymmetricKey(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'SymmetricKey', got a {:?}", a),
        }
    }

    fn dyn_asymmetric_key(asymmetric_key: Self::AsymmetricKey) -> omg::DynTerm<Self> {
        omg::DynTerm::AsymmetricKey(asymmetric_key)
    }
    fn concrete_asymmetric_key(
        &mut self,
        asymmetric_key: &omg::DynTerm<Self>,
    ) -> anyhow::Result<Self::AsymmetricKey> {
        match asymmetric_key {
            DynTerm::AsymmetricKey(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'AsymmetricKey', got a {:?}", a),
        }
    }

    fn dyn_number(number: Self::Number) -> omg::DynTerm<Self> {
        omg::DynTerm::Number(number)
    }
    fn concrete_number(&mut self, number: &omg::DynTerm<Self>) -> anyhow::Result<Self::Number> {
        match number {
            DynTerm::Number(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'Number', got a {:?}", a),
        }
    }

    fn dyn_const(c: Self::Const) -> omg::DynTerm<Self> {
        omg::DynTerm::Constant(c)
    }
    fn concrete_const(&mut self, c: &omg::DynTerm<Self>) -> anyhow::Result<Self::Const> {
        match c {
            DynTerm::Constant(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'Constant', got a {:?}", a),
        }
    }

    fn dyn_sym_enc(c: Self::SymEnc) -> omg::DynTerm<Self> {
        omg::DynTerm::SymEnc(c)
    }
    fn concrete_sym_enc(&mut self, c: &omg::DynTerm<Self>) -> anyhow::Result<Self::SymEnc> {
        match c {
            DynTerm::SymEnc(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'SymEnc', got a {:?}", a),
        }
    }

    fn dyn_asym_enc(c: Self::AsymEnc) -> omg::DynTerm<Self> {
        omg::DynTerm::AsymEnc(c)
    }
    fn concrete_asym_enc(&mut self, c: &omg::DynTerm<Self>) -> anyhow::Result<Self::AsymEnc> {
        match c {
            DynTerm::AsymEnc(x) => Ok(x.clone()),
            a => anyhow::bail!("Expected a 'AsymEnc', got a {:?}", a),
        }
    }

    fn generate_nonce(&mut self) -> Self::Number {
        // Picked by random dice roll
        4.to_string()
    }
    fn generate_sym_key(&mut self) -> Self::SymmetricKey {
        // Picked by random dice roll
        2.to_string()
    }

    type Payload = String;

    fn serialize<S: serde::Serialize + serde::de::DeserializeOwned>(
        &mut self,
        msg: S,
    ) -> anyhow::Result<Self::Payload> {
        Ok(serde_json::to_string_pretty(&msg)?)
    }

    fn symmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::Payload,
    ) -> anyhow::Result<Self::SymEnc> {
        Ok(serde_json::to_string(&(body, key))?)
    }

    fn asymmetric_encrypt<T: serde::Serialize>(
        &mut self,
        body: T,
        key: Self::Payload,
    ) -> anyhow::Result<Self::AsymEnc> {
        Ok(serde_json::to_string(&(body, key))?)
    }

    fn symmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::Payload,
    ) -> anyhow::Result<T> {
        Ok(serde_json::from_str(&body)?)
    }

    fn asymmetric_dencrypt<T: serde::de::DeserializeOwned>(
        &mut self,
        body: &Self::Payload,
        key: Self::Payload,
    ) -> anyhow::Result<T> {
        Ok(serde_json::from_str(&body)?)
    }
}

pub enum LeEffect {
    Send { contents: String, recipient: String },
}

// Rest of the protocol

fn main() -> anyhow::Result<()> {
    let a_name = "joe";
    let b_name = "mike";
    let s_name = "robert";

    let mut base = SuperBase {};

    let mut b = test::Agent_B::<SuperBase>::new(
        &mut base,
        test::Agent_B_InitialKnowledge {
            Agent_A_: a_name.to_string(),
            Agent_B_: b_name.to_string(),
            Agent_s_: s_name.to_string(),
            User_sk__Agent_B__Agent_s__: "poop poop".to_string(),
        },
    );

    let (mut a, a_effects) = test::Agent_A::<SuperBase>::new(
        &mut base,
        |_, _, _| test::Agent_A_InitialKnowledge {
            Agent_A_: a_name.to_string(),
            Agent_B_: b_name.to_string(),
            Agent_s_: s_name.to_string(),
            User_sk__Agent_A__Agent_s__: "wup wup".to_string(),
        },
        (a_name.to_string(), b_name.to_string(), "123".to_string()),
    )?;
    let msg = match a_effects {
        omg::Effect::Send { receiver, body } => {
            println!("{receiver} -> {body}");
            body
        }
        omg::Effect::Finished => todo!(),
    };
    let (mut s, s_effects) = test::Agent_s::<SuperBase>::new(
        &mut base,
        |_, _, _, _| test::Agent_s_InitialKnowledge {
            Agent_A_: a_name.to_string(),
            Agent_B_: b_name.to_string(),
            Agent_s_: s_name.to_string(),
            User_sk__Agent_A__Agent_s__: "wup wup".to_string(),
            User_sk__Agent_B__Agent_s__: "poop poop".to_string(),
        },
        match serde_json::from_str::<test::Message<SuperBase>>(&msg)? {
            test::Message::Agent_A(_) => todo!(),
            test::Message::Agent_B(_) => todo!(),
            test::Message::Agent_s(msg) => match msg {
                test::Agent_s_Msg::S1(m1, m2, m3, m4) => (m1, m2, m3, m4),
            },
        },
    )?;
    let msg = match s_effects {
        omg::Effect::Send { receiver, body } => {
            println!("{receiver} -> {body}");
            body
        }
        omg::Effect::Finished => todo!(),
    };
    let a_effect = a.pump(
        &mut base,
        match serde_json::from_str::<test::Message<SuperBase>>(&msg)? {
            test::Message::Agent_A(msg) => msg,
            test::Message::Agent_B(_) => todo!(),
            test::Message::Agent_s(_) => todo!(),
        },
    )?;

    println!("{a_effect:?}");

    Ok(())
}
