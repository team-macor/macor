use std::marker::PhantomData;

use anyhow::Context;
use clap::Parser;
use omg::{comm::TcpChannel, terms::*, Base};
use omg_crypt::RingBase;

mod asymtest;

mod test;
use test as proto;
use tracing::info;

fn sym_test() -> anyhow::Result<()> {
    impl proto::Terms for RingBase {
        type User_sk = String;

        type User_h = String;

        fn h(
            &mut self,
            arg_0: &<Self as Base>::Agent,
            arg_1: &<Self as Base>::Agent,
        ) -> Self::User_h {
            format!("md5({arg_0:?}, {arg_1:?})")
        }

        fn sk(
            &mut self,
            arg_0: &<Self as Base>::Agent,
            arg_1: &<Self as Base>::Agent,
        ) -> Self::User_sk {
            todo!()
        }
    }

    use proto::*;

    let network_mapping = |_, name: &<RingBase as Base>::Agent| match name.as_str() {
        "joe" => "0.0.0.0:8080".parse().unwrap(),
        "robert" => "0.0.0.0:8081".parse().unwrap(),
        x => todo!("addr for {x:?}"),
    };
    let joe_thread = std::thread::spawn(move || {
        let scope = tracing::span!(tracing::Level::INFO, "   Joe");
        let _enter = scope.enter();
        agent_A::listen::<_, TcpChannel<_, _>>(
            &mut RingBase::new(),
            |p| match p {
                agent_A::ListenPort::B => "0.0.0.0:8080".parse().unwrap(),
            },
            network_mapping,
            |_, m| {
                Ok((
                    m.0.clone(),
                    m.1.clone(),
                    Agent("robert".to_string()),
                    Func("Very good".to_string(), PhantomData),
                ))
            },
        )
    });
    let robert_thread = std::thread::spawn(move || {
        let scope = tracing::span!(tracing::Level::INFO, "Robert");
        let _enter = scope.enter();
        agent_s::listen::<_, TcpChannel<_, _>>(
            &mut RingBase::new(),
            |p| match p {
                agent_s::ListenPort::A => "0.0.0.0:8081".parse().unwrap(),
            },
            network_mapping,
            |_, m| {
                Ok((
                    m.0.clone(),
                    m.1.clone(),
                    Agent("robert".to_string()),
                    Func("Very good".to_string(), PhantomData),
                    Func("Super cool".to_string(), PhantomData),
                ))
            },
        )
    });
    let mike_thread = std::thread::spawn(move || {
        let scope = tracing::span!(tracing::Level::INFO, "  Mike");
        let _enter = scope.enter();
        agent_B::run::<_, TcpChannel<_, _>>(
            &mut RingBase::new(),
            |a| match a {},
            network_mapping,
            Initiate(
                Agent("joe".to_string()),
                Agent("mike".to_string()),
                Agent("robert".to_string()),
                Func("Super cool".to_string(), PhantomData),
            ),
        )
    });

    joe_thread.join().unwrap().with_context(|| "Joe")?;
    mike_thread.join().unwrap().with_context(|| "Mike")?;
    robert_thread.join().unwrap().with_context(|| "Robert")?;

    Ok(())
}
impl asymtest::Terms for RingBase {
    type User_pk = ();

    fn pk(&mut self, arg_0: &<Self as Base>::Agent) -> <Self as Base>::AsymmetricKey {
        todo!()
    }
}
fn asym_test() -> anyhow::Result<()> {
    use asymtest::*;

    info!("Generating key pair for a");
    let (pk_a, inv_a) = RingBase::gen_key_pair()?;
    info!("Generating key pair for b");
    let (pk_b, inv_b) = RingBase::gen_key_pair()?;
    info!("Done!");

    let network_mapping = |_, name: &<RingBase as Base>::Agent| match name.as_str() {
        "mike" => "0.0.0.0:8080".parse().unwrap(),
        x => todo!("addr for {x:?}"),
    };
    let a_pk_a = pk_a.clone();
    let a_pk_b = pk_b.clone();
    let joe_thread = std::thread::spawn(move || {
        let scope = tracing::span!(tracing::Level::INFO, "   Joe");
        let _enter = scope.enter();
        agent_A::run::<_, TcpChannel<_, _>>(
            &mut RingBase::new(),
            |a| match a {},
            network_mapping,
            Initiate(
                // A
                Agent("joe".to_string()),
                // B
                Agent("mike".to_string()),
                // pk(A)
                AsymmetricKey(a_pk_a, PhantomData),
                // pk(B)
                AsymmetricKey(a_pk_b, PhantomData),
                // inv(pk(A))
                Inv(inv_a, PhantomData),
            ),
        )
    });
    let mike_thread = std::thread::spawn(move || {
        let scope = tracing::span!(tracing::Level::INFO, "  Mike");
        let _enter = scope.enter();
        agent_B::listen::<_, TcpChannel<_, _>>(
            &mut RingBase::new(),
            |p| match p {
                agent_B::ListenPort::A => "0.0.0.0:8080".parse().unwrap(),
            },
            network_mapping,
            |_, m| {
                Ok((
                    // A
                    Agent("joe".to_string()),
                    // B
                    Agent("mike".to_string()),
                    // pk(A)
                    AsymmetricKey(pk_a.clone(), PhantomData),
                    // pk(B)
                    AsymmetricKey(pk_b.clone(), PhantomData),
                    // inv(pk(B))
                    Inv(inv_b.clone(), PhantomData),
                ))
            },
        )
    });

    joe_thread.join().unwrap().with_context(|| "Joe")?;
    mike_thread.join().unwrap().with_context(|| "Mike")?;

    Ok(())
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .without_time()
        .init();

    multi_agent()?;
    // asym_test()?;

    Ok(())
}

#[derive(Debug, clap::Parser)]
enum Cli {
    A {
        #[clap(short, long)]
        s: String,
        #[clap(short, long)]
        b_port: String,
    },
    B {
        name: String,
        #[clap(short, long)]
        a: String,
        #[clap(short, long)]
        s: String,
    },
    S {
        name: String,
        #[clap(short, long)]
        a_port: String,
    },
}

fn multi_agent() -> anyhow::Result<()> {
    let cli = Cli::parse();

    use proto::*;

    match cli {
        Cli::A { s, b_port } => {
            let scope = tracing::span!(tracing::Level::INFO, "A");
            let _enter = scope.enter();
            agent_A::listen::<_, TcpChannel<_, _>>(
                &mut RingBase::new(),
                |p| match p {
                    agent_A::ListenPort::B => format!("0.0.0.0:{b_port}").parse().unwrap(),
                },
                |a, _| match a {
                    ProtocolAgent::A => todo!(),
                    ProtocolAgent::B => todo!(),
                    ProtocolAgent::s => s.parse().unwrap(),
                },
                |_, m| {
                    Ok((
                        m.0.clone(),
                        m.1.clone(),
                        Agent(s.clone()),
                        Func("Very good".to_string(), PhantomData),
                    ))
                },
            )
        }
        Cli::B { name, a, s } => {
            let scope = tracing::span!(tracing::Level::INFO, "B");
            let _enter = scope.enter();
            agent_B::run::<_, TcpChannel<_, _>>(
                &mut RingBase::new(),
                |a| match a {},
                |agent, _| match agent {
                    ProtocolAgent::A => a.parse().unwrap(),
                    ProtocolAgent::B => todo!(),
                    ProtocolAgent::s => s.parse().unwrap(),
                },
                Initiate(
                    Agent(a.clone()),
                    Agent(name),
                    Agent(s.clone()),
                    Func("Super cool".to_string(), PhantomData),
                ),
            )
        }
        Cli::S { name, a_port } => {
            let scope = tracing::span!(tracing::Level::INFO, "s");
            let _enter = scope.enter();
            agent_s::listen::<_, TcpChannel<_, _>>(
                &mut RingBase::new(),
                |p| match p {
                    agent_s::ListenPort::A => format!("0.0.0.0:{a_port}").parse().unwrap(),
                },
                |a, _| match a {
                    ProtocolAgent::A => todo!(),
                    ProtocolAgent::B => todo!(),
                    ProtocolAgent::s => todo!(),
                },
                |_, m| {
                    Ok((
                        m.0.clone(),
                        m.1.clone(),
                        Agent(name.clone()),
                        Func("Very good".to_string(), PhantomData),
                        Func("Super cool".to_string(), PhantomData),
                    ))
                },
            )
        }
    }
}
