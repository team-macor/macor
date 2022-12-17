use std::marker::PhantomData;

use anyhow::Context;
use omg::{comm::TcpChannel, terms::*, Base};
use omg_crypt::RingBase;

mod test;
use test as proto;

impl proto::Terms for RingBase {
    type User_sk = String;
}

fn main() -> anyhow::Result<()> {
    use proto::*;

    tracing_subscriber::fmt::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .without_time()
        .init();

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
