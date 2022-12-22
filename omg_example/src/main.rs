use std::marker::PhantomData;

use anyhow::Context;
use clap::Parser;
use omg::{
    comm::{TcpChannel, TcpJsonChannel},
    terms::*,
    Base,
};
use omg_crypt::RingBase;

mod asymtest;

mod test;
use test as proto;
use tracing::info;

fn sym_test() -> anyhow::Result<()> {
    impl proto::Terms for RingBase {
        type User_sk = String;

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
        agent_A::listen::<_, TcpJsonChannel<_, _>>(
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
        agent_s::listen::<_, TcpJsonChannel<_, _>>(
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
        agent_B::run::<_, TcpJsonChannel<_, _>>(
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

    fn pk(&mut self, arg_0: &<Self as Base>::Agent) -> <Self as Base>::AsymKey {
        todo!()
    }
}
fn asym_test() -> anyhow::Result<()> {
    use asymtest::*;

    const PK_A: &str = r#"{"inner":{"n":[925810435,618804571,1756943413,1108304621,3410233072,998883160,3982230569,3638863615,1153586644,916906264,3286255795,1189332720,2350968919,3250739518,2890013705,4115097247,3656954572,3650776779,1674078140,1510172961,2028838947,1438364473,2069829731,1394223969,2840423974,1116786410,993104903,3751007145,2590032329,3182248577,2960155777,3239536141,603545695,968387009,857246235,907096450,693723166,4268438359,2617485352,251219062,2352881043,516519644,1523660846,1845831645,310769870,4032565180,71511768,271547475,977486861,194912677,149063118,2180348861,1165796748,3250185009,3857179928,187624481,2871176343,1028920575,3036333112,1359553616,432221137,1467799745,2710016383,2884663208],"e":[65537]},"marker":null}"#;
    const INV_A: &str = r#"{"inner":{"pubkey_components":{"n":[925810435,618804571,1756943413,1108304621,3410233072,998883160,3982230569,3638863615,1153586644,916906264,3286255795,1189332720,2350968919,3250739518,2890013705,4115097247,3656954572,3650776779,1674078140,1510172961,2028838947,1438364473,2069829731,1394223969,2840423974,1116786410,993104903,3751007145,2590032329,3182248577,2960155777,3239536141,603545695,968387009,857246235,907096450,693723166,4268438359,2617485352,251219062,2352881043,516519644,1523660846,1845831645,310769870,4032565180,71511768,271547475,977486861,194912677,149063118,2180348861,1165796748,3250185009,3857179928,187624481,2871176343,1028920575,3036333112,1359553616,432221137,1467799745,2710016383,2884663208],"e":[65537]},"d":[687175305,1707212597,4245873716,4017721889,2695983203,3961898681,1824283117,2630687853,4196542554,1447238375,946159710,1824957689,3096907202,3266524064,1405674797,2265087403,314123956,2154139803,1536963472,3547022591,3596076043,4041503543,3439835513,611246308,918308270,833781983,2410766838,3656231250,3890887797,811086883,4073185343,2304264645,1662765263,2750782623,653712793,2155022647,2301042171,3352846070,863075550,1417176517,2552251604,2554247632,3191767147,3195131975,1358968656,433683016,3954254745,499593608,4201035877,4282480042,3177845970,407557835,2649119945,1633867677,1386846363,2910712057,2089350312,3918246504,3260673340,696060477,4107929090,3924648799,929616955,1837439395],"primes":[[3795124223,1442089965,1003116591,1445580352,2095052723,2874107191,507769036,852539801,925216126,1778520735,3991941992,2262899167,2530385339,3512906747,1347958182,969121745,2908477948,698617164,970498867,537446453,3968225414,3384642546,323323924,1145216685,2421981545,1932656155,4001042838,2780850990,2753297326,644553780,3290345484,3815691355],[2536653053,362741412,220582120,4126577943,880069477,218635675,2723212538,3394818818,1548792884,2447249933,1790994729,2907592056,282049516,800132438,2405655949,1308021872,1117093329,610163548,2049679406,4276790949,3338283844,3737195607,354201240,4013081764,1446735895,3428714633,3144432289,2287453057,4052156964,3354361811,3163521132,3246995887]]},"marker":null}"#;
    const PK_B: &str = r#"{"inner":{"n":[4276065009,3213747665,778657347,3193046143,653769242,4045371481,3830914271,3366930980,211705309,9996283,3170900839,4253452577,3422421360,1459953087,652498812,852193693,124095027,3284806090,2457036498,1274529202,3061343233,2271692926,793010386,3849556202,2783657617,1316778988,2331415076,1922204788,2557449336,1585595340,3821012283,249614156,3021666712,890622771,2233602961,2303518894,940308722,234006320,432850424,2202848075,3345689220,2868230745,3987388936,2454972500,500583356,2252609642,4066260996,1306046261,3400766693,2507809452,3767162225,2396286677,4258359731,1257557998,3720378736,1215555327,1411210338,585690939,2871664597,890755759,3612714932,2652542197,2096421341,2953328605],"e":[65537]},"marker":null}"#;
    const INV_B: &str = r#"{"inner":{"pubkey_components":{"n":[4276065009,3213747665,778657347,3193046143,653769242,4045371481,3830914271,3366930980,211705309,9996283,3170900839,4253452577,3422421360,1459953087,652498812,852193693,124095027,3284806090,2457036498,1274529202,3061343233,2271692926,793010386,3849556202,2783657617,1316778988,2331415076,1922204788,2557449336,1585595340,3821012283,249614156,3021666712,890622771,2233602961,2303518894,940308722,234006320,432850424,2202848075,3345689220,2868230745,3987388936,2454972500,500583356,2252609642,4066260996,1306046261,3400766693,2507809452,3767162225,2396286677,4258359731,1257557998,3720378736,1215555327,1411210338,585690939,2871664597,890755759,3612714932,2652542197,2096421341,2953328605],"e":[65537]},"d":[3899447297,689132800,3165539700,924046836,770752250,1559484885,1996816820,3352196770,3600098010,2195800082,981016273,2325343729,2842111056,286959726,882384409,3990803832,1786816107,2829393739,586546226,3118436455,34337234,1823468659,4294541311,2331679096,4078092311,1025705327,1038942468,1682909660,3195630242,4024505265,4125536243,4194497915,3568087251,754891252,3673605991,3480344649,1058929945,993565103,225013743,430484434,2262441264,376059903,3771950535,2906046919,982856068,2404502088,1234114344,921868102,1581063360,1688705757,3874572545,3628402776,3131729773,2512570052,1304885648,956743919,3194128664,4258711457,1370563352,1633547297,578522550,1040037401,3718993145,628455997],"primes":[[710344001,3893218185,1618471677,4182746608,2885071442,1934642306,2270091717,1281683310,2726262385,2935092220,2557636199,3126896137,4253775512,4242237459,3580414740,1129453119,90598087,3719578319,3617280200,3945031608,2013446255,2899797830,4024531738,1541708634,3803930643,643803577,951494629,2823099984,4219842061,1850221698,1541119456,3235542273],[3113695665,1267064187,1255302010,3462932440,2009485108,4066280815,2863087811,2799973864,1057301579,3651087976,4279863130,735287914,4013700694,1327984931,3142098273,1167506763,999890417,2146710376,1657347939,139954025,3096228470,3446555703,1968727517,3020449228,349983576,1300232793,2312073753,1299388478,263908474,2208070649,490380238,3920347411]]},"marker":null}"#;

    info!("Generating key pair for a");
    // let (pk_a, inv_a) = RingBase::gen_key_pair()?;
    let (pk_a, inv_a): (omg_crypt::RsaPublicKey<_>, omg_crypt::RsaPrivateKey<_>) =
        (serde_json::from_str(PK_A)?, serde_json::from_str(INV_A)?);
    info!("Generating key pair for b");
    // let (pk_b, inv_b) = RingBase::gen_key_pair()?;
    let (pk_b, inv_b): (omg_crypt::RsaPublicKey<_>, omg_crypt::RsaPrivateKey<_>) =
        (serde_json::from_str(PK_B)?, serde_json::from_str(INV_B)?);
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
                AsymKey(a_pk_a, PhantomData),
                // pk(B)
                AsymKey(a_pk_b, PhantomData),
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
                    AsymKey(pk_a.clone(), PhantomData),
                    // pk(B)
                    AsymKey(pk_b.clone(), PhantomData),
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

    // multi_agent()?;
    asym_test()?;
    // sym_test()?;

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
            agent_A::listen::<_, TcpJsonChannel<_, _>>(
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
            agent_B::run::<_, TcpJsonChannel<_, _>>(
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
            agent_s::listen::<_, TcpJsonChannel<_, _>>(
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
