use clap::Parser;
use omg::comm::{Channel, TcpChannel};
use std::net::SocketAddr;

#[derive(Debug, Parser)]
struct Cli {
    port: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let addr = format!("0.0.0.0:{}", &cli.port);

    println!("Waiting for connection on {addr}");
    let mut channel = TcpChannel::listen(addr.parse()?)?;
    println!("Got connection!");

    loop {
        println!("Waiting for next message");
        let msg: String = channel.receive()?;

        println!("Got message: {msg}");
        print!("Answer: ");
        let mut ans = String::new();
        std::io::stdin().read_line(&mut ans)?;
        channel.send(ans)?;
        print!("\n");
    }
}
