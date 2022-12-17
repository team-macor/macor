use clap::Parser;
use omg::comm::{Channel, TcpChannel};
use std::net::SocketAddr;

#[derive(Debug, Parser)]
struct Cli {
    server_address: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let addr: SocketAddr = cli.server_address.parse()?;
    println!("Trying to connect to {addr}");
    let mut channel = TcpChannel::<String, String>::connect(addr)?;
    println!("Connected!");

    loop {
        print!("Write message: ");
        let mut ans = String::new();
        std::io::stdin().read_line(&mut ans)?;
        channel.send(ans)?;
        print!("\n");

        println!("Waiting for next message");
        let msg: String = channel.receive()?;

        println!("Got message: {msg}");
    }
}
