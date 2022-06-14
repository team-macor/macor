#![feature(box_syntax)]

use macor::{
    protocol::Protocol,
    search::{SearchOptions, Searcher},
};

fn main() -> miette::Result<()> {
    miette::set_hook(box |_| {
        box miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .context_lines(4)
            .force_graphical(true)
            .build()
    })?;

    let src = std::fs::read_to_string(
        std::env::args()
            .nth(1)
            .expect("the program must be run with an argument"),
    )
    .unwrap();
    let parsed = macor::parse_document(&src)?;

    let protocol = Protocol::new(src.clone(), parsed).map_err(|x| x.first().cloned().unwrap())?;
    let searcher = Searcher::new(protocol);

    let attack = searcher.find_attack(SearchOptions { num_sessions: 1 });

    println!("{attack:#?}");

    Ok(())
}
