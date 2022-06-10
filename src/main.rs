#![feature(box_syntax)]

fn main() -> miette::Result<()> {
    miette::set_hook(box |_| {
        box miette::MietteHandlerOpts::new()
            .terminal_links(true)
            .context_lines(4)
            .force_graphical(true)
            .build()
    })?;

    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let parsed = macor::parse_document(&src)?;

    dbg!(parsed);

    Ok(())
}
