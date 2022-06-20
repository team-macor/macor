use itertools::Itertools;
use macor_parse::parse_document;

pub fn prettify(src: String) -> Result<String, std::fmt::Error> {
    use std::fmt::Write;

    let ast = parse_document(&src).unwrap();

    let mut buf = String::new();

    writeln!(buf, "Protocol: {}", ast.name)?;
    writeln!(buf)?;
    writeln!(buf, "Types:")?;
    for (t, ids) in ast.types {
        writeln!(buf, "  {}: ", t)?;
        writeln!(buf, "{};", ids.iter().format(","))?;
    }
    writeln!(buf)?;
    writeln!(buf, "Knowledge:")?;
    for (agent, msgs) in ast.knowledge.agents.iter() {
        writeln!(buf, "  {}: ", agent)?;
        writeln!(buf, "{};", msgs.iter().format(","))?;
    }
    writeln!(buf)?;
    writeln!(buf, "Actions:")?;
    for action in ast.actions.iter() {
        writeln!(buf, "  {}->{}: ", action.from, action.to)?;
        writeln!(buf, "{}", action.msgs.iter().format(","))?;
    }
    writeln!(buf)?;
    writeln!(buf, "Goals:")?;
    for goal in ast.goals.iter() {
        match goal {
            macor_parse::ast::Goal::Authenticates { a, b, msgs, weakly } => {
                if *weakly {
                    writeln!(
                        buf,
                        "  {} weakly authenticates {} on {}",
                        a,
                        b,
                        msgs.iter().format(",")
                    )?;
                } else {
                    writeln!(
                        buf,
                        "  {} authenticates {} on {}",
                        a,
                        b,
                        msgs.iter().format(",")
                    )?;
                }
            }
            macor_parse::ast::Goal::SecretBetween {
                msgs,
                agents,
                guessable,
            } => {
                if *guessable {
                    writeln!(
                        buf,
                        "  {} guessable secret between {}",
                        msgs.iter().format(","),
                        agents.iter().format(",")
                    )?;
                } else {
                    writeln!(
                        buf,
                        "  {} secret between {}",
                        msgs.iter().format(","),
                        agents.iter().format(",")
                    )?;
                }
            }
        }
    }

    Ok(buf)
}
