use itertools::Itertools;
use macor_parse::parse_document;

pub fn prettify(src: &str) -> Result<Option<String>, std::fmt::Error> {
    use std::fmt::Write;

    let ast = if let Ok(ast) = parse_document(src) {
        ast
    } else {
        return Ok(None);
    };

    let mut buf = String::new();

    writeln!(buf, "Protocol: {}", ast.name)?;
    writeln!(buf)?;
    writeln!(buf, "Types:")?;
    let mut first = true;
    for (t, ids) in ast.types {
        if first {
            write!(buf, "  {} ", t)?;
            write!(buf, "{}", ids.iter().format(","))?;
        } else {
            writeln!(buf, ";");
            write!(buf, "  {} ", t)?;
            write!(buf, "{}", ids.iter().format(","))?;
        }
        first = false;
    }
    writeln!(buf)?;
    writeln!(buf)?;
    writeln!(buf, "Knowledge:")?;
    first = true;
    for (agent, msgs) in ast.knowledge.agents.iter() {
        if first {
            write!(buf, "  {}: ", agent)?;
            write!(buf, "{}", msgs.iter().format(","))?;
        } else {
            writeln!(buf, ";")?;
            write!(buf, "  {}: ", agent)?;
            write!(buf, "{}", msgs.iter().format(","))?;
        }
        first = false;
    }
    writeln!(buf)?;
    writeln!(buf)?;
    writeln!(buf, "Actions:")?;
    for action in ast.actions.iter() {
        write!(buf, "  {}->{}: ", action.from, action.to)?;
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

    Ok(Some(buf))
}
