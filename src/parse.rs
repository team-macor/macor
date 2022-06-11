use lalrpop_util::lalrpop_mod;
use miette::SourceSpan;

use crate::ast::Document;

lalrpop_mod!(parser);

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
pub enum ParseError {
    #[error("Invalid Token")]
    #[diagnostic()]
    InvalidToken {
        #[source_code]
        src: String,
        #[label("This token is not valid in this context")]
        err_span: SourceSpan,
    },
    #[error("Unrecognized Token")]
    #[diagnostic(help("Expected tokens here are: {expected}"))]
    UnrecognizedToken {
        #[source_code]
        src: String,
        #[label = "The token \"{token}\" is unrecognized in this context."]
        err_span: SourceSpan,
        token: String,
        expected: String,
    },
    #[error("Unrecognized EOF")]
    #[diagnostic(help("Expected tokens in this context are:\n{expected}"))]
    UnrecognizedEOF {
        #[source_code]
        src: String,
        #[label = "The document ends too early. Are you missing a token?"]
        err_span: SourceSpan,
        expected: String,
    },
}

impl ParseError {
    fn new(
        src: &str,
        e: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &str>,
    ) -> Self {
        let prep_src = || format!("{}\n", src);

        match e {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError::InvalidToken {
                src: prep_src(),
                err_span: (location, 0).into(),
            },
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                ParseError::UnrecognizedEOF {
                    src: prep_src(),
                    err_span: (location, 0).into(),
                    expected: expected.join(", "),
                }
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                ParseError::UnrecognizedToken {
                    src: prep_src(),
                    err_span: (token.0, token.2 - token.0).into(),
                    token: token.1.to_string(),
                    expected: expected.join(", "),
                }
            }
            lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
            lalrpop_util::ParseError::User { .. } => todo!(),
        }
    }
}

pub struct ParseResult<T> {
    pub ast: Option<T>,
    pub errors: Vec<ParseError>,
}

pub fn parse_document(src: &str) -> Result<Document, ParseError> {
    static PARSER: once_cell::sync::Lazy<parser::DocumentParser> =
        once_cell::sync::Lazy::new(parser::DocumentParser::new);

    match PARSER.parse(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(src, e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static KEY_EX1: &str = include_str!("../example_programs/KeyEx1.AnB");
    #[test]
    fn basic_parser_test() -> miette::Result<()> {
        miette::set_hook(box |_| {
            box miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .context_lines(4)
                .force_graphical(true)
                .build()
        })?;

        let doc = parse_document(KEY_EX1)?;

        assert_eq!(&*doc.name, "KeyEx");
        assert_eq!(doc.types.len(), 3);
        assert_eq!(doc.actions.len(), 3);
        assert_eq!(doc.goals.len(), 3);
        assert_eq!(doc.knowledge.agents.len(), 3);
        assert_eq!(&*doc.knowledge.agents[0].0, "A");
        assert_eq!(doc.knowledge.agents[0].1.len(), 4);
        assert_eq!(&*doc.knowledge.agents[1].0, "B");
        assert_eq!(doc.knowledge.agents[1].1.len(), 4);
        assert_eq!(&*doc.knowledge.agents[2].0, "s");
        assert_eq!(doc.knowledge.agents[2].1.len(), 5);

        Ok(())
    }
}
