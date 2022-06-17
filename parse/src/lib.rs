#![feature(box_syntax)]

pub mod ast;
pub mod chumskyparse;

use lalrpop_util::lalrpop_mod;
use miette::SourceSpan;

use crate::ast::{Document, Message};

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

pub fn parse_document(src: &str) -> Result<Document<&str>, ParseError> {
    static PARSER: once_cell::sync::Lazy<parser::DocumentParser> =
        once_cell::sync::Lazy::new(parser::DocumentParser::new);

    match PARSER.parse(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(src, e)),
    }
}

pub fn parse_message(arg: &str) -> Result<Message<&str>, ParseError> {
    static PARSER: once_cell::sync::Lazy<parser::MessageParser> =
        once_cell::sync::Lazy::new(parser::MessageParser::new);

    match PARSER.parse(arg) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(arg, e)),
    }
}

pub fn parse_messages(arg: &str) -> Result<Vec<Message<&str>>, ParseError> {
    static PARSER: once_cell::sync::Lazy<parser::MessagesParser> =
        once_cell::sync::Lazy::new(parser::MessagesParser::new);

    match PARSER.parse(arg) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(arg, e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chumskyparse;

    static KEY_EX1: &str = include_str!("../../example_programs/KeyEx1.AnB");
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

        assert_eq!(doc.name.as_str(), "KeyEx");
        assert_eq!(doc.types.len(), 3);
        assert_eq!(doc.actions.len(), 3);
        assert_eq!(doc.goals.len(), 3);
        assert_eq!(doc.knowledge.agents.len(), 3);
        assert_eq!(doc.knowledge.agents[0].0.as_str(), "A");
        assert_eq!(doc.knowledge.agents[0].1.len(), 4);
        assert_eq!(doc.knowledge.agents[1].0.as_str(), "B");
        assert_eq!(doc.knowledge.agents[1].1.len(), 4);
        assert_eq!(doc.knowledge.agents[2].0.as_str(), "s");
        assert_eq!(doc.knowledge.agents[2].1.len(), 5);

        Ok(())
    }
    #[test]
    fn compare_parsers() -> miette::Result<(), Box<dyn std::error::Error>> {
        for p in std::fs::read_dir("../example_programs")? {
            let src = std::fs::read_to_string(p?.path())?;

            let doc = parse_document(&src)?;
            let doc = doc.map(|s| s.to_string());
            let (doc2, _errors) = chumskyparse::parse_document(&src);
            let doc2 = doc2.unwrap();

            assert_eq!(doc, doc2);
        }

        Ok(())
    }
}
