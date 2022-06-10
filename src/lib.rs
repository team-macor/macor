#![feature(box_syntax)]

pub mod ast;

lalrpop_mod!(pub parser);

use lalrpop_util::lalrpop_mod;
use logos::Logos;
use miette::SourceSpan;

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

pub fn parse_document(src: &str) -> Result<crate::ast::Document, ParseError> {
    static PARSER: once_cell::sync::Lazy<parser::DocumentParser> =
        once_cell::sync::Lazy::new(parser::DocumentParser::new);

    match PARSER.parse(src) {
        Ok(p) => Ok(p),
        Err(e) => Err(ParseError::new(src, e)),
    }
}

#[derive(Logos, Debug, PartialEq)]
enum Token<'a> {
    #[token("Protocol")]
    Protocol,
    #[token("Knowledge")]
    Knowledge,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token(",")]
    Comma,
    #[token("Types")]
    Types,
    #[token("Agent")]
    Agent,
    #[token("Number")]
    Number,
    #[token("SeqNumber")]
    SeqNumber,
    #[token("PublicKey")]
    PublicKey,
    #[token("Symmetric_key")]
    SymmetricKey,
    #[token("Function")]
    Function,
    #[token("Untyped")]
    Untyped,
    #[token("Actions")]
    Actions,
    #[token("Goals")]
    Goals,
    #[token("Abstraction")]
    Abstraction,
    #[token("authenticates")]
    Authenticates,
    #[token("on")]
    On,
    #[token("secret")]
    Secret,
    #[token("between")]
    Between,

    #[regex(r"[a-zA-Z0-9]+")]
    Raw(&'a str),

    #[regex(r"[\n]+")]
    Newline,
    #[token("#")]
    CommentBegin,
    #[token("(")]
    OpenParenthesis,
    #[token(")")]
    CloseParenthesis,
    #[token("->")]
    SendsTo,

    #[error]
    #[regex(r"[ ]+", logos::skip)]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    static KEY_EX1: &str = include_str!("../example_programs/KeyEx1.AnB");

    #[test]
    fn ex1() {
        dbg!(KEY_EX1);
        let mut lex = Token::lexer(KEY_EX1);

        assert_eq!(lex.next(), Some(Token::Protocol));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("KeyEx")));
        assert_eq!(lex.next(), Some(Token::Newline));
        // comment
        assert_eq!(lex.next(), Some(Token::CommentBegin));
        assert_eq!(lex.next(), Some(Token::Raw("Naive")));
        assert_eq!(lex.next(), Some(Token::Raw("first")));
        assert_eq!(lex.next(), Some(Token::Raw("version")));
        assert_eq!(lex.next(), Some(Token::Raw("without")));
        assert_eq!(lex.next(), Some(Token::Raw("encryption")));
        assert_eq!(lex.next(), Some(Token::Newline));

        assert_eq!(lex.next(), Some(Token::Types));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // Agent
        assert_eq!(lex.next(), Some(Token::Agent));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::SemiColon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // sym key
        assert_eq!(lex.next(), Some(Token::SymmetricKey));
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::SemiColon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // function
        assert_eq!(lex.next(), Some(Token::Function));
        assert_eq!(lex.next(), Some(Token::Raw("sk")));
        assert_eq!(lex.next(), Some(Token::Newline));

        // knowledge
        assert_eq!(lex.next(), Some(Token::Knowledge));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // A knowledge
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("sk")));
        assert_eq!(lex.next(), Some(Token::OpenParenthesis));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::CloseParenthesis));
        assert_eq!(lex.next(), Some(Token::SemiColon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // B knowledge
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("sk")));
        assert_eq!(lex.next(), Some(Token::OpenParenthesis));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::CloseParenthesis));
        assert_eq!(lex.next(), Some(Token::SemiColon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // s knowledge
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("sk")));
        assert_eq!(lex.next(), Some(Token::OpenParenthesis));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::CloseParenthesis));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("sk")));
        assert_eq!(lex.next(), Some(Token::OpenParenthesis));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::CloseParenthesis));
        assert_eq!(lex.next(), Some(Token::Newline));

        // actions
        assert_eq!(lex.next(), Some(Token::Actions));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // step 1
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::SendsTo));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Newline));
        // comment
        assert_eq!(lex.next(), Some(Token::CommentBegin));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Raw("creates")));
        assert_eq!(lex.next(), Some(Token::Raw("key")));
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::Newline));
        // step 2
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::SendsTo));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::Newline));
        // step 3
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::SendsTo));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::Newline));

        // goals
        assert_eq!(lex.next(), Some(Token::Goals));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Newline));
        // 1.
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Authenticates));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::On));
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Newline));
        // 2.
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Authenticates));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::On));
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Newline));
        // 2.
        assert_eq!(lex.next(), Some(Token::Raw("KAB")));
        assert_eq!(lex.next(), Some(Token::Secret));
        assert_eq!(lex.next(), Some(Token::Between));
        assert_eq!(lex.next(), Some(Token::Raw("A")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("B")));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), Some(Token::Raw("s")));
        assert_eq!(lex.next(), Some(Token::Newline));
        assert_eq!(lex.next(), None);
    }

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
