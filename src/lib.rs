use logos::Logos;

mod dolev_yao;

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

    #[token("Abstration")]
    Abstration,

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
}
