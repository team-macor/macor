use crate::ast::{Action, Document, Goal, Ident, Knowledge, Message, TypesKey, Where};
use chumsky::{prelude::*, Stream};

type Span = std::ops::Range<usize>;

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let arrow = just("->").to(Token::Arrow);
    let comma = just(',').to(Token::Comma);
    let semicolon = just(';').to(Token::Semicolon);
    let colon = just(':').to(Token::Colon);

    let neq = just("!=").to(Token::Neq);

    let lparan = just('(').to(Token::LParen);

    let rparan = just(')').to(Token::RParen);

    let lcurly = just('{').to(Token::LCurly);

    let rcurly = just('}').to(Token::RCurly);

    let lsym = just("{|").to(Token::LSym);
    let rsym = just("|}").to(Token::RSym);

    // A parser for identifiers and keywords
    let word = text::ident().map(|ident: String| match ident.as_str() {
        "Protocol" => Token::Protocol,
        "Types" => Token::Types,
        "Knowledge" => Token::Knowledge,
        "Actions" => Token::Actions,
        "Goals" => Token::Goals,
        "Agent" => Token::Agent,
        "Symmetric_key" => Token::SymmetricKey,
        "Function" => Token::Function,
        "Number" => Token::Number,
        "Public_key" => Token::PublicKey,
        "authenticates" => Token::Authenticates,
        "on" => Token::On,
        "secret" => Token::Secret,
        "between" => Token::Between,
        "weakly" => Token::Weakly,
        "guessable" => Token::Guessable,
        "where" => Token::Where,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = word
        .or(arrow)
        .or(comma)
        .or(semicolon)
        .or(colon)
        .or(neq)
        .or(lparan)
        .or(rparan)
        .or(lsym)
        .or(rsym)
        .or(lcurly)
        .or(rcurly)
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Ident(String),
    Arrow,
    Comma,
    Semicolon,
    Colon,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSym,
    RSym,
    Protocol,
    Types,
    Knowledge,
    Actions,
    Goals,
    Agent,
    SymmetricKey,
    Function,
    Number,
    PublicKey,
    Authenticates,
    On,
    Secret,
    Between,
    Weakly,
    Guessable,
    Where,
    Neq,
}

fn messages_parser() -> impl Parser<Token, Vec<Message<String>>, Error = Simple<Token>> + Clone {
    message_parser().separated_by(just(Token::Comma))
}

fn message_parser() -> impl Parser<Token, Message<String>, Error = Simple<Token>> + Clone {
    recursive(|msg| {
        let fun = ident_parser()
            .then_ignore(just(Token::LParen))
            .then(msg.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::RParen))
            .map(|(func, args)| Message::Fun(func, args));
        let var = ident_parser().map(Message::Var);
        let sym = just(Token::LSym)
            .then(msg.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::RSym))
            .then(msg.clone())
            .map(|((_, msgs), key)| Message::SymEnc(msgs, Box::new(key)));
        let sym_par = just(Token::LSym)
            .then(msg.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::RSym))
            .then_ignore(just(Token::LParen))
            .then(msg.clone())
            .then_ignore(just(Token::RParen))
            .map(|((_, msgs), key)| Message::SymEnc(msgs, Box::new(key)));
        let asym = just(Token::LCurly)
            .then(msg.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::RCurly))
            .then(msg.clone())
            .map(|((_, msgs), key)| Message::AsymEnc(msgs, Box::new(key)));
        let asym_par = just(Token::LCurly)
            .then(msg.clone().separated_by(just(Token::Comma)))
            .then_ignore(just(Token::RCurly))
            .then_ignore(just(Token::LParen))
            .then(msg.clone())
            .then_ignore(just(Token::RParen))
            .map(|((_, msgs), key)| Message::AsymEnc(msgs, Box::new(key)));
        fun.or(sym)
            .or(sym_par)
            .or(asym)
            .or(asym_par)
            .or(var)
            .or(msg.delimited_by(just(Token::LParen), just(Token::RParen)))
    })
}

fn ident_parser() -> impl Parser<Token, Ident<String>, Error = Simple<Token>> + Clone {
    select! { Token::Ident(var) => var.as_str().into() }
}

pub fn document_parser() -> impl Parser<Token, Document<String>, Error = Simple<Token>> {
    let protocol_name = just(Token::Protocol)
        .then(just(Token::Colon))
        .then(ident_parser())
        .map(|(_, name)| name);

    // Parser<TypesKey>
    let types_key = select! {
        Token::Agent => TypesKey::Agent,
        Token::Number => TypesKey::Number,
        Token::SymmetricKey => TypesKey::SymmetricKey,
        Token::PublicKey => TypesKey::PublicKey,
        Token::Function => TypesKey::Function,
    };

    let types = just(Token::Types)
        .then(just(Token::Colon))
        .ignored()
        .then(
            types_key
                .then(ident_parser().separated_by(just(Token::Comma)))
                .separated_by(just(Token::Semicolon)),
        )
        .map(|(_, types)| types);

    let where_parse = just(Token::Where)
        .then(
            ident_parser()
                .then_ignore(just(Token::Neq))
                .then(ident_parser())
                .map(|(a, b)| Where::NotEqual(a, b))
                .separated_by(just(Token::Comma)),
        )
        .map(|(_, wheres)| wheres);

    let knowledge = just(Token::Knowledge)
        .then(just(Token::Colon))
        .ignored()
        .then(
            ident_parser()
                .then_ignore(just(Token::Colon))
                .then(messages_parser())
                .separated_by(just(Token::Semicolon)),
        )
        .then(where_parse.or_not())
        .map(|((_, agents), wheres)| Knowledge {
            agents,
            wheres: wheres.unwrap_or_default(),
        });

    let action_parser = ident_parser()
        .then_ignore(just(Token::Arrow))
        .then(ident_parser())
        .then_ignore(just(Token::Colon))
        .then(messages_parser())
        .map(|((from, to), msgs)| Action { from, to, msgs });

    let actions = just(Token::Actions)
        .then(just(Token::Colon))
        .ignored()
        .then(action_parser.repeated())
        .map(|(_, actions)| actions);

    let authenticates_on_goal = ident_parser()
        .then(just(Token::Weakly).or_not())
        .then_ignore(just(Token::Authenticates))
        .then(ident_parser())
        .then_ignore(just(Token::On))
        .then(messages_parser())
        .map(|(((a, weakly), b), msgs)| Goal::Authenticates {
            a,
            b,
            msgs,
            weakly: weakly.is_some(),
        });

    let secret_between_goal = message_parser()
        .separated_by(just(Token::Comma))
        .then(just(Token::Guessable).or_not())
        .then_ignore(just(Token::Secret).then_ignore(just(Token::Between)))
        .then(ident_parser().separated_by(just(Token::Comma)))
        .map(|((msgs, guessable), agents)| Goal::SecretBetween {
            msgs,
            agents,
            guessable: guessable.is_some(),
        });

    let goals = just(Token::Goals)
        .ignored()
        .then_ignore(just(Token::Colon))
        .then(authenticates_on_goal.or(secret_between_goal).repeated())
        .map(|(_, goals)| goals);

    protocol_name
        .then(types)
        .then(knowledge)
        .then(actions)
        .then(goals)
        .map(|((((name, types), knowledge), actions), goals)| Document {
            name,
            types,
            knowledge,
            actions,
            goals,
        })
        .then_ignore(end())
}

pub fn parse_document(src: &str) -> (Option<Document<String>>, Vec<Simple<String>>) {
    #[allow(clippy::type_complexity)]
    static LEXER: once_cell::sync::Lazy<
        std::sync::Arc<dyn Parser<char, Vec<(Token, Span)>, Error = Simple<char>> + Send + Sync>,
    > = once_cell::sync::Lazy::new(|| std::sync::Arc::new(lexer()));

    // TODO: The document_parser does not implement Send + Sync since it uses
    // the recursive combinator
    // static PARSER: once_cell::sync::Lazy<
    //     std::sync::Arc<dyn Parser<Token, Document<String>, Error = Simple<Token>> + Send + Sync>,
    // > = once_cell::sync::Lazy::new(|| std::sync::Arc::new(document_parser()));

    let (tokens, errs) = LEXER.parse_recovery(src);

    let tokens = if let Some(tokens) = tokens {
        tokens
    } else {
        return (
            None,
            errs.into_iter()
                .map(|err| err.map(|c| c.to_string()))
                .collect(),
        );
    };

    let len = src.chars().count();
    let (doc, parse_errs) =
        document_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

    (
        doc,
        parse_errs
            .into_iter()
            .map(|tok| tok.map(|tok| format!("{tok:?}")))
            .collect(),
    )
}

#[cfg(test)]
mod tests {
    use chumsky::{prelude::*, Stream};
    use itertools::Itertools;

    use super::*;

    #[test]
    fn test_chomsky_message() {
        let src = include_str!("../../example_programs/KeyEx1.AnB");
        let (tokens, errs) = lexer().parse_recovery(src);

        let _ = tokens
            .clone()
            .unwrap()
            .iter()
            .inspect(|(t, _)| {
                println!("{:?}", t);
            })
            .collect_vec();

        if !errs.is_empty() {
            panic!("{errs:?}");
        }

        let tokens = tokens.unwrap();
        let len = src.chars().count();
        let (result, parse_errs) =
            document_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if !parse_errs.is_empty() {
            panic!("{parse_errs:?}");
        }

        let doc = result.unwrap();

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
    }
}
