use chumsky::{primitive::just, select, Parser};

use crate::{
    ast::CallArg,
    error::ParseError,
    parser::{Chain, RecursiveExpressionParser},
    token::Token,
};

pub fn call_parser<'a>(
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, Chain, Error = ParseError> + 'a {
    // Labelled function call parser
    //
    // Examples:  (name: "Von Trier", age: 25)
    select! { Token::Name { name } => name }
        .then_ignore(just(Token::Colon))
        .or_not()
        .then(expression_parser)
        .map_with_span(|(label, value), location| CallArg {
            label,
            location,
            value: value,
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
        .map_with_span(Chain::Call)
}
