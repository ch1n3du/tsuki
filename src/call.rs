use chumsky::{
    primitive::{choice, just},
    select, Parser,
};

use crate::{
    ast::CallArg,
    error::ParseError,
    parser::{Chain, RecursiveExpressionParser},
    token::Token,
};

pub fn call_parser<'a>(
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, Chain, Error = ParseError> + 'a {
    choice((
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
                value: Some(value),
            }),
        // Discard function call parser
        //
        // Examples:  (name: _, age: _)
        select! { Token::Name { name } => name }
            .then_ignore(just(Token::Colon))
            .or_not()
            .then_ignore(select! { Token::DiscardName { name } => name })
            .map_with_span(|label, location| CallArg {
                location,
                label,
                value: None,
            }),
    ))
    .separated_by(just(Token::Comma))
    .allow_trailing()
    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
    .map_with_span(Chain::Call)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        error::ParseError,
        parser::{expression_parser, expression_sequence_parser, Chain},
        utils::attempt_to_parse,
    };

    use super::call_parser;

    fn try_parsing_call(src: &str) -> Result<Chain, Vec<ParseError>> {
        attempt_to_parse(
            src,
            call_parser(expression_parser(expression_sequence_parser())),
        )
    }

    #[test]
    fn can_parse_simple_labelled_call() {
        let result = try_parsing_call("(name: \"Von Trier\"), age: 17)");
        panic!("{result:#?}")
    }
}
