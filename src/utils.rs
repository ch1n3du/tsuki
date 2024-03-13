use chumsky::Parser;

use crate::{ast, error::ParseError, lexer, token::Token};

pub fn attempt_to_parse<Value>(
    src: &str,
    parser: impl Parser<Token, Value, Error = ParseError>,
) -> Result<Value, Vec<ParseError>> {
    let lexer::LexInfo {
        tokens,
        module_extra: _,
    } = lexer::run(src).unwrap();

    let stream = chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

    parser.parse(stream)
}
