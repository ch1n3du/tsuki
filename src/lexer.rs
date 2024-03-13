use chumsky::{
    self,
    primitive::{any, choice, end, filter, just, one_of, take_until},
    recovery::skip_then_retry_until,
    text, Error, Parser,
};

use crate::{ast::Span, error::ParseError, extra::ModuleExtra, token::Token};

#[derive(Debug)]
pub struct LexInfo {
    pub tokens: Vec<(Token, Span)>,
    pub module_extra: ModuleExtra,
}

pub fn run(src: &str) -> Result<LexInfo, Vec<ParseError>> {
    let len = src.as_bytes().len();
    let src_stream = chumsky::Stream::from_iter(
        Span::create(len, 1),
        src.chars().scan(0, |index, chary| {
            let start = *index;
            let offset = chary.len_utf8();
            // Increment the index by the width of the current character.
            *index = start + offset;

            Some((chary, Span::create(start, offset)))
        }),
    );

    let tokens: Vec<(Token, Span)> = lexer().parse(src_stream)?;

    let mut module_extra = ModuleExtra::new();

    let mut previous_token_is_newline = false;

    let tokens: Vec<(Token, Span)> = tokens
        .into_iter()
        .filter_map(|(token, ref span)| {
            let current_is_newline: bool = token == Token::NewLine || token == Token::EmptyLine;
            let result: Option<(Token, Span)> = match token {
                Token::ModuleComment => {
                    module_extra.module_comments.push(*span);
                    None
                }
                Token::DocComment => {
                    module_extra.doc_comments.push(*span);
                    None
                }
                Token::Comment => {
                    module_extra.comments.push(*span);
                    None
                }
                Token::LeftParen => {
                    if previous_token_is_newline {
                        Some((Token::NewLineLeftParen, *span))
                    } else {
                        Some((Token::LeftParen, *span))
                    }
                }
                Token::Minus => {
                    if previous_token_is_newline {
                        Some((Token::NewLineMinus, *span))
                    } else {
                        Some((Token::Minus, *span))
                    }
                }
                Token::Pipe => {
                    if previous_token_is_newline {
                        Some((Token::NewLinePipe, *span))
                    } else {
                        Some((Token::Pipe, *span))
                    }
                }
                Token::NewLine => None,
                _ => Some((token, *span)),
            };

            previous_token_is_newline = current_is_newline;

            result
        })
        .collect::<Vec<(Token, Span)>>();

    Ok(LexInfo {
        tokens,
        module_extra,
    })
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = ParseError> {
    let base10 = text::int(10).map(|value| Token::Integer { value });

    let base10_underscore = one_of("0123456789")
        .repeated()
        .at_least(1)
        .at_most(3)
        .separated_by(just("_"))
        .at_least(2)
        .flatten()
        .collect::<String>()
        .map(|value| Token::Integer { value });

    let int = choice((base10_underscore, base10));

    let op = choice((
        just("==").to(Token::EqualEqual),
        just('=').to(Token::Equal),
        // just("..").to(Token::DotDot),
        just('.').to(Token::Dot),
        just("!=").to(Token::BangEqual),
        choice((
            just("<=").to(Token::LessThanOrEqual),
            just('<').to(Token::LessThan),
            just(">=").to(Token::GreaterThanOrEqual),
            just('>').to(Token::GreaterThan),
        )),
        just('+').to(Token::Plus),
        just("->").to(Token::RightArrow),
        just('-').to(Token::Minus),
        just('*').to(Token::Multiply),
        just('/').to(Token::Divide),
        just("|>").to(Token::Pipe),
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just('%').to(Token::Modulo),
        // just('#').to(Token::Hash),
    ));

    let grouping = choice((
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just('[').to(Token::LeftSquare),
        just(']').to(Token::RightSquare),
        just('{').to(Token::LeftCurly),
        just('}').to(Token::RightCurly),
    ));

    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('"'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    let string = just('@')
        .ignore_then(just('"'))
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|value| Token::String { value })
        .labelled("string");

    let keyword = text::ident().map(|s: String| match s.as_str() {
        // "trace" => Token::Trace,
        // "error" => Token::Fail,
        // "fail" => Token::Fail,
        "and" => Token::And,
        "or" => Token::Or,
        // "expect" => Token::Expect,
        "const" => Token::Const,
        "fn" => Token::Fn,
        "test" => Token::Test,
        "if" => Token::If,
        "else" => Token::Else,
        "let" => Token::Let,
        // "opaque" => Token::Opaque,
        "pub" => Token::Pub,
        "use" => Token::Use,
        "todo" => Token::Todo,
        "type" => Token::Type,
        "match" => Token::Match,
        _ => {
            if s.chars().next().map_or(false, |c| c.is_uppercase()) {
                Token::UpName {
                    // TODO: do not allow _ in upname
                    name: s,
                }
            } else if s.starts_with('_') {
                Token::DiscardName {
                    // TODO: do not allow uppercase letters in discard name
                    name: s,
                }
            } else {
                Token::Name {
                    // TODO: do not allow uppercase letters in name
                    name: s,
                }
            }
        }
    });

    fn comment_parser(token: Token) -> impl Parser<char, (Token, Span), Error = ParseError> {
        let n = match token {
            Token::ModuleComment => 4,
            Token::DocComment => 3,
            Token::Comment => 2,
            _ => unreachable!(),
        };

        choice((
            // NOTE: The first case here work around a bug introduced with chumsky=0.9.0 which
            // miscalculate the offset for empty comments.
            just("/".repeat(n))
                .ignore_then(choice((text::newline().rewind(), end())))
                .to(token.clone())
                .map_with_span(move |token, span: Span| {
                    (token, chumsky::Span::new((), span.start + n..span.end))
                }),
            just("/".repeat(n)).ignore_then(
                take_until(choice((text::newline().rewind(), end())))
                    .to(token)
                    .map_with_span(|token, span| (token, span)),
            ),
        ))
    }

    let newlines = choice((
        choice((just("\n\n"), just("\r\n\r\n"))).to(Token::EmptyLine),
        choice((just("\n"), just("\r\n"))).to(Token::NewLine),
    ));

    choice((
        comment_parser(Token::ModuleComment),
        comment_parser(Token::DocComment),
        comment_parser(Token::Comment),
        choice((keyword, int, op, newlines, grouping, string))
            .or(any().map(Token::Error).validate(|t, span, emit| {
                emit(ParseError::expected_input_found(
                    span,
                    None,
                    Some(t.clone()),
                ));
                t
            }))
            .map_with_span(|token, span| (token, span)),
    ))
    .padded_by(one_of(" \t").ignored().repeated())
    .recover_with(skip_then_retry_until([]))
    .repeated()
    .padded_by(one_of(" \t").ignored().repeated())
    .then_ignore(end())
}
