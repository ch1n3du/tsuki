use chumsky::{
    primitive::{choice, just},
    recursive::{recursive, Recursive},
    select, Parser,
};

use crate::{
    ast::{self, Span},
    expr::UntypedExpr,
    extra::ModuleExtra,
    lexer,
    type_annotation::type_annotation_parser,
};

use super::{error::ParseError, token::Token};

type RecursiveParser<'a> = Recursive<'a, Token, UntypedExpr, ParseError>;

fn int_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    choice((just(Token::NewLineMinus), just(Token::Minus)))
        .ignored()
        .or_not()
        .map(|v| v.is_some())
        .then(select! { Token::Integer { value } => value })
        .map_with_span(|(number_has_minus, value), location| {
            let value = if number_has_minus {
                format!("-{value}")
            } else {
                value
            };

            UntypedExpr::String { location, value }
        })
}

fn float_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! { |location|
        Token::Float { value } => {
            UntypedExpr::Float { location, value, }
        }
    }
}

fn bool_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! { |location|
        Token::Boolean { value } => {
            UntypedExpr::Boolean { location, value }
        }
    }
}

fn string_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! { |location|
        Token::String { value } => {
            UntypedExpr::String { location, value }
        }
    }
}

fn identifier_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! {
        Token::UpName { name } => name,
        Token::Name { name } => name,
    }
    .map_with_span(|name, location| UntypedExpr::Identifier { location, name })
}

// TODO: Remove this massive hack
fn raw_identifier_parser() -> impl Parser<Token, String, Error = ParseError> {
    select! {
        Token::UpName { name } => name,
        Token::Name { name } => name,
    }
}

// TODO Complete implementation
fn let_expr_parser<'a>(
    expression_parser: RecursiveParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    just(Token::Let)
        // TODO: Implement Parsing for patterns
        .ignore_then(raw_identifier_parser())
        .then(
            just(Token::Colon)
                .ignore_then(type_annotation_parser())
                .or_not(),
        )
        .then_ignore(just(Token::Equal))
        .then(expression_parser.clone())
        .validate(
            move |((identifier, type_annotation), value), location, emit| {
                if let UntypedExpr::Assignment { .. } = value {
                    emit(ParseError::invalid_assignment_right_hand_side(location))
                }

                UntypedExpr::Assignment {
                    location,
                    value: Box::new(value),
                    // TODO: Implement patterns
                    pattern: identifier,
                    type_annotation,
                }
            },
        )
}

/// Parses things that can be found at the start of a chained expression.
fn chain_start<'a>(
    _expression_sequence_parser: RecursiveParser<'a>,
    _expression_parser: RecursiveParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    // TODO add more subparsers
    choice((string_parser(), int_parser(), identifier_parser()))
}

/// Parses a single expression.
pub fn expression_parser<'a>(
    expression_sequence_parser: RecursiveParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    recursive(|expression_parser| {
        choice((
            // fail_todo_trace(expression.clone(), sequence.clone()),
            pure_expression_parser(expression_sequence_parser, expression_parser),
        ))
    })
}

/// Helper functions that calls the expression_parser recursively
pub fn pure_expression_parser<'a>(
    _expression_sequence_parser: RecursiveParser<'a>,
    _expression_parser: RecursiveParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    let literal_expr = select! { |span|
        Token::Integer { value  } => {
            UntypedExpr::Integer { location: span, value: value, }
        },
        Token::String { value } => {
            UntypedExpr::String { location: span, value: value, }
        }
    }
    .labelled("literal");

    // Unary Operand
    let unary_op = choice((
        just(Token::Not).to(ast::UnaryOp::Not),
        choice((just(Token::Minus), just(Token::NewLineMinus))).to(ast::UnaryOp::Negate),
    ));

    let unary_expr = unary_op
        .map_with_span(|op: ast::UnaryOp, span: Span| (op, span))
        .repeated()
        // TODO .then(chained(sequence, expression))
        .then(literal_expr)
        .foldr(|(unary_op, span), value| UntypedExpr::UnaryOp {
            location: span.union(value.location()),
            op: unary_op,
            value: Box::new(value),
        })
        .boxed();

    // Product ::= expr ( * | / | % ) expr
    let product_op = choice((
        just(Token::Multiply).to(ast::BinaryOp::Multiply),
        just(Token::Divide).to(ast::BinaryOp::Divide),
        just(Token::Modulo).to(ast::BinaryOp::Modulo),
    ));

    let product = unary_expr
        .clone()
        .then(product_op.then(unary_expr).repeated())
        .foldl(|left_expr, (op, right_expr)| UntypedExpr::BinaryOp {
            location: left_expr.location().union(right_expr.location()),
            op: op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
        .boxed();

    // Sum
    let sum_op = choice((
        just(Token::Plus).to(ast::BinaryOp::Add),
        just(Token::Minus).to(ast::BinaryOp::Subtract),
    ));

    let sum = product
        .clone()
        .then(sum_op.then(product).repeated())
        .foldl(|a, (op, b)| UntypedExpr::BinaryOp {
            location: a.location().union(b.location()),
            op: op,
            left: Box::new(a),
            right: Box::new(b),
        })
        .boxed();

    // Comparison
    let comparison_op = choice((
        just(Token::EqualEqual).to(ast::BinaryOp::Eq),
        just(Token::BangEqual).to(ast::BinaryOp::NotEq),
        just(Token::LessThan).to(ast::BinaryOp::LessThan),
        just(Token::GreaterThan).to(ast::BinaryOp::GreaterThan),
        just(Token::LessThanOrEqual).to(ast::BinaryOp::LessThanOrEqual),
        just(Token::GreaterThanOrEqual).to(ast::BinaryOp::GreaterThanOrEqual),
    ));

    let comparison = sum
        .clone()
        .then(comparison_op.then(sum).repeated())
        .foldl(|left_expr, (op, right_expr)| UntypedExpr::BinaryOp {
            location: left_expr.location().union(right_expr.location()),
            op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
        .boxed();

    let or_op = just(Token::And).to(ast::BinaryOp::And);
    let or_expr = comparison
        .clone()
        .then(or_op.then(comparison).repeated())
        .foldl(|left_expr, (op, right_expr)| UntypedExpr::BinaryOp {
            location: left_expr.location().union(right_expr.location()),
            op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
        .boxed();

    let and_op = just(Token::Or).to(ast::BinaryOp::Or);
    let and_expr = or_expr
        .clone()
        .then(and_op.then(or_expr).repeated())
        .foldl(|left_expr, (op, right_expr)| UntypedExpr::BinaryOp {
            location: left_expr.location().union(right_expr.location()),
            op: op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
        .boxed();

    let pipeline_expr = and_expr
        .clone()
        .then(
            choice((just(Token::Pipe), just(Token::NewLinePipe)))
                .then(and_expr)
                .repeated(),
        )
        .foldl(|left_expr, (pipe, right_expr)| match left_expr {
            UntypedExpr::Pipeline {
                mut expressions,
                one_liner,
            } => {
                expressions.push(right_expr);
                UntypedExpr::Pipeline {
                    expressions,
                    one_liner,
                }
            }
            _ => {
                let expressions = vec![left_expr, right_expr];
                UntypedExpr::Pipeline {
                    expressions,
                    one_liner: pipe != Token::NewLinePipe,
                }
            }
        });

    pipeline_expr
}

/// Parses a sequence of expressions into an `UntypedExpr::Sequence {..}`
pub fn expression_sequence_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    recursive(|expression_sequence_parser| {
        // Parse an expression
        expression_parser(expression_sequence_parser.clone())
            // Recursively call the expression sequence parser to
            // parse the sequence of expressions one at a time.
            .then(expression_sequence_parser.repeated())
            .foldl(|current_expression, next_expression| {
                current_expression.append_in_sequence(next_expression)
            })
    })
}

pub fn module_parser(
    src: &str,
    // kind
) -> Result<(UntypedExpr, ModuleExtra), Vec<ParseError>> {
    let lexer::LexInfo {
        tokens,
        module_extra,
    } = lexer::run(src).unwrap();

    let stream = chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

    let expr = expression_parser().parse(stream);
    match expr {
        Ok(expr) => Ok((expr, module_extra)),
        Err(parse_errors) => Err(parse_errors),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn can_parse(src: &str) -> (UntypedExpr, ModuleExtra) {
        module_parser(src).unwrap()
    }
}
