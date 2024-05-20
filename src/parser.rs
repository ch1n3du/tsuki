use chumsky::{
    primitive::{choice, just},
    recursive::{recursive, Recursive},
    select, Parser,
};

use crate::{
    ast::{self, Argument, ArgumentName, Span, UntypedCallArg, UntypedDefinition},
    call::call_parser,
    expr::{self, UntypedExpr},
    extra::ModuleExtra,
    lexer,
    type_::type_annotation_parser,
    utils,
};

use super::{error::ParseError, token::Token};

pub type RecursiveExpressionParser<'a> = Recursive<'a, Token, UntypedExpr, ParseError>;

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

            UntypedExpr::Integer { location, value }
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
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    just(Token::Let)
        // TODO: Implement Parsing for patterns
        .ignore_then(raw_identifier_parser())
        .then(just(Token::Colon).ignore_then(type_annotation_parser()))
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

// Helper type for parsing chained expressions
#[derive(Debug)]
pub enum Chain {
    Call(Vec<UntypedCallArg>, Span),
    FieldAccess(String, Span),
    TupleIndex(usize, Span),
}

fn tuple_index_parser() -> impl Parser<Token, Chain, Error = ParseError> {
    just(Token::Dot)
        .ignore_then(select! {
            Token::Ordinal { index } => index,
        })
        .validate(|tuple_index, location, emit| {
            if tuple_index < 1 {
                emit(ParseError::invalid_tuple_index(
                    location,
                    tuple_index.to_string(),
                    None,
                ));
                Chain::TupleIndex(0, location)
            } else {
                Chain::TupleIndex(tuple_index as usize - 1, location)
            }
        })
}

fn field_access_parser() -> impl Parser<Token, Chain, Error = ParseError> {
    just(Token::Dot)
        .ignore_then(select! {
            Token::Name { name } => name,
        })
        .map_with_span(Chain::FieldAccess)
}

// TODO: Finish implementation
fn _constructor_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! {Token::Name { name } => name}
        .map_with_span(|module, span| (module, span))
        .then_ignore(just(Token::Dot))
        .then(select! {Token::UpName { name } => name})
        .map_with_span(|((module, m_span), name), span| UntypedExpr::FieldAccess {
            location: span,
            label: name,
            container: Box::new(UntypedExpr::Identifier {
                location: m_span,
                name: module,
            }),
        })
}

fn block_parser<'a>(
    expression_sequence_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    choice((
        expression_sequence_parser
            .clone()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly)),
        expression_sequence_parser
            .clone()
            .delimited_by(just(Token::LeftCurly), just(Token::RightCurly)),
    ))
    .map_with_span(|expr, span| {
        if matches!(expr, UntypedExpr::Assignment { .. }) {
            UntypedExpr::Sequence {
                location: span,
                expressions: vec![expr],
            }
        } else {
            expr
        }
    })
}

fn if_else_parser<'a>(
    expression_sequence_parser: RecursiveExpressionParser<'a>,
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    just(Token::If)
        .ignore_then(
            expression_parser
                .clone()
                .then(block_parser(expression_sequence_parser.clone()))
                .map_with_span(|(condition, body), span| expr::IfBranch {
                    condition,
                    body,
                    location: span,
                }),
        )
        .then(
            just(Token::Else)
                .ignore_then(just(Token::If))
                .ignore_then(
                    expression_parser
                        .clone()
                        .then(block_parser(expression_sequence_parser.clone()))
                        .map_with_span(|(condition, body), span| expr::IfBranch {
                            condition,
                            body,
                            location: span,
                        }),
                )
                .repeated(),
        )
        .then_ignore(just(Token::Else))
        .then(block_parser(expression_sequence_parser))
        .map_with_span(|((first_branch, alternative_branches), final_else), span| {
            let mut branches = vec1::Vec1::new(first_branch);

            branches.extend(alternative_branches);

            UntypedExpr::If {
                location: span,
                branches,
                final_else: Box::new(final_else),
            }
        })
}

/// Parses things that can be found at the start of a chained expression.
fn chain_start<'a>(
    expression_sequence_parser: RecursiveExpressionParser<'a>,
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    // TODO add more subparsers
    choice((
        string_parser(),
        int_parser(),
        float_parser(),
        bool_parser(),
        identifier_parser(),
        let_expr_parser(expression_parser.clone()),
        if_else_parser(expression_sequence_parser, expression_parser),
    ))
}

fn chain_parser<'a>(
    expression_sequence_parser: RecursiveExpressionParser<'a>,
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    let chain = choice((
        tuple_index_parser(),
        field_access_parser(),
        call_parser(expression_parser.clone()),
    ));

    chain_start(expression_sequence_parser, expression_parser)
        .then(chain.repeated())
        .foldl(|current_expr, chain| match chain {
            Chain::Call(args, location) => current_expr.call(args, location),
            Chain::FieldAccess(label, location) => current_expr.field_access(label, location),
            Chain::TupleIndex(index, location) => current_expr.tuple_index(index, location),
        })
}

/// Parses a single expression.
pub fn expression_parser<'a>(
    expression_sequence_parser: RecursiveExpressionParser<'a>,
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
    expression_sequence_parser: RecursiveExpressionParser<'a>,
    expression_parser: RecursiveExpressionParser<'a>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    // Unary Operand
    let unary_op = choice((
        just(Token::Not).to(ast::UnaryOp::Not),
        choice((just(Token::Minus), just(Token::NewLineMinus))).to(ast::UnaryOp::Negate),
    ));

    let unary_expr = unary_op
        .map_with_span(|op: ast::UnaryOp, span: Span| (op, span))
        .repeated()
        // TODO .then(chained(sequence, expression))
        .then(chain_parser(expression_sequence_parser, expression_parser))
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
pub fn expression_sequence_parser<'a>() -> RecursiveExpressionParser<'a> {
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

pub fn parameter_parser() -> impl Parser<Token, ast::Argument, Error = ParseError> {
    choice((
        select! { Token::Name { name } => name }
            .then(select! { Token::Name { name } => name})
            .map_with_span(move |(label, name), span| ArgumentName::Named {
                label,
                name,
                location: span,
            }),
        select! { Token::Name { name } => name }.map_with_span(move |name, span| {
            ArgumentName::Named {
                label: name.clone(),
                name,
                location: span,
            }
        }),
    ))
    .then(just(Token::Colon).ignore_then(type_annotation_parser()))
    .map_with_span(|(argument_name, annotation), location| Argument {
        location,
        annotation,
        doc: None,
        argument_name,
    })
}

pub fn function_parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::optional_flag(Token::Pub)
        .then_ignore(just(Token::Fn))
        .then(select! { Token::Name { name } => name })
        .then(
            parameter_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|arguments, span| (arguments, span)),
        )
        .then(just(Token::RightArrow).ignore_then(type_annotation_parser()))
        .then(
            expression_sequence_parser()
                .or_not()
                .delimited_by(just(Token::LeftCurly), just(Token::RightCurly)),
        )
        .map_with_span(
            |((((is_public, name), (arguments, arguments_span)), return_annotation), body),
             span| {
                ast::UntypedDefinition::Fn(ast::Function {
                    location: ast::Span {
                        start: span.start,
                        end: return_annotation.get_location().end,
                    },
                    is_public,
                    name,
                    arguments,
                    arguments_span,
                    body: body.unwrap(),
                    doc: None,
                    return_type: return_annotation,
                    end_position: span.end - 1,
                })
            },
        )
}

pub fn definition_parser() -> impl Parser<Token, UntypedDefinition, Error = ParseError> {
    choice((function_parser(), function_parser()))
}

/// Parses a src string into a module
pub fn module_parser(
    src: &str,
    // kind
) -> Result<(UntypedDefinition, ModuleExtra), Vec<ParseError>> {
    let lexer::LexInfo {
        tokens,
        module_extra,
    } = lexer::run(src).unwrap();

    let stream = chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

    // let expr = expression_parser(expression_sequence_parser()).parse(stream);
    let result = definition_parser().parse(stream);
    match result {
        Ok(definition) => Ok((definition, module_extra)),
        Err(parse_errors) => Err(parse_errors),
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    //
    // fn can_parse(src: &str) -> (UntypedExpr, ModuleExtra) {
    //     module_parser(src).unwrap()
    // }
}
