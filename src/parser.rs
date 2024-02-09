use chumsky::{
    primitive::{choice, just, todo},
    recursive::{recursive, Recursive},
    select, Parser,
};

use crate::{
    ast::{self, Span},
    expr::{self, UntypedExpr},
    extra::ModuleExtra,
    lexer,
};

use super::{error::ParseError, token::Token};

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

// pub fn sequence_parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
//     recursive(|sequence| {
//         expression_parser(sequence.clone())
//             .then(sequence.repeated())
//             .foldl(|current, next| current.append_in_sequence(next))
//     })
// }

pub fn expression_parser<'a>(// sequence: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    recursive(|expression| {
        choice((
            // fail_todo_trace(expression.clone(), sequence.clone()),
            pure_expression(expression),
        ))
    })
}

pub fn pure_expression<'a>(
    // sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    let val = select! { |span|
        Token::Integer { value, has_underscores } => {
            UntypedExpr::Integer { location: span, value: value.replace("_", "") }
        }
    }
    .labelled("value");

    // Unary Operand
    let unary_op = choice((
        just(Token::Not).to(ast::UnaryOp::Not),
        choice((just(Token::Minus), just(Token::NewLineMinus))).to(ast::UnaryOp::Negate),
    ));

    let unary_expr = unary_op
        .map_with_span(|op: ast::UnaryOp, span: Span| (op, span))
        .repeated()
        // TODO .then(chained(sequence, expression))
        .then(val)
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
            op: op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
        .boxed();

    // Conjunction
    let or_op = just(Token::And).to(ast::BinaryOp::And);
    let or_expr = comparison
        .clone()
        .then(or_op.then(comparison).repeated())
        .foldl(|left_expr, (op, right_expr)| UntypedExpr::BinaryOp {
            location: left_expr.location().union(right_expr.location()),
            op: op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
        .boxed();

    // Disjunction
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
