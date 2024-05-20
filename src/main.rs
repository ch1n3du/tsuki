use chumsky::Parser;
use tsuki::typechecker::TypeChecker;
use tsuki::{
    ast,
    lexer::{self, LexInfo},
    parser,
};

fn main() {
    // let src = "- 1 + 3";
    // let src = r#"
    // fn fib(n: Int) -> Int {
    //      if n < 2 {
    //         1
    //      } else {
    //          fib(n-1) + fib(n-2)
    //      }
    //     2 + 2
    // }
    // "#;
    let src = r#"23 - 4 > 42"#;

    println!("Source: {src}");
    let LexInfo { tokens, .. } = lexer::run(src).unwrap();
    let stream = chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());
    // println!("{tokens:#?}");
    let untyped_expr = parser::expression_sequence_parser().parse(stream).unwrap();
    println!("Untyped Expression: {untyped_expr:#?}");

    let mut checker = TypeChecker::new("testing_typechecking".to_string());
    let typed_expr = checker.check_expr(&untyped_expr).unwrap();
    println!("Typed Expresion: {typed_expr:#?}")
}
