use tsuki::lexer::run;
use tsuki::parser::module_parser;

fn main() {
    let src = "- 1 + 3";
    let tokens = run(src).unwrap();
    println!("{tokens:?}");
    let expr = module_parser(src);
    println!("Expression: {expr:?}");

    let raw: String = "1000_000".to_string();
    let new = raw.replace("_", "");
    println!("{new}")
}
