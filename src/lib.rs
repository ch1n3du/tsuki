use lexer::LexInfo;
use typechecker::TypeChecker;
use walrus_backend::Compiler;

mod anf_tree;
pub mod ast;
mod call;
pub mod cli;
mod error;
pub mod expr;
mod extra;
mod field_map;
pub mod lexer;
pub mod parser;
mod pattern;
mod token;
mod type_;
pub mod typechecker;
mod utils;

mod walrus_backend;

use chumsky::Parser;

pub fn try_to_compile_module_to_file(src: &str, output_file_path: &str) {
    let (untyped_module, module_extra) = parser::module_parser(src).unwrap();
    let module_name = untyped_module.name.clone();

    println!("🏗️  Successfully parsed '{module_name}' module");

    let mut checker = TypeChecker::new(untyped_module.name.clone());

    // println!("Typed Expresion: {typed_expr:#?}")
    let typed_module = checker.check_module(&untyped_module).unwrap();

    println!("🔍 Successfully typechecked '{module_name}' module");

    println!("⚙️  Compiling '{module_name}' module.");
    let mut compy = Compiler::new();
    compy.compile_module(&typed_module).unwrap();
    println!("✅ Successfully compiled '{module_name}' module");

    println!("✍  Emmitting '{module_name}' module to '{output_file_path}'.");
    compy.emit_wasm_file(&output_file_path);
    println!("✅ Succcessfully emitted code")
}
