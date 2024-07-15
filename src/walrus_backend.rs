use chumsky::primitive::todo;
// use owo_colors::OwoColorize;
use walrus::{ir::*, FunctionId, InstrSeqBuilder};
use walrus::{FunctionBuilder, Module, ModuleConfig, ValType};

use crate::ast::{self, TypedDefinition, TypedFunction, TypedModule};
use crate::error::TResult;
use crate::expr::TypedExpr;
use crate::type_::Type;

#[derive(Debug)]
struct LocalData {
    name: String,
    id: LocalId,
    scope: u32,
}

pub struct Compiler {
    locals: Vec<LocalData>,
    module: Module,
    current_scope: u32,
}

impl Compiler {
    pub fn new() -> Self {
        let mut module = Module::with_config(ModuleConfig::new());
        // let start_func_builder = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        Compiler {
            locals: Vec::new(),
            module,
            current_scope: 0,
        }
    }

    pub fn emit_wasm(mut self) -> Vec<u8> {
        self.module.emit_wasm()
    }

    pub fn emit_wasm_file(mut self, file_path: &str) {
        self.module.emit_wasm_file(file_path).unwrap()
    }

    pub fn compile_module(&mut self, module: &TypedModule) -> TResult<()> {
        for definition in module.definitions.iter() {
            self.compile_definition(definition)?;
        }

        Ok(())
    }

    pub fn compile_definition(&mut self, definition: &TypedDefinition) -> TResult<()> {
        match definition {
            TypedDefinition::Fn(func) => {
                self.compile_function(func)?;
            }
        }

        Ok(())
    }

    pub fn compile_function(&mut self, function: &TypedFunction) -> TResult<()> {
        // Convert the argument types to WASM value types
        let argument_val_types: Vec<ValType> = function
            .argument_types()
            .iter()
            .map(tsuki_type_to_val_type)
            .collect();

        let return_type: ValType = tsuki_type_to_val_type(&function.return_type);
        // Create a local to store the function's return value
        let return_local = self.module.locals.add(return_type);

        // Generate `LocalId`s for the functions arguments and add them to the locals list
        let mut argument_ids: Vec<LocalId> = Vec::new();
        for (argument, val_type) in function.arguments.iter().zip(argument_val_types.iter()) {
            let argument_local_id =
                self.add_local(argument.get_variable_name().unwrap(), val_type.clone())?;
            argument_ids.push(argument_local_id)
        }

        // Start the scope of the function to take account for the function's parameters.
        self.start_scope();

        let mut func_builder =
            FunctionBuilder::new(&mut self.module.types, &argument_val_types, &[return_type]);
        let mut func_body = func_builder.func_body();

        // Compile the function body
        self.compile_expr(&mut func_body, &function.body)?;

        // End the scope of the function
        let prev_scope_local_count = self.end_scope()?;
        // Store the function result in the return local
        func_body.local_set(return_local);

        // Pop the previous locals
        self.pop_n_locals(&mut func_body, prev_scope_local_count);

        func_body.local_get(return_local);

        let func_id = func_builder.finish(argument_ids, &mut self.module.funcs);

        self.module.exports.add(&function.name, func_id);
        // Ok(func_id)
        Ok(())
    }

    fn compile_expr(&mut self, func_body: &mut InstrSeqBuilder, expr: &TypedExpr) -> TResult<()> {
        match expr {
            TypedExpr::Integer { location: _, value } => {
                func_body.i32_const(*value);
                Ok(())
            }
            TypedExpr::Float { location: _, value } => {
                func_body.f32_const(*value);
                Ok(())
            }
            TypedExpr::Boolean { location: _, value } => {
                let value = match *value {
                    true => 1,
                    false => 0,
                };
                func_body.i32_const(value);
                Ok(())
            }
            TypedExpr::String {
                location: _,
                value: _,
            } => todo!(),
            TypedExpr::UnaryOp {
                location: _,
                op,
                value,
            } => {
                match op {
                    ast::UnaryOp::Not => {
                        self.compile_expr(func_body, &value)?;
                        func_body.unop(UnaryOp::I32Eqz);
                    }
                    ast::UnaryOp::Negate => {
                        func_body.i32_const(0x8000);
                        self.compile_expr(func_body, &value)?;
                        func_body.binop(BinaryOp::I32And);
                    }
                }
                Ok(())
            }
            TypedExpr::BinaryOp {
                location: _,
                type_,
                op,
                left,
                right,
            } => {
                self.compile_expr(func_body, &left)?;
                self.compile_expr(func_body, &right)?;

                let lhs_type = left.get_type();

                match op {
                    ast::BinaryOp::Add => {
                        let opcode = if type_.is_int() {
                            BinaryOp::I32Add
                        } else if type_.is_float() {
                            BinaryOp::F32Add
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::Subtract => {
                        let opcode = if type_.is_int() {
                            BinaryOp::I32Sub
                        } else if type_.is_float() {
                            BinaryOp::F32Sub
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::Multiply => {
                        let opcode = if type_.is_int() {
                            BinaryOp::I32Mul
                        } else if type_.is_float() {
                            BinaryOp::F32Mul
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::Divide => {
                        let opcode = if type_.is_int() {
                            BinaryOp::I32DivS
                        } else if type_.is_float() {
                            BinaryOp::F32Div
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::Modulo => {
                        func_body.binop(BinaryOp::I32RemS);
                    }

                    ast::BinaryOp::And => {
                        func_body.binop(BinaryOp::I32And);
                    }
                    ast::BinaryOp::Or => {
                        func_body.binop(BinaryOp::I32Or);
                    }

                    ast::BinaryOp::Eq => {
                        let opcode = if lhs_type.is_int() || lhs_type.is_bool() {
                            BinaryOp::I32Eq
                        } else if lhs_type.is_float() {
                            BinaryOp::F32Eq
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::NotEq => {
                        let opcode = if lhs_type.is_int() || lhs_type.is_bool() {
                            BinaryOp::I32Ne
                        } else if lhs_type.is_float() {
                            BinaryOp::F32Ne
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }

                    ast::BinaryOp::GreaterThan => {
                        let opcode = if lhs_type.is_int() {
                            BinaryOp::I32GtS
                        } else if lhs_type.is_float() {
                            BinaryOp::F32Gt
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::GreaterThanOrEqual => {
                        let opcode = if lhs_type.is_int() {
                            BinaryOp::I32GeS
                        } else if lhs_type.is_float() {
                            BinaryOp::F32Ge
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::LessThan => {
                        let opcode = if lhs_type.is_int() {
                            BinaryOp::I32LtS
                        } else if lhs_type.is_float() {
                            BinaryOp::F32Lt
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                    ast::BinaryOp::LessThanOrEqual => {
                        let opcode = if lhs_type.is_int() {
                            BinaryOp::I32LeS
                        } else if lhs_type.is_float() {
                            BinaryOp::F32Le
                        } else {
                            unreachable!()
                        };

                        func_body.binop(opcode);
                    }
                }

                Ok(())
            }
            TypedExpr::Assignment {
                location: _,
                type_annotation,
                value,
                pattern,
            } => {
                self.compile_expr(func_body, &value)?;
                self.add_local(&pattern, tsuki_type_to_val_type(type_annotation))?;
                Ok(())
            }
            TypedExpr::Identifier {
                location: _,
                type_: _,
                name,
            } => {
                // println!("Name: {name}");
                // println!(" locals: {:#?}", self.locals);
                let local_id = self.get_local_id(&name).unwrap();
                func_body.local_get(local_id);
                Ok(())
            }
            TypedExpr::If {
                location: _,
                type_,
                branches,
                final_else,
            } => {
                // Builds if/else expressions bottom-up

                let type_: ValType = tsuki_type_to_val_type(type_);

                let last_if = branches.last().unwrap();

                // Build last if/else branches
                let mut then_block = func_body.dangling_instr_seq(type_);
                let then_block_id = self.compile_block(&mut then_block, &last_if.body, type_)?;

                let mut else_block = func_body.dangling_instr_seq(type_);
                let else_block_id = self.compile_block(&mut else_block, &final_else, type_)?;

                let mut prev_condition = &last_if.condition;
                let mut prev_if_else = IfElse {
                    consequent: then_block_id,
                    alternative: else_block_id,
                };

                for if_branch in branches.iter().rev().skip(1) {
                    let mut then_block = func_body.dangling_instr_seq(type_);
                    let then_block_id =
                        self.compile_block(&mut then_block, &last_if.body, type_)?;

                    let mut else_block = func_body.dangling_instr_seq(type_);
                    let else_block_id = else_block.id();
                    self.compile_expr(&mut else_block, prev_condition)?;
                    else_block.instr(prev_if_else.clone());

                    prev_if_else = IfElse {
                        consequent: then_block_id,
                        alternative: else_block_id,
                    };
                    prev_condition = &if_branch.condition;
                }

                // Place the first if/else condition at the top of the stack.
                self.compile_expr(func_body, prev_condition)?;
                func_body.instr(prev_if_else);

                Ok(())
            }
            TypedExpr::Sequence {
                location: _,
                expressions,
            } => {
                // Flag to know when to insert `drop` for non-let expressions
                // to ensure they don't interfere with the order of the locals.
                let mut prev_expr_was_non_let = false;

                for expr in expressions.iter() {
                    if prev_expr_was_non_let {
                        func_body.drop();
                    }

                    self.compile_expr(func_body, expr)?;

                    // Set the flag
                    prev_expr_was_non_let = !matches!(expr, TypedExpr::Assignment { .. });
                }

                Ok(())
            }
            TypedExpr::Call {
                location: _,
                type_: _,
                arguments: _,
                function: _,
            } => todo!(),
            TypedExpr::Pipeline {
                expressions: _,
                one_liner: _,
            } => todo!(),
            TypedExpr::FieldAccess {
                location: _,
                type_: _,
                label: _,
                container: _,
            } => todo!(),
            TypedExpr::Tuple {
                location: _,
                type_: _,
                elements: _,
            } => todo!(),
            TypedExpr::TupleIndex {
                location: _,
                type_: _,
                index: _,
                tuple: _,
            } => todo!(),
            TypedExpr::Function {
                location: _,
                type_: _,
                arguments: _,
                arg_span: _,
                body: _,
                return_annotation: _,
            } => todo!(),
        }
    }

    fn compile_block(
        &mut self,
        instr_seq: &mut InstrSeqBuilder,
        block: &TypedExpr,
        val_type: ValType,
    ) -> TResult<InstrSeqId> {
        let return_type: ValType = val_type;

        // Create a local to store the block's return value
        let return_local = self.module.locals.add(return_type);

        self.start_scope();

        // Compile the block expression
        self.compile_expr(instr_seq, &block)?;

        // End the scope of the block
        let current_scope_local_count = self.end_scope()?;

        // Store the block's result in the return local
        instr_seq.local_set(return_local);

        // Pop the locals introduced by this block
        self.pop_n_locals(instr_seq, current_scope_local_count);

        instr_seq.local_get(return_local);

        Ok(instr_seq.id())
    }

    fn start_scope(&mut self) {
        self.current_scope += 1;
    }

    fn end_scope(&mut self) -> TResult<usize> {
        if self.current_scope == 0 {
            todo!("Can't pop the last scope")
        }

        // Assume that all locals are from the current scope
        let mut number_of_current_scope_locals = self.locals.len();
        for (i, local_data) in self.locals.iter().enumerate().rev() {
            // Once a local from a lower scope is found subtract it's index + 1 from the total number of locals
            if local_data.scope < self.current_scope {
                number_of_current_scope_locals -= i + 1;
                break;
            }
        }

        // Decrement the current scope
        self.current_scope -= 1;

        Ok(number_of_current_scope_locals)
    }

    fn add_local(&mut self, name: &str, val_type: ValType) -> TResult<LocalId> {
        let id = self.module.locals.add(val_type);
        self.locals.push(LocalData {
            name: name.to_string(),
            id,
            scope: self.current_scope,
        });

        Ok(id)
    }

    fn get_local_id(&mut self, name: &str) -> Option<LocalId> {
        for local_data in self.locals.iter().rev() {
            if local_data.name == name {
                return Some(local_data.id);
            }
        }

        None
    }

    fn pop_n_locals(&mut self, func_body: &mut InstrSeqBuilder, n: usize) {
        for _ in 0..n {
            self.locals.pop();
            func_body.drop();
        }
    }
}

fn tsuki_type_to_val_type(type_: &Type) -> walrus::ValType {
    if type_.is_int() {
        ValType::I32
    } else if type_.is_float() {
        ValType::F32
    } else if type_.is_bool() {
        ValType::I32
    } else {
        todo!("Should throw an error")
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, Span, TypedFunction, TypedModule},
        expr::TypedExpr,
        lexer::{self, LexInfo},
        parser,
        type_::Type,
        typechecker::TypeChecker,
    };
    use chumsky::Parser;

    use super::Compiler;

    fn try_to_compile_func(src: &str) {
        // Lex the tokens and convert them into a stream
        let LexInfo { tokens, .. } = lexer::run(src).unwrap();
        let stream =
            chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let untyped_func = parser::function_parser().parse(stream).unwrap();

        let mut checker = TypeChecker::new("testing_typechecking".to_string());
        // println!("Typed Expresion: {typed_expr:#?}")
        let checked_func = checker.check_function(&untyped_func).unwrap();

        let mut compy = Compiler::new();
        compy.compile_function(&checked_func).unwrap();
        compy.emit_wasm_file("test.wasm");
    }

    #[test]
    fn can_compile_add_one_func() {
        try_to_compile_func(
            r#"
        fn add_one(x: Int) -> Int {
            let a: Int = 42
            let b: Int = 32
            let c: Int = 12
            let z: Float = 23.1
            if true {
                a
            } else if false {
                c
            } else {
                c
            }
        }
        "#,
        );
    }
}
