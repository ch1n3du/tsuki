use std::collections::HashMap;

use chumsky::chain::Chain;

use crate::{
    ast::{
        BinaryOp, CallArg, Definition, Function, Module, Span, TypedDefinition, TypedModule,
        UnaryOp, UntypedDefinition, UntypedModule,
    },
    call,
    expr::{IfBranch, TypedExpr, UntypedExpr},
    type_::Type,
};

#[allow(unused)]
pub struct TypeChecker {
    current_module: String,
    top_level_definitions: HashMap<String, Type>,
    scopes: Vec<HashMap<String, Type>>,
    // function_label_maps: HashMap<String, Vec<>>
}

impl TypeChecker {
    pub fn new(module_name: String) -> TypeChecker {
        TypeChecker {
            current_module: module_name,
            scopes: vec![HashMap::new()],
            top_level_definitions: HashMap::new(),
        }
    }

    pub fn check_module(&mut self, module: &UntypedModule) -> TypeResult<TypedModule> {
        let Module {
            name,
            definitions,
            docs,
            type_info,
        } = module;

        // Put dummy types for all definitions
        for definition in definitions {
            self.top_level_definitions
                .insert(definition.get_name(), definition.get_type());
        }

        let mut typed_definitions: Vec<TypedDefinition> = Vec::new();
        for definition in definitions {
            typed_definitions.push(self.check_definition(definition)?);
        }

        Ok(TypedModule {
            name: name.clone(),
            docs: docs.clone(),
            type_info: type_info.clone(),
            definitions: typed_definitions,
        })
    }

    pub fn check_definition(
        &mut self,
        definition: &UntypedDefinition,
    ) -> TypeResult<TypedDefinition> {
        match definition {
            Definition::Fn(func) => Ok(TypedDefinition::Fn(self.check_function(func)?)),
        }
    }

    pub fn check_function(
        &mut self,
        function: &Function<UntypedExpr>,
    ) -> TypeResult<Function<TypedExpr>> {
        let expected_type = Type::Function {
            location: function.type_definition_span,
            type_of_arguments: function.argument_types(),
            return_type: Box::new(function.return_type.clone()),
        };

        // Insert the expected function type to support checking recursive functions
        self.scopes[0].insert(function.name.clone(), expected_type);

        // Create a new scope above the prelude scope to typecheck the function.
        self.push_scope();

        // Insert the arguments at the top of the scope
        for argument in function.arguments.iter() {
            self.scopes.last_mut().unwrap().insert(
                argument
                    .argument_name
                    .get_variable_name()
                    .unwrap()
                    .to_string(),
                argument.annotation.clone(),
            );
        }

        // Typecheck the function body
        let typed_body = self.check_expr(&function.body)?;

        self.pop_scope();

        Ok(Function {
            is_public: function.is_public,
            arguments: function.arguments.clone(),
            arguments_span: function.arguments_span,
            name: function.name.clone(),
            body: typed_body,
            return_type: function.return_type.clone(),
            doc: function.doc.clone(),
            type_definition_span: function.type_definition_span,
            end_position: function.end_position,
        })
    }

    pub fn check_expr(&mut self, expr: &UntypedExpr) -> TypeResult<TypedExpr> {
        let typed_expr = match expr {
            UntypedExpr::Integer { location, value } => TypedExpr::Integer {
                location: location.clone(),
                value: value.clone(),
            },
            UntypedExpr::Boolean { location, value } => TypedExpr::Boolean {
                location: location.clone(),
                value: value.clone(),
            },
            UntypedExpr::Float { location, value } => TypedExpr::Float {
                location: location.clone(),
                value: value.clone(),
            },
            UntypedExpr::String { location, value } => TypedExpr::String {
                location: location.clone(),
                value: value.clone(),
            },
            UntypedExpr::UnaryOp {
                location,
                op,
                value,
            } => self.check_unary_expr(op.clone(), value, location.clone())?,
            UntypedExpr::BinaryOp {
                location,
                op,
                left,
                right,
            } => self.check_binary_expr(op.clone(), left, right, location.clone())?,
            UntypedExpr::Assignment {
                location,
                value,
                pattern,
                type_annotation,
            } => self.check_assignment_expr(
                value,
                pattern.clone(),
                type_annotation.clone(),
                location.clone(),
            )?,
            UntypedExpr::Identifier { location, name } => {
                self.check_identifier_expr(name, location.clone())?
            }
            UntypedExpr::If {
                location,
                branches,
                final_else,
            } => self.check_if_expr(branches, final_else, location.clone())?,
            UntypedExpr::Sequence {
                location,
                expressions,
            } => self.check_sequence_expr(&expressions, location.clone())?,
            UntypedExpr::Function {
                location: _,
                arguments: _,
                arg_span: _,
                body: _,
                return_annotation: _,
            } => todo!("Typecheck anonymous functions"),
            UntypedExpr::Call {
                location,
                arguments,
                function,
            } => self.check_call_expr(function, arguments, location.clone())?,
            UntypedExpr::Pipeline {
                expressions: _,
                one_liner: _,
            } => todo!("Typecheck pipelines"),
            UntypedExpr::FieldAccess {
                location: _,
                label: _,
                container: _,
            } => todo!("Typecheck field accesses"),
            UntypedExpr::Tuple {
                location: _,
                elements: _,
            } => todo!("Typecheck tuple instantiation"),
            UntypedExpr::TupleIndex {
                location: _,
                index: _,
                tuple: _,
            } => todo!("Typecheck tuple indexing"),
        };

        Ok(typed_expr)
    }

    fn check_unary_expr(
        &mut self,
        op: UnaryOp,
        value: &UntypedExpr,
        location: Span,
    ) -> TypeResult<TypedExpr> {
        let typed_value: TypedExpr = self.check_expr(&value)?;
        let type_ = typed_value.get_type();

        match op {
            UnaryOp::Negate => {
                if !type_.is_float() && type_.is_int() {
                    todo!("Handle invalid negation")
                }
            }
            UnaryOp::Not => {
                if !type_.is_bool() {
                    todo!("Handle invalid logical Nots")
                }
            }
        }

        Ok(TypedExpr::UnaryOp {
            location,
            op,
            value: Box::new(typed_value),
        })
    }

    fn check_binary_expr(
        &mut self,
        op: BinaryOp,
        left: &UntypedExpr,
        right: &UntypedExpr,
        location: Span,
    ) -> TypeResult<TypedExpr> {
        let left: TypedExpr = self.check_expr(left)?;
        let right: TypedExpr = self.check_expr(right)?;

        let left_type: Type = left.get_type();
        let right_type: Type = right.get_type();

        let type_: Option<Type> = match (op, left.get_type(), right.get_type()) {
            (BinaryOp::Add, left_type, right_type) => {
                if left_type.is_int() && right_type.is_int() {
                    Some(Type::int_type(location))
                } else if left_type.is_float() && right_type.is_float() {
                    Some(Type::float_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::Subtract, left_type, right_type) => {
                if left_type.is_int() && right_type.is_int() {
                    Some(Type::int_type(location))
                } else if left_type.is_float() && right_type.is_float() {
                    Some(Type::float_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::Multiply, left_type, right_type) => {
                if left_type.is_int() && right_type.is_int() {
                    Some(Type::int_type(location))
                } else if left_type.is_float() && right_type.is_float() {
                    Some(Type::float_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::Divide, left_type, right_type) => {
                if left_type.is_int() && right_type.is_int() {
                    Some(Type::int_type(location))
                } else if left_type.is_float() && right_type.is_float() {
                    Some(Type::float_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::Modulo, left_type, right_type) => {
                if left_type.is_int() && right_type.is_int() {
                    Some(Type::int_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::Or, left_type, right_type) => {
                if left_type.is_bool() && right_type.is_bool() {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::And, left_type, right_type) => {
                if left_type.is_bool() && right_type.is_bool() {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::Eq, left_type, right_type) => {
                // TODO: This is shit
                if left_type == right_type {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::NotEq, left_type, right_type) => {
                // TODO: This is shit
                if left_type == right_type {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::LessThan, left_type, right_type) => {
                if (left_type.is_int() && right_type.is_int())
                    || (left_type.is_float() && right_type.is_float())
                {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::LessThanOrEqual, left_type, right_type) => {
                if (left_type.is_int() && right_type.is_int())
                    || (left_type.is_float() && right_type.is_float())
                {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::GreaterThan, left_type, right_type) => {
                if (left_type.is_int() && right_type.is_int())
                    || (left_type.is_float() && right_type.is_float())
                {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
            (BinaryOp::GreaterThanOrEqual, left_type, right_type) => {
                if (left_type.is_int() && right_type.is_int())
                    || (left_type.is_float() && right_type.is_float())
                {
                    Some(Type::bool_type(location))
                } else {
                    None
                }
            }
        };

        let type_: Type = type_.ok_or(TypeError::InvalidBinaryExpr {
            op,
            left: left_type,
            right: right_type,
            location,
        })?;

        Ok(TypedExpr::BinaryOp {
            location,
            type_,
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn check_assignment_expr(
        &mut self,
        value: &UntypedExpr,
        pattern: String,
        type_annotation: Type,
        location: Span,
    ) -> TypeResult<TypedExpr> {
        let value: TypedExpr = self.check_expr(value)?;

        if value.get_type() != type_annotation {
            return Err(TypeError::AssingmentValueHasIncorrectType {
                name: pattern,
                expected: type_annotation,
                found: value.get_type(),
            });
        }
        self.insert_variable_type(pattern.clone(), type_annotation.clone(), location)?;

        Ok(TypedExpr::Assignment {
            location,
            value: Box::new(value),
            pattern,
            type_annotation,
        })
    }

    fn check_identifier_expr(&mut self, name: &str, location: Span) -> TypeResult<TypedExpr> {
        let type_: Type = self.get_variable_type(name, location)?;

        Ok(TypedExpr::Identifier {
            location,
            type_,
            name: name.to_string(),
        })
    }

    fn check_if_branch(
        &mut self,
        if_branch: &IfBranch<UntypedExpr>,
    ) -> TypeResult<IfBranch<TypedExpr>> {
        let condition: TypedExpr = self.check_expr(&if_branch.condition)?;
        if !condition.get_type().is_bool() {
            //TODO: Throw an error for non boolean condition expressions.
            todo!("Handle non-boolean if block conditions")
        }

        let body: TypedExpr = self.check_expr(&if_branch.body)?;

        Ok(IfBranch {
            condition,
            body,
            location: if_branch.location,
        })
    }

    fn check_if_expr(
        &mut self,
        branches: &[IfBranch<UntypedExpr>],
        final_else: &UntypedExpr,
        location: Span,
    ) -> TypeResult<TypedExpr> {
        // TODO: Added scoping
        let mut if_branches: Vec<IfBranch<TypedExpr>> = Vec::new();

        self.push_scope();
        if_branches.push(self.check_if_branch(&branches[0])?);
        self.pop_scope();

        let first_if_branch_type: Type = if_branches[0].get_type();

        for if_branch in branches.iter().skip(1) {
            self.push_scope();
            let if_branch: IfBranch<TypedExpr> = self.check_if_branch(if_branch)?;
            self.pop_scope();

            if if_branch.get_type() != first_if_branch_type {
                return Err(TypeError::IfBranchesHaveDifferentTypes {
                    first_branch_type: first_if_branch_type,
                    first_branch_location: if_branches[0].location,
                    second_branch_type: if_branch.get_type(),
                    second_branch_location: if_branch.location,
                });
            }

            if_branches.push(if_branch);
        }

        self.push_scope();
        let final_else: TypedExpr = self.check_expr(final_else)?;
        self.pop_scope();
        if final_else.get_type() != first_if_branch_type {
            return Err(TypeError::IfBranchesHaveDifferentTypes {
                first_branch_type: first_if_branch_type,
                first_branch_location: if_branches[0].location,
                second_branch_type: final_else.get_type(),
                second_branch_location: final_else.location(),
            });
        }

        Ok(TypedExpr::If {
            location,
            type_: first_if_branch_type,
            branches: if_branches,
            final_else: Box::new(final_else),
        })
    }

    fn check_sequence_expr(
        &mut self,
        expressions: &[UntypedExpr],
        location: Span,
    ) -> TypeResult<TypedExpr> {
        let mut typed_expressions: Vec<TypedExpr> = Vec::new();

        for expr in expressions {
            let typed_expr: TypedExpr = self.check_expr(expr)?;
            typed_expressions.push(typed_expr);
        }

        Ok(TypedExpr::Sequence {
            location,
            expressions: typed_expressions,
        })
    }

    fn check_call_arg(
        &mut self,
        call_arg: &CallArg<UntypedExpr>,
    ) -> TypeResult<CallArg<TypedExpr>> {
        let typed_expr = self.check_expr(&call_arg.value)?;

        Ok(CallArg {
            label: call_arg.label.clone(),
            location: call_arg.location,
            value: typed_expr,
        })
    }

    fn check_call_expr(
        &mut self,
        function: &UntypedExpr,
        arguments: &[CallArg<UntypedExpr>],
        location: Span,
    ) -> TypeResult<TypedExpr> {
        let function: TypedExpr = self.check_expr(function)?;
        let function_type: Type = function.get_type();
        let mut typed_call_args = Vec::with_capacity(arguments.len());

        // TODO: Arrange args

        for call_arg in arguments {
            typed_call_args.push(self.check_call_arg(call_arg)?)
        }

        if let Type::Function {
            location: _,
            type_of_arguments,
            return_type,
        } = function_type
        {
            if type_of_arguments.len() != typed_call_args.len() {
                todo!(
                    "Arguments are not of the same length, expected {} arguments but got {}",
                    type_of_arguments.len(),
                    typed_call_args.len()
                )
            }

            for (i, (expected_type, actual_type)) in type_of_arguments
                .iter()
                .zip(typed_call_args.iter().map(|arg| arg.value.get_type()))
                .enumerate()
            {
                if *expected_type != actual_type {
                    todo!("Argument \"{i}\" is of an incorrect type expected \"{expected_type:?}\" found \"{actual_type:?}\"",)
                }
            }

            Ok(TypedExpr::Call {
                location,
                type_: return_type.as_ref().clone(),
                arguments: typed_call_args,
                function: Box::new(function),
            })
        } else {
            todo!("Type should be a function")
        }
    }

    // Utility methods
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_variable_type(&mut self, name: &str, location: Span) -> TypeResult<Type> {
        let type_found_in_scopes = self
            .scopes
            .iter()
            .find_map(|scope| scope.get(name).map(|type_| type_.clone()));

        if let Some(type_) = type_found_in_scopes {
            Ok(type_)
        } else {
            self.top_level_definitions
                .get(name)
                .map(|type_| type_.clone())
                .ok_or(TypeError::VariableDoesNotExist {
                    name: name.to_string(),
                    location: location,
                })
        }
    }

    fn insert_variable_type(
        &mut self,
        name: String,
        type_: Type,
        location: Span,
    ) -> TypeResult<()> {
        let variable_exists = self.scopes.iter().any(|scope| scope.contains_key(&name));

        if variable_exists {
            Err(TypeError::VariableAlreadyExists { name, location })
        } else {
            self.scopes.last_mut().unwrap().insert(name, type_);
            Ok(())
        }
    }
}

type TypeResult<T> = Result<T, TypeError>;

// #[derive(Debug, thiserror::Error, Diagnostic, Clone)]
#[derive(Debug)]
pub enum TypeError {
    InvalidUnaryExpr {
        op: UnaryOp,
        value: Type,
        location: Span,
    },
    InvalidBinaryExpr {
        op: BinaryOp,
        left: Type,
        right: Type,
        location: Span,
    },
    IfConditionNotBoolean {
        location: Span,
    },
    IfBranchesHaveDifferentTypes {
        first_branch_type: Type,
        first_branch_location: Span,
        second_branch_type: Type,
        second_branch_location: Span,
    },
    VariableDoesNotExist {
        name: String,
        location: Span,
    },
    VariableAlreadyExists {
        name: String,
        location: Span,
    },
    AssingmentValueHasIncorrectType {
        name: String,
        expected: Type,
        found: Type,
    },
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

    fn parse_and_typecheck_expr(src: &str) -> TypedExpr {
        // Lex the tokens and convert them into a stream
        let LexInfo { tokens, .. } = lexer::run(src).unwrap();
        let stream =
            chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let untyped_expr = parser::expression_sequence_parser().parse(stream).unwrap();

        let mut checker = TypeChecker::new("testing_typechecking".to_string());
        checker.check_expr(&untyped_expr).unwrap()
    }

    #[test]
    fn can_typecheck_int_literals() {
        assert_eq!(
            parse_and_typecheck_expr("100_000").get_type(),
            Type::int_type(Span::empty())
        );

        assert_eq!(
            parse_and_typecheck_expr("14819").get_type(),
            Type::int_type(Span::empty())
        );
    }

    #[test]
    fn can_typecheck_float_literals() {
        assert_eq!(
            parse_and_typecheck_expr("10.23").get_type(),
            Type::float_type(Span::empty())
        );
    }

    #[test]
    fn can_typecheck_string_literals() {
        assert_eq!(
            parse_and_typecheck_expr("@\"Hello World!\"").get_type(),
            Type::string_type(Span::empty())
        );
    }

    #[test]
    fn can_typecheck_bool_literals() {
        assert_eq!(
            parse_and_typecheck_expr("true").get_type(),
            Type::bool_type(Span::empty())
        );

        assert_eq!(
            parse_and_typecheck_expr("false").get_type(),
            Type::bool_type(Span::empty())
        );
    }

    #[test]
    fn can_typecheck_let_expr() {
        assert_eq!(
            parse_and_typecheck_expr("let pi: Float = 3.14").get_type(),
            Type::unit_type(Span::empty())
        )
    }

    #[test]
    fn can_typecheck_sequence_expr() {
        let src = r#"
        let x: Int = 23
        x + 2
        "#;

        assert_eq!(
            parse_and_typecheck_expr(src).get_type(),
            Type::int_type(Span::empty())
        )
    }

    #[test]
    fn can_typecheck_identifiers_expr() {
        let src = r#"
        let x: String = @"This is a Tsuki string"
        x
        "#;

        assert_eq!(
            parse_and_typecheck_expr(src).get_type(),
            Type::string_type(Span::empty())
        )
    }

    #[test]
    fn can_typecheck_if_exprs() {
        let src = r#"
        let x: Int = 32
        
        if x > 42 {
          2.3
        } else {
          3.2
        }
        "#;

        assert_eq!(
            parse_and_typecheck_expr(src).get_type(),
            Type::float_type(Span::empty())
        )
    }

    pub fn parse_and_typecheck_function(src: &str) -> TypedFunction {
        // Lex the tokens and convert them into a stream
        let LexInfo { tokens, .. } = lexer::run(src).unwrap();
        let stream =
            chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

        let untyped_func = parser::function_parser().parse(stream).unwrap();

        let mut checker = TypeChecker::new("testing_typechecking".to_string());
        // println!("Typed Expresion: {typed_expr:#?}")
        checker.check_function(&untyped_func).unwrap()
    }

    #[test]
    fn can_typecheck_add_one_func() {
        let src = r#"
        fn add_one(n: Int) -> Int {
            n + 1
        }
        "#;
        let typed_func = parse_and_typecheck_function(src);
        assert_eq!(
            typed_func.argument_types(),
            vec![Type::int_type(Span::empty())]
        );

        assert_eq!(typed_func.return_type, Type::int_type(Span::empty()))
    }

    #[test]
    fn can_typecheck_fib_func() {
        let src = r#"
        fn fib(n: Int) -> Int {
            if n < 2 {
                1
            } else {
                fib(n-1) + fib(n-2)
            }
        }
        "#;
        let typed_func = parse_and_typecheck_function(src);
        assert_eq!(
            typed_func.argument_types(),
            vec![Type::int_type(Span::empty())]
        );

        assert_eq!(typed_func.return_type, Type::int_type(Span::empty()))
    }

    #[test]
    fn can_typecheck_factorial_func() {
        let src = r#"
        fn factorial(n: Int) -> Int {
            if n == 1 {
                1
            } else {
                n * factorial(n-1)
            }
        }
        "#;
        let typed_func = parse_and_typecheck_function(src);
        assert_eq!(
            typed_func.argument_types(),
            vec![Type::int_type(Span::empty())]
        );

        assert_eq!(typed_func.return_type, Type::int_type(Span::empty()))
    }

    fn parse_and_typecheck_module(src: &str) -> TypedModule {
        let (untyped_module, _module_extra) = parser::module_parser(src).unwrap();

        let mut checker = TypeChecker::new("testing_typechecking".to_string());
        checker.check_module(&untyped_module).unwrap()
    }

    #[test]
    fn can_typecheck_fibonnacci_module() {
        let src = r#"
        module fibonnacci

        fn fib(n: Int) -> Int {
            if n < 2 {
                1
            } else {
                fib(n-1) + fib(n-2)
            }
        }
        "#;

        parse_and_typecheck_module(&src);
    }
}
