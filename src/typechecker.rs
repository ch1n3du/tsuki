use std::collections::HashMap;

use crate::{
    ast::{BinaryOp, Span, UnaryOp},
    expr::{TypedExpr, UntypedExpr},
    type_::Type,
};

pub struct TypeChecker {
    current_module: String,
    environment: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new(module_name: String) -> TypeChecker {
        TypeChecker {
            current_module: module_name,
            environment: HashMap::new(),
        }
    }

    pub fn check_expr(&mut self, expr: &UntypedExpr) -> TypeResult<TypedExpr> {
        let typed_expr = match expr {
            UntypedExpr::Integer { location, value } => TypedExpr::Integer {
                location: location.clone(),
                type_: Type::int_type(location.clone()),
                value: value.clone(),
            },
            UntypedExpr::Boolean { location, value } => TypedExpr::Boolean {
                location: location.clone(),
                type_: Type::bool_type(location.clone()),
                value: value.clone(),
            },
            UntypedExpr::Float { location, value } => TypedExpr::Float {
                location: location.clone(),
                type_: Type::float_type(location.clone()),
                value: value.clone(),
            },
            UntypedExpr::String { location, value } => TypedExpr::String {
                location: location.clone(),
                type_: Type::string_type(location.clone()),
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
            } => todo!("Typecheck assignment"),
            UntypedExpr::Identifier { location, name } => todo!("Typecheck identifiers"),
            UntypedExpr::Sequence {
                location,
                expressions,
            } => self.check_sequence_expr(&expressions, location.clone())?,
            UntypedExpr::If {
                location,
                branches,
                final_else,
            } => todo!("Typecheck if/else expressions"),
            UntypedExpr::Function {
                location,
                arguments,
                body,
                return_annotation,
            } => todo!("Typecheck functions"),
            UntypedExpr::Call {
                location,
                arguments,
                function,
            } => todo!("Typecheck function calls"),
            UntypedExpr::Pipeline {
                expressions,
                one_liner,
            } => todo!("Typecheck pipelines"),
            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => todo!("Typecheck field accesses"),
            UntypedExpr::Tuple { location, elements } => todo!("Typecheck tuple instantiation"),
            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
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
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, Span},
        expr::TypedExpr,
        lexer::{self, LexInfo},
        parser,
        type_::Type,
        typechecker::TypeChecker,
    };
    use chumsky::Parser;

    fn parse_and_typecheck_expr(src: &str) -> TypedExpr {
        // println!("Source: {src}");
        let LexInfo { tokens, .. } = lexer::run(src).unwrap();
        let stream =
            chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

        // println!("{tokens:#?}");
        let untyped_expr = parser::expression_sequence_parser().parse(stream).unwrap();
        // println!("Untyped Expression: {untyped_expr:#?}");

        let mut checker = TypeChecker::new("testing_typechecking".to_string());
        // println!("Typed Expresion: {typed_expr:#?}")
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
}
