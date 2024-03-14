use crate::{
    ast::{BinaryOp, Span, UnaryOp},
    type_annotation::TypeAnnotation,
};

#[derive(Debug)]
pub enum UntypedExpr {
    Integer {
        location: Span,
        value: String,
    },
    Float {
        location: Span,
        value: String,
    },
    Boolean {
        location: Span,
        value: bool,
    },
    String {
        location: Span,
        value: String,
    },
    Identifier {
        location: Span,
        name: String,
    },
    UnaryOp {
        location: Span,
        op: UnaryOp,
        value: Box<UntypedExpr>,
    },
    BinaryOp {
        location: Span,
        op: BinaryOp,
        left: Box<UntypedExpr>,
        right: Box<UntypedExpr>,
    },
    Assignment {
        location: Span,
        value: Box<UntypedExpr>,
        // TODO: Implement patterns
        pattern: String,
        type_annotation: Option<TypeAnnotation>,
    },
    Sequence {
        location: Span,
        expressions: Vec<UntypedExpr>,
    },
    Pipeline {
        expressions: Vec<UntypedExpr>,
        one_liner: bool,
    },
    // TODO: Variable assignment
    // TODO: Functions and function calls
}

impl UntypedExpr {
    /// Append two `UntypedExpr`s into an `UntypedExpr::Sequence {..}`
    pub fn append_in_sequence(self, next_expression: UntypedExpr) -> UntypedExpr {
        let location: Span = self.location().union(next_expression.location());

        match (self, next_expression) {
            (
                UntypedExpr::Sequence {
                    expressions: mut current_expressions,
                    ..
                },
                UntypedExpr::Sequence {
                    expressions: mut next_expressions,
                    ..
                },
            ) => {
                current_expressions.append(&mut next_expressions);

                UntypedExpr::Sequence {
                    location,
                    expressions: current_expressions,
                }
            }
            (
                non_sequence_expression,
                UntypedExpr::Sequence {
                    expressions: mut next_expressions,
                    ..
                },
            ) => {
                let mut current_expressions = vec![non_sequence_expression];
                current_expressions.append(&mut next_expressions);

                UntypedExpr::Sequence {
                    location,
                    expressions: current_expressions,
                }
            }
            (non_sequence_expression_1, non_sequence_expression_2) => UntypedExpr::Sequence {
                location,
                expressions: vec![non_sequence_expression_1, non_sequence_expression_2],
            },
        }
    }

    pub fn location(&self) -> Span {
        match self {
            Self::Integer { location, .. }
            | Self::Float { location, .. }
            | Self::Boolean { location, .. }
            | Self::String { location, .. }
            | Self::Identifier { location, .. }
            | Self::UnaryOp { location, .. }
            | Self::BinaryOp { location, .. }
            | Self::Assignment { location, .. }
            | Self::Sequence { location, .. } => *location,
            Self::Pipeline { expressions, .. } => expressions
                .first()
                .unwrap()
                .location()
                .union(expressions.last().unwrap().location()),
        }
    }
}
