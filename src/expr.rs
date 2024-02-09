use crate::ast::{BinaryOp, Span, UnaryOp};

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
    pub fn location(&self) -> Span {
        match self {
            Self::Integer { location, .. }
            | Self::Float { location, .. }
            | Self::String { location, .. }
            | Self::Identifier { location, .. }
            | Self::UnaryOp { location, .. }
            | Self::BinaryOp { location, .. }
            | Self::Sequence { location, .. } => *location,
            Self::Pipeline { expressions, .. } => expressions
                .first()
                .unwrap()
                .location()
                .union(expressions.last().unwrap().location()),
        }
    }
}
