use crate::{
    ast::{self, Argument, BinaryOp, CallArg, Span, UnaryOp, UntypedCallArg},
    type_::Type,
};

#[derive(Debug, Clone)]
pub enum UntypedExpr {
    Integer {
        location: Span,
        value: String,
    },
    Boolean {
        location: Span,
        value: bool,
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
        value: Box<Self>,
    },
    BinaryOp {
        location: Span,
        op: BinaryOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    Sequence {
        location: Span,
        expressions: Vec<Self>,
    },
    Pipeline {
        expressions: Vec<Self>,
        one_liner: bool,
    },
    Assignment {
        location: Span,
        value: Box<Self>,
        // TODO: Change to a `Pattern`
        pattern: String,
        type_annotation: Type,
    },
    FieldAccess {
        location: Span,
        label: String,
        container: Box<Self>,
    },
    Tuple {
        location: Span,
        elements: Vec<Self>,
    },
    TupleIndex {
        location: Span,
        index: usize,
        tuple: Box<Self>,
    },
    Function {
        location: Span,
        arguments: Vec<Argument>,
        body: Box<Self>,
        return_annotation: Option<Type>,
    },
    Call {
        location: Span,
        arguments: Vec<CallArg<UntypedExpr>>,
        function: Box<Self>,
    },
}

pub const DEFAULT_TODO_STR: &str = "tsuki::todo";
pub const DEFAULT_ERROR_STR: &str = "tsuki::error";

impl UntypedExpr {
    // pub fn todo(reason: Option<Self>, location: Span) -> Self {
    //     UntypedExpr::Trace {
    //         location,
    //         kind: TraceKind::Todo,
    //         then: Box::new(UntypedExpr::ErrorTerm { location }),
    //         text: Box::new(reason.unwrap_or_else(|| UntypedExpr::String {
    //             location,
    //             value: DEFAULT_TODO_STR.to_string(),
    //         })),
    //     }
    // }

    // pub fn fail(reason: Option<Self>, location: Span) -> Self {
    //     if let Some(reason) = reason {
    //         UntypedExpr::Trace {
    //             location,
    //             kind: TraceKind::Error,
    //             then: Box::new(UntypedExpr::ErrorTerm { location }),
    //             text: Box::new(reason),
    //         }
    //     } else {
    //         UntypedExpr::ErrorTerm { location }
    //     }
    // }

    pub fn tuple_index(self, index: usize, location: Span) -> Self {
        UntypedExpr::TupleIndex {
            location: self.location().union(location),
            index,
            tuple: Box::new(self),
        }
    }

    pub fn field_access(self, label: String, location: Span) -> Self {
        UntypedExpr::FieldAccess {
            location: self.location().union(location),
            label,
            container: Box::new(self),
        }
    }

    pub fn call(self, args: Vec<UntypedCallArg>, location: Span) -> Self {
        UntypedExpr::Call {
            location: self.location().union(location),
            function: Box::new(self),
            arguments: args,
        }
    }

    pub fn append_in_sequence(self, next: Self) -> Self {
        let location = Span {
            start: self.location().start,
            end: next.location().end,
        };

        match (self.clone(), next.clone()) {
            (
                Self::Sequence {
                    expressions: mut current_expressions,
                    ..
                },
                Self::Sequence {
                    expressions: mut next_expressions,
                    ..
                },
            ) => {
                current_expressions.append(&mut next_expressions);

                Self::Sequence {
                    location,
                    expressions: current_expressions,
                }
            }
            (
                _,
                Self::Sequence {
                    expressions: mut next_expressions,
                    ..
                },
            ) => {
                let mut current_expressions = vec![self];

                current_expressions.append(&mut next_expressions);

                Self::Sequence {
                    location,
                    expressions: current_expressions,
                }
            }

            (_, _) => Self::Sequence {
                location,
                expressions: vec![self, next],
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
            | Self::FieldAccess { location, .. }
            | Self::Tuple { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::Function { location, .. }
            | Self::Call { location, .. }
            | Self::Sequence { location, .. } => *location,
            Self::Pipeline { expressions, .. } => expressions
                .first()
                .unwrap()
                .location()
                .union(expressions.last().unwrap().location()),
        }
    }
}
