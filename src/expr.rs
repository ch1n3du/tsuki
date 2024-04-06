use crate::{
    ast::{self, Argument, BinaryOp, CallArg, Span, UnaryOp},
    type_annotation::TypeAnnotation,
};

// Represent how a function was written so that we can format it back.
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum FunctionStyle {
    Plain,
    Capture,
}

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
        type_annotation: Option<TypeAnnotation>,
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
        function_style: FunctionStyle,
        arguments: Vec<Argument<()>>,
        body: Box<Self>,
        return_annotation: Option<TypeAnnotation>,
    },
    Call {
        location: Span,
        arguments: Vec<CallArg<UntypedExpr>>,
        function: Box<Self>,
    },
}

pub const DEFAULT_TODO_STR: &str = "aiken::todo";
pub const DEFAULT_ERROR_STR: &str = "aiken::error";

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

    pub fn call(self, args: Vec<CallArg<Option<UntypedExpr>>>, location: Span) -> Self {
        let mut holes = Vec::new();

        let args = args
            .into_iter()
            .enumerate()
            .map(|(index, a)| match a {
                CallArg {
                    value: Some(value),
                    label,
                    location,
                } => CallArg {
                    value,
                    label,
                    location,
                },
                CallArg {
                    value: None,
                    label,
                    location,
                } => {
                    let name = format!("{}__{index}", ast::CAPTURE_VARIABLE);

                    holes.push(ast::Argument {
                        location: Span::empty(),
                        annotation: None,
                        doc: None,
                        argument_name: ast::ArgumentName::Named {
                            label: name.clone(),
                            name,
                            location: Span::empty(),
                        },
                        type_: (),
                    });

                    ast::CallArg {
                        label,
                        location,
                        value: UntypedExpr::Identifier {
                            location,
                            name: format!("{}__{index}", ast::CAPTURE_VARIABLE),
                        },
                    }
                }
            })
            .collect();

        let call = UntypedExpr::Call {
            location: self.location().union(location),
            function: Box::new(self),
            arguments: args,
        };

        // If some arguments are not supplied the return a closure that takes those as an
        // argument
        if holes.is_empty() {
            call
        } else {
            UntypedExpr::Function {
                location: call.location(),
                function_style: FunctionStyle::Capture,
                arguments: holes,
                body: Box::new(call),
                return_annotation: None,
            }
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
