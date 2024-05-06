use std::ops::Range;

use crate::{expr::UntypedExpr, type_::Type};

pub type UntypedDefinition = Definition<UntypedExpr>;

#[derive(Debug)]
pub enum Definition<Expr> {
    Fn(Function<Expr>),
}

pub type UntypedFunction = Function<UntypedExpr>;

#[derive(Debug)]
pub struct Function<Expr> {
    pub is_public: bool,
    pub arguments: Vec<Argument>,
    pub name: String,
    pub body: Expr,
    pub return_type: Type,
    pub doc: Option<String>,
    // Span of the function type definition
    pub location: Span,
    // Location of actual end of the function
    pub end_position: usize,
}

pub type UntypedCallArg = CallArg<UntypedExpr>;

#[derive(Debug, Clone)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub location: Span,
    pub value: A,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub argument_name: ArgumentName,
    pub annotation: Type,
    pub doc: Option<String>,
    pub location: Span,
}

impl Argument {
    pub fn get_variable_name(&self) -> Option<&str> {
        self.argument_name.get_variable_name()
    }

    pub fn put_doc(&mut self, new_doc: String) {
        self.doc = Some(new_doc);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgumentName {
    Discarded {
        name: String,
        label: String,
        location: Span,
    },
    Named {
        name: String,
        label: String,
        location: Span,
    },
}

impl ArgumentName {
    pub fn get_variable_name(&self) -> Option<&str> {
        match self {
            ArgumentName::Named { name, .. } => Some(name),
            ArgumentName::Discarded { .. } => None,
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            ArgumentName::Discarded { label, .. } => label.to_string(),
            ArgumentName::Named { label, .. } => label.to_string(),
        }
    }
}

pub const CAPTURE_VARIABLE: &str = "_capture";
pub const PIPE_VARIABLE: &str = "_pipe";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Boolean Logic
    And,
    Or,

    // Equality
    Eq,
    NotEq,

    // Order Comparison
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // Math
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn empty() -> Self {
        use chumsky::Span;

        Self::new((), 0..0)
    }

    pub fn create(offset: usize, length: usize) -> Self {
        use chumsky::Span;

        Self::new((), offset..offset + length)
    }

    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn union(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.min(other.end),
        }
    }

    pub fn contains(&self, index: usize) -> bool {
        index >= self.start && index < self.end
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        let start: miette::SourceOffset = span.start.into();
        let length: miette::SourceOffset = (span.end - span.start).into();

        Self::new(start, length)
    }
}

impl chumsky::Span for Span {
    type Context = ();
    type Offset = usize;

    fn new(_context: Self::Context, range: Range<Self::Offset>) -> Self {
        assert!(range.start() <= range.end());

        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
