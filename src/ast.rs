use std::ops::Range;

use crate::{expr::UntypedExpr, type_annotation::TypeAnnotation};

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

pub type ParsedCallArg = CallArg<Option<UntypedExpr>>;

#[derive(Debug, Clone)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub location: Span,
    pub value: A,
}

pub type UntypedArgument = Argument<()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Argument<T> {
    pub argument_name: ArgumentName,
    pub location: Span,
    pub annotation: Option<TypeAnnotation>,
    pub doc: Option<String>,
    pub type_: T,
}

impl<T> Argument<T> {
    pub fn set_type<B>(self, type_: B) -> Argument<B> {
        Argument {
            type_,
            argument_name: self.argument_name,
            location: self.location,
            annotation: self.annotation,
            doc: self.doc,
        }
    }

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
