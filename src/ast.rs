use std::ops::Range;

use crate::{
    expr::{TypedExpr, UntypedExpr},
    type_::Type,
};

pub type TypedModule = Module<(), TypedDefinition>;
pub type UntypedModule = Module<(), UntypedDefinition>;

// TODO: pub struct Module<TypeInfo, Definitions> {
#[derive(Debug)]
pub struct Module<TypeInfo, Definitions> {
    pub name: String,
    pub docs: Vec<String>,
    // TODO: Implement collecting module type info
    pub type_info: TypeInfo,
    pub definitions: Vec<Definitions>,
    // TODO: pub lines: LineNumbers
}

// TODO: pub struct TypeInfo {}

pub type TypedDefinition = Definition<TypedExpr>;
pub type UntypedDefinition = Definition<UntypedExpr>;

#[derive(Debug)]
pub enum Definition<Expr> {
    Fn(Function<Expr>),
}

impl<Expr> Definition<Expr> {
    pub fn get_name(&self) -> String {
        match self {
            Definition::Fn(func) => func.name.clone(),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Definition::Fn(func) => func.get_type(),
        }
    }
}

pub type TypedFunction = Function<TypedExpr>;
pub type UntypedFunction = Function<UntypedExpr>;

#[derive(Debug)]
pub struct Function<Expr> {
    pub is_public: bool,
    pub arguments: Vec<Argument>,
    pub arguments_span: Span,
    pub name: String,
    pub body: Expr,
    pub return_type: Type,
    pub doc: Option<String>,
    // Span of the function type definition
    pub type_definition_span: Span,
    // Location of actual end of the function
    pub end_position: usize,
}

impl<Expr> Function<Expr> {
    pub fn get_type(&self) -> Type {
        Type::Function {
            location: self.type_definition_span,
            type_of_arguments: self.argument_types(),
            return_type: Box::new(self.return_type.clone()),
        }
    }

    pub fn argument_types(&self) -> Vec<Type> {
        self.arguments
            .iter()
            .map(|argument| argument.get_type())
            .to_owned()
            .collect()
    }
}

pub type UntypedCallArg = CallArg<UntypedExpr>;

#[derive(Debug, Clone)]
pub struct CallArg<Expr> {
    pub label: Option<String>,
    pub location: Span,
    pub value: Expr,
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

    pub fn get_type(&self) -> Type {
        self.annotation.clone()
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
