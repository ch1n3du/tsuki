use crate::ast::Span;

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct ModuleExtra {
    pub module_comments: Vec<Span>,
    pub doc_comments: Vec<Span>,
    pub comments: Vec<Span>,
    pub empty_lines: Vec<usize>,
}

impl ModuleExtra {
    pub fn new() -> Self {
        Default::default()
    }
}
