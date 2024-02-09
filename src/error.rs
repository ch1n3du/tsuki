use std::collections::HashSet;

use miette::Diagnostic;

use crate::ast::Span;

use super::token::Token;

#[derive(Debug, Clone, Diagnostic, thiserror::Error)]
#[error("{kind}\n")]
pub struct ParseError {
    pub kind: ErrorKind,
    #[label]
    pub span: Span,
    while_parsing: Option<(Span, &'static str)>,
    expected_patterns: HashSet<Pattern>,
    label: Option<&'static str>,
}

impl ParseError {
    fn merge(mut self, other: Self) -> Self {
        for pattern in other.expected_patterns.into_iter() {
            self.expected_patterns.insert(pattern);
        }
        self
    }
}

impl<T: Into<Pattern>> chumsky::Error<T> for ParseError {
    type Span = Span;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Self::Span,
        input_expected: Iter,
        input_found: Option<T>,
    ) -> Self {
        ParseError {
            kind: input_found
                .map(Into::into)
                .map(ErrorKind::Unexpected)
                .unwrap_or(ErrorKind::UnexpectedEnd),
            span,
            while_parsing: None,
            expected_patterns: input_expected
                .into_iter()
                .map(|x| x.map(Into::into).unwrap_or(Pattern::End))
                .collect(),
            label: None,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(self, other: Self) -> Self {
        ParseError::merge(self, other)
    }
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.span == other.span && self.label == other.label
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Diagnostic, thiserror::Error)]
pub enum ErrorKind {
    #[error("I arrived at the end of the file unexpectedly.")]
    UnexpectedEnd,

    #[error("{0}")]
    #[diagnostic(help("{}", .0.help().unwrap_or_else(|| Box::new(""))))]
    Unexpected(Pattern),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Diagnostic, thiserror::Error)]
pub enum Pattern {
    #[error("I found an unexpected char '{0:?}'.")]
    #[diagnostic(help("Try removing it!"))]
    Char(char),
    #[error("I found an unexpected token '{0}'.")]
    #[diagnostic(help("Try removing it!"))]
    Token(Token),
    #[error("I found an unexpected end of input.")]
    End,
}

impl From<char> for Pattern {
    fn from(chary: char) -> Self {
        Self::Char(chary)
    }
}

impl From<Token> for Pattern {
    fn from(token: Token) -> Self {
        Self::Token(token)
    }
}
