use crate::{ast::Span, token::Token};

use miette::Diagnostic;
use owo_colors::{OwoColorize, Stream::Stdout};
use std::collections::HashSet;

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

    pub fn invalid_assignment_right_hand_side(span: Span) -> Self {
        Self {
            kind: ErrorKind::UnfinishedAssignmentRightHandSide,
            span,
            while_parsing: None,
            expected_patterns: HashSet::new(),
            label: Some("invalid assignment right-hand side"),
        }
    }

    pub fn invalid_tuple_index(span: Span, index: String, suffix: Option<String>) -> Self {
        let hint = suffix.map(|suffix| format!("Did you mean '{index}{suffix}'?"));
        Self {
            kind: ErrorKind::InvalidTupleIndex { hint },
            span,
            while_parsing: None,
            expected_patterns: HashSet::new(),
            label: None,
        }
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

    #[error("I spotted an unfinished assignment.")]
    #[diagnostic(
        help(
            "{} and {} bindings must be followed by a valid, complete, expression.",
            "let".if_supports_color(Stdout, |s| s.yellow()),
            "expect".if_supports_color(Stdout, |s| s.yellow()),
        ),
    )]
    UnfinishedAssignmentRightHandSide,

    #[error("I discovered an invalid tuple index.")]
    #[diagnostic()]
    InvalidTupleIndex {
        #[help]
        hint: Option<String>,
    },
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

#[derive(Debug)]
pub enum Error {
    DuplicateArgument {
        label: String,
        location: Span,
        duplicate_location: Span,
    },
    DuplicateField {
        label: String,
        location: Span,
        duplicate_location: Span,
    },
}

/// Helper alias for Tsuki's Result type.
pub type TResult<T> = Result<T, Error>;
