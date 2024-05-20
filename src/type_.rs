use crate::{ast::Span, error::ParseError, token::Token};
use chumsky::{
    chain::Chain,
    primitive::{choice, just},
    recursive::recursive,
    select, Parser,
};

/// This type serves as the AST for type annotations
#[derive(Debug, Clone)]
pub enum Type {
    Constructor {
        location: Span,
        module: Option<String>,
        name: String,
        type_arguments: Vec<Type>,
    },
    Function {
        location: Span,
        type_of_arguments: Vec<Type>,
        return_type: Box<Type>,
    },
    Tuple {
        location: Span,
        type_of_elements: Vec<Type>,
    },
    Variable {
        location: Span,
        name: String,
    },
}

impl Type {
    pub fn get_location(&self) -> Span {
        use Type::*;

        match self {
            Function { location, .. }
            | Tuple { location, .. }
            | Variable { location, .. }
            | Constructor { location, .. } => *location,
        }
    }

    pub fn unit_type(location: Span) -> Self {
        Type::Constructor {
            module: None,
            name: "Unit".to_string(),
            type_arguments: Vec::new(),
            location,
        }
    }

    pub fn bool_type(location: Span) -> Self {
        Type::Constructor {
            name: "Bool".to_string(),
            module: None,
            type_arguments: Vec::new(),
            location,
        }
    }
    pub fn int_type(location: Span) -> Self {
        Type::Constructor {
            name: "Int".to_string(),
            module: None,
            type_arguments: Vec::new(),
            location,
        }
    }

    pub fn float_type(location: Span) -> Self {
        Type::Constructor {
            name: "Float".to_string(),
            module: None,
            type_arguments: Vec::new(),
            location,
        }
    }

    pub fn string_type(location: Span) -> Self {
        Type::Constructor {
            name: "String".to_string(),
            module: None,
            type_arguments: Vec::new(),
            location,
        }
    }

    pub fn is_bool(&self) -> bool {
        *self == Type::bool_type(Span::empty())
    }

    pub fn is_int(&self) -> bool {
        *self == Type::int_type(Span::empty())
    }

    pub fn is_float(&self) -> bool {
        *self == Type::float_type(Span::empty())
    }

    pub fn is_string(&self) -> bool {
        *self == Type::string_type(Span::empty())
    }

    pub fn is_unit(&self) -> bool {
        *self == Type::unit_type(Span::empty())
    }
}

impl std::cmp::PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (
                Constructor {
                    module: module_1,
                    name: name_1,
                    type_arguments: arg_types_1,
                    ..
                },
                Constructor {
                    module: module_2,
                    name: name_2,
                    type_arguments: arg_types_2,
                    ..
                },
            ) => module_1 == module_2 && name_1 == name_2 && arg_types_1.len() == arg_types_2.len(),
            (
                Function {
                    type_of_arguments: arg_types_1,
                    return_type: return_type_1,
                    ..
                },
                Function {
                    type_of_arguments: arg_types_2,
                    return_type: return_type_2,
                    ..
                },
            ) => arg_types_1 == arg_types_2 && return_type_1 == return_type_2,
            (
                Tuple {
                    type_of_elements: element_types_1,
                    ..
                },
                Tuple {
                    type_of_elements: element_types_2,
                    ..
                },
            ) => element_types_1 == element_types_2,
            (left, right) => {
                println!("{left:?} {right:?}");
                todo!("Implement equality for type variables");
            }
        }
    }
}

pub fn type_annotation_parser() -> impl Parser<Token, Type, Error = ParseError> {
    recursive(|annotation_parser| {
        choice((
            // Constructor Annotation Parser
            select! { Token::UpName { name } => name }
                .then(
                    annotation_parser
                        .clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
                        .or_not(),
                )
                .map_with_span(|(constructor_name, constructor_type_arguments), location| {
                    Type::Constructor {
                        location,
                        module: None,
                        name: constructor_name,
                        type_arguments: constructor_type_arguments.unwrap_or_default(),
                    }
                }),
            // Constuctor qualified with module Annotation Parser
            // Examples: list.List<a>
            select! { Token::Name { name } => name}
                .then(
                    just(Token::Dot)
                        .ignore_then(select! { Token::UpName { name } => name })
                        .then(
                            annotation_parser
                                .clone()
                                .separated_by(just(Token::Comma))
                                .allow_trailing()
                                .delimited_by(just(Token::LessThan), just(Token::GreaterThan))
                                .or_not(),
                        )
                        .or_not(),
                )
                .map_with_span(|(module_name, optional_dot), location| {
                    if let Some((constructor_name, constructor_type_arguments)) = optional_dot {
                        Type::Constructor {
                            location,
                            module: Some(module_name),
                            name: constructor_name,
                            type_arguments: constructor_type_arguments.unwrap_or_default(),
                        }
                    } else {
                        // TODO: Fix this massive hack
                        // Should throw an error.
                        Type::Variable {
                            location,
                            name: module_name,
                        }
                    }
                }),
            // Tuple Type Annotation Parser
            // Example: "(Int, String)"
            annotation_parser
                .clone()
                .separated_by(just(Token::Comma))
                .at_least(2)
                .allow_trailing()
                .delimited_by(
                    choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
                    just(Token::RightParen),
                )
                .map_with_span(|type_of_tuple_elements, location| Type::Tuple {
                    location,
                    type_of_elements: type_of_tuple_elements,
                }),
            // Function Type Annotation Parser
            // Example: "fn (Int, String) -> String"
            just(Token::Fn)
                .ignore_then(
                    annotation_parser
                        .clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                )
                .then_ignore(just(Token::RightArrow))
                .then(annotation_parser.clone())
                .map_with_span(
                    |(type_of_arguments, return_type), location| Type::Function {
                        location,
                        type_of_arguments,
                        return_type: Box::new(return_type),
                    },
                ),
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::{type_annotation_parser, Type};
    use crate::{ast::Span, error::ParseError, utils::attempt_to_parse};

    use pretty_assertions::assert_eq;

    fn try_parsing_annotation(src: &str) -> Result<Type, Vec<ParseError>> {
        attempt_to_parse(src, type_annotation_parser())
    }

    #[test]
    fn can_parse_simple_constructor_annotations() {
        let result = try_parsing_annotation("Int");
        assert_eq!(
            result.unwrap(),
            Type::Constructor {
                location: Span { start: 0, end: 3 },
                module: None,
                name: "Int".to_string(),
                type_arguments: vec![],
            }
        )
    }

    #[test]
    fn can_parse_module_qualified_simple_constructor_annotations() {
        let result = try_parsing_annotation("int.Int");
        assert_eq!(
            result.unwrap(),
            Type::Constructor {
                location: Span { start: 0, end: 7 },
                module: Some("int".to_string(),),
                name: "Int".to_string(),
                type_arguments: vec![],
            },
        );
    }

    #[test]
    fn can_parse_generic_constructor_annotations() {
        let result = try_parsing_annotation("Result<value, reason>");
        assert_eq!(
            result.unwrap(),
            Type::Constructor {
                location: Span { start: 0, end: 21 },
                module: None,
                name: "Result".to_string(),
                type_arguments: vec![
                    Type::Variable {
                        location: Span { start: 7, end: 12 },
                        name: "value".to_string(),
                    },
                    Type::Variable {
                        location: Span { start: 14, end: 20 },
                        name: "reason".to_string(),
                    },
                ],
            }
        )
    }

    #[test]
    fn can_parse_module_qualified_generic_constructor_annotations() {
        let result = try_parsing_annotation("result.Result<value, reason>");
        assert_eq!(
            result.unwrap(),
            Type::Constructor {
                location: Span { start: 0, end: 28 },
                module: Some("result".to_string(),),
                name: "Result".to_string(),
                type_arguments: vec![
                    Type::Variable {
                        location: Span { start: 14, end: 19 },
                        name: "value".to_string(),
                    },
                    Type::Variable {
                        location: Span { start: 21, end: 27 },
                        name: "reason".to_string(),
                    },
                ],
            }
        )
    }

    #[test]
    fn can_parse_simple_tuple_annotations() {
        let result = try_parsing_annotation("(Int, String)");
        assert_eq!(
            result.unwrap(),
            Type::Tuple {
                location: Span { start: 0, end: 13 },
                type_of_elements: vec![
                    Type::Constructor {
                        location: Span { start: 1, end: 4 },
                        module: None,
                        name: "Int".to_string(),
                        type_arguments: vec![],
                    },
                    Type::Constructor {
                        location: Span { start: 6, end: 12 },
                        module: None,
                        name: "String".to_string(),
                        type_arguments: vec![],
                    },
                ],
            }
        )
    }

    #[test]
    fn can_parse_tuple_annotations_with_generics_and_module_qualification() {
        let result = try_parsing_annotation("(int.Int, result.Result<value, reason>)");
        let expected = Type::Tuple {
            location: Span { start: 0, end: 39 },
            type_of_elements: vec![
                Type::Constructor {
                    location: Span { start: 1, end: 8 },
                    module: Some("int".to_string()),
                    name: "Int".to_string(),
                    type_arguments: vec![],
                },
                Type::Constructor {
                    location: Span { start: 10, end: 38 },
                    module: Some("result".to_string()),
                    name: "Result".to_string(),
                    type_arguments: vec![
                        Type::Variable {
                            location: Span { start: 24, end: 29 },
                            name: "value".to_string(),
                        },
                        Type::Variable {
                            location: Span { start: 31, end: 37 },
                            name: "reason".to_string(),
                        },
                    ],
                },
            ],
        };

        assert_eq!(result.unwrap(), expected)
    }

    #[test]
    fn can_parse_simple_function_annotations() {
        let result = try_parsing_annotation("fn (Int, String) -> String");
        assert_eq!(
            result.unwrap(),
            Type::Function {
                location: Span { start: 0, end: 26 },
                type_of_arguments: vec![
                    Type::Constructor {
                        location: Span { start: 4, end: 7 },
                        module: None,
                        name: "Int".to_string(),
                        type_arguments: vec![],
                    },
                    Type::Constructor {
                        location: Span { start: 9, end: 15 },
                        module: None,
                        name: "String".to_string(),
                        type_arguments: vec![],
                    },
                ],
                return_type: Box::new(Type::Constructor {
                    location: Span { start: 20, end: 26 },
                    module: None,
                    name: "String".to_string(),
                    type_arguments: vec![],
                }),
            }
        )
    }

    #[test]
    fn can_parse_complex_function_annotations() {
        let result =
            try_parsing_annotation("fn (int.Int, result.Result<value, reason>) -> int.Int");
        let expected = Type::Function {
            location: Span { start: 0, end: 53 },
            type_of_arguments: vec![
                Type::Constructor {
                    location: Span { start: 4, end: 11 },
                    module: Some("int".to_string()),
                    name: "Int".to_string(),
                    type_arguments: vec![],
                },
                Type::Constructor {
                    location: Span { start: 13, end: 41 },
                    module: Some("result".to_string()),
                    name: "Result".to_string(),
                    type_arguments: vec![
                        Type::Variable {
                            location: Span { start: 27, end: 32 },
                            name: "value".to_string(),
                        },
                        Type::Variable {
                            location: Span { start: 34, end: 40 },
                            name: "reason".to_string(),
                        },
                    ],
                },
            ],
            return_type: Box::new(Type::Constructor {
                location: Span { start: 46, end: 53 },
                module: Some("int".to_string()),
                name: "Int".to_string(),
                type_arguments: vec![],
            }),
        };

        assert_eq!(result.unwrap(), expected)
    }
}
