use crate::ast::{CallArg, Span};

#[allow(unused)]
pub type UntypedPattern = Pattern<(), ()>;

#[allow(unused)]
#[derive(Debug)]
pub enum Pattern<Constructor, Type> {
    Int {
        location: Span,
        value: String,
    },

    /// The creation of a variable.
    /// e.g. `expect [this_is_a_var, .._] = x`
    /// e.g. `let foo = 42`
    Var {
        location: Span,
        name: String,
    },

    /// A name given to a sub-pattern using the `as` keyword.
    ///
    /// ```aiken
    /// when foo is {
    ///    [_, _] as the_list -> ...
    /// }
    /// ```
    Assign {
        name: String,
        location: Span,
        pattern: Box<Self>,
    },

    /// A pattern that binds to any value but does not assign a variable.
    /// Always starts with an underscore.
    Discard {
        name: String,
        location: Span,
    },

    List {
        location: Span,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    /// The constructor for a custom type. Starts with an uppercase letter.
    Constructor {
        is_record: bool,
        location: Span,
        name: String,
        arguments: Vec<CallArg<Self>>,
        module: Option<String>,
        constructor: Constructor,
        with_spread: bool,
        tipo: Type,
    },

    Tuple {
        location: Span,
        elems: Vec<Self>,
    },
}
