#[derive(Debug, PartialEq)]
pub struct Location {
    start: usize,
    end: usize,
    column: usize,
    line_no: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    /// A lowercase name used for variables and modules.
    ///
    /// Examples: `ordinal`, `counter`, etc.
    Name {
        name: String,
    },
    /// An uppercase name used for naming types.
    ///
    /// Examples: `Int`, `String`, `List(U8)`, etc.
    UpName {
        name: String,
    },
    /// A name that starts with an underscore, used to signify values
    /// that can be discarded.
    ///
    /// Examples: `_result`, `_`, etc.
    DiscardName {
        name: String,
    },
    // Literals
    Integer {
        value: String,
    },
    Float {
        value: String,
    },
    String {
        value: String,
    },
    Boolean {
        value: bool,
    },
    Ordinal {
        index: u32,
    },

    // Integer/Float Operands
    Plus,         // +
    NewLineMinus, // '↳-'
    Minus,        // -
    Multiply,     // *
    Divide,       // /
    Modulo,       // %

    // Comparison
    EqualEqual,         // ==
    BangEqual,          // !=
    Equal,              // =
    GreaterThan,        // >
    GreaterThanOrEqual, // >=
    LessThan,           // <
    LessThanOrEqual,    // <=

    // Other Punctuation
    RightArrow, // ->
    Not,        // not
    Or,         // or
    And,        // and
    Colon,      // :
    Comma,      // ,
    Dot,        // .

    // Groupings
    NewLineLeftParen, // '↳('
    LeftParen,        // (
    RightParen,       // )
    LeftSquare,       // [
    RightSquare,      // ]
    LeftCurly,        // {
    RightCurly,       // }
    NewLinePipe,      // '↳|>'
    Pipe,             // |>
    Question,         // ?
    Hash,             // #

    // Keywords
    Fn,
    Test,
    Todo,
    Type,
    If,
    Else,
    Match,
    With,
    Let,
    Const,
    Pub,
    Use,
    Module,

    // Extra
    Comment,
    DocComment,
    ModuleComment,
    WhiteSpace,
    EmptyLine,
    NewLine,

    Error(char),

    EndOfFile,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftSquare => "[",
            Token::RightSquare => "]",
            _ => todo!(),
        };
        write!(f, "\"{s}\"")
    }
}
