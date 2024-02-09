#[derive(Debug, PartialEq)]
pub struct Location {
    start: usize,
    end: usize,
    column: usize,
    line_no: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // Literals
    Identifier {
        name: String,
    },
    Integer {
        value: String,
        has_underscores: bool,
    },
    Float {
        value: String,
        has_underscores: bool,
    },
    String {
        value: String,
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
    Arrow, // ->
    Not,   // not
    Or,    // or
    And,   // and
    Colon, // :
    Comma, // ,
    Dot,   // .

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
