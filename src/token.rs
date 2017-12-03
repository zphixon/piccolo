
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // keywords
    Do,       // do
    End,      // end
    Fn,       // fn
    If,       // if
    Else,     // else
    While,    // while
    For,      // for
    In,       // in
    Data,     // data
    Is,       // is
    Pub,      // pub
    Me,       // me
    New,      // new
    Err,      // err
    Retn,     // retn

    // syntax
    LBracket, // [
    RBracket, // ]
    LParen,   // (
    RParen,   // )
    Comma,    // ,
    Dot,      // .
    ERange,   // ..
    IRange,   // ...
    Assign,   // =
    Newline,  // \n

    // operators
    Bang,                // !
    Plus,                // +
    Hyphen,              // -
    Star,                // *
    FSlash,              // /
    Mod,                 // %
    And,                 // &&
    Or,                  // ||
    BAnd,                // &
    BOr,                 // |
    BXor,                // ^
    Equals,              // ==
    BangEquals,          // !=
    LessThan,            // <
    GreaterThan,         // >
    LessThanEquals,      // <=
    GreaterThanEquals,   // >=

    // other syntax elements
    Ident,
    String,
    Double(f64),
    Integer(i64),

    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: u64,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: u64) -> Self {
        Token {
            kind,
            lexeme,
            line,
        }
    }
}

