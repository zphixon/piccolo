//! Modules for compiling Piccolo source code.

pub mod ast;
pub mod emitter;
pub mod parser;
pub mod scanner;
pub mod typeck;

use crate::{
    compiler::scanner::Scanner,
    error::{ErrorKind, PiccoloError},
    runtime::{chunk, op::Opcode},
};
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    hash::{Hash, Hasher},
};

#[derive(PartialEq, Eq, Hash, Default, Debug)]
pub(crate) struct Local {
    pub(crate) name: String,
    pub(crate) depth: u16,
    slot: u16,
    is_upvalue: bool,
}

impl Local {
    pub(crate) fn new(name: String, depth: u16) -> Self {
        Self {
            name,
            depth,
            ..Self::default()
        }
    }
}

pub fn compile_chunk(src: &str) -> Result<chunk::Module, Vec<PiccoloError>> {
    let ast = parser::parse(src)?;
    emitter::compile(&ast)
}

pub fn scan_all(source: &str) -> Result<Vec<Token>, PiccoloError> {
    Scanner::new(source).scan_all()
}

pub(crate) fn escape_string(t: Token) -> Result<String, PiccoloError> {
    match t.kind {
        TokenKind::String => {
            let s = t.lexeme;
            let mut value = Vec::new();
            let line_start = t.pos;
            let mut pos = line_start;

            let mut i = 1;
            while i < s.as_bytes().len() - 1 {
                let byte = s.as_bytes()[i];
                if byte == b'\n' {
                    pos.line += 1;
                }

                if byte == b'\\' {
                    i += 1;
                    let byte = s.as_bytes()[i];
                    match byte {
                        b'n' => {
                            value.push(b'\n');
                        }
                        b'r' => {
                            value.push(b'\r');
                        }
                        b'\\' => {
                            value.push(b'\\');
                        }
                        b'"' => {
                            value.push(b'"');
                        }
                        b't' => {
                            value.push(b'\t');
                        }
                        b'\r' | b'\n' => {
                            while i < s.as_bytes().len() - 1
                                && scanner::is_whitespace(s.as_bytes()[i])
                            {
                                i += 1;
                            }
                            i -= 1;
                        }
                        c => {
                            return Err(PiccoloError::new(ErrorKind::UnknownFormatCode {
                                code: c as char,
                            })
                            .pos(pos));
                        }
                    }
                } else {
                    value.push(byte);
                }

                i += 1;
            }

            Ok(String::from_utf8(value)?)
        }
        _ => {
            panic!("Cannot escape string from token {:?}", t);
        }
    }
}

/// Kinds of tokens that may exist in Piccolo code.
///
/// Some of these don't currently have a use, and only exist for the creation of
/// syntax errors :^)
#[cfg_attr(fuzzing, derive(arbitrary::Arbitrary))]
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
    Let,      // let
    Me,       // me
    New,      // new
    Err,      // err
    Break,    // break
    Continue, // continue
    Retn,     // retn
    Assert,   // assert
    Nil,      // nil

    // syntax
    LeftBracket,    // [
    RightBracket,   // ]
    LeftParen,      // (
    RightParen,     // )
    Comma,          // ,
    Period,         // .
    ExclusiveRange, // ..
    InclusiveRange, // ...
    Assign,         // =
    Declare,        // =:

    // misc. non-tokens
    LeftBrace,
    RightBrace,
    Dollar,
    At,
    Grave,
    Tilde,
    Colon,
    Semicolon,
    Backslash,
    Question,
    SingleQuote,

    // operators
    Not,          // !
    Plus,         // +
    Minus,        // -
    Multiply,     // *
    Divide,       // /
    Modulo,       // %
    LogicalAnd,   // &&
    LogicalOr,    // ||
    BitwiseAnd,   // &
    BitwiseOr,    // |
    BitwiseXor,   // ^
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    ShiftLeft,    // <<
    ShiftRight,   // >>

    PlusAssign,       // +=
    MinusAssign,      // -=
    DivideAssign,     // /=
    MultiplyAssign,   // *=
    ModuloAssign,     // %=
    BitwiseAndAssign, // &=
    BitwiseOrAssign,  // |=
    BitwiseXorAssign, // ^=
    ShiftLeftAssign,  // <<=
    ShiftRightAssign, // >>=

    // other syntax elements
    Identifier,
    String,
    True,
    False,
    Double(f64),
    Integer(i64),

    Eof,
}

#[cfg_attr(fuzzing, derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct SourcePos {
    pub line: usize,
    pub col: usize,
}

impl SourcePos {
    pub(crate) fn empty() -> Self {
        SourcePos { line: 0, col: 0 }
    }

    pub(crate) fn one() -> Self {
        SourcePos { line: 1, col: 1 }
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Represents a token in source code.
///
/// Maintains a reference to the original source.
#[cfg_attr(fuzzing, derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'a str,
    pub(crate) pos: SourcePos,
}

impl Hash for Token<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
    }
}

impl Eq for Token<'_> {}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl<'a> Token<'a> {
    pub(crate) fn identifier(lexeme: &'a str) -> Self {
        Token::new(TokenKind::Identifier, lexeme, SourcePos::empty())
    }

    pub fn new(kind: TokenKind, lexeme: &'a str, pos: SourcePos) -> Self {
        Token { kind, lexeme, pos }
    }

    /// Whether or not the token is a value literal.
    pub fn is_value(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Nil
                | TokenKind::String
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Double(_)
                | TokenKind::Integer(_)
        )
    }

    pub fn is_assign(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Assign
                | TokenKind::PlusAssign
                | TokenKind::MinusAssign
                | TokenKind::DivideAssign
                | TokenKind::MultiplyAssign
                | TokenKind::ModuloAssign
                | TokenKind::BitwiseAndAssign
                | TokenKind::BitwiseOrAssign
                | TokenKind::BitwiseXorAssign
                | TokenKind::ShiftLeftAssign
                | TokenKind::ShiftRightAssign
        )
    }

    pub fn assign_by_mutate_op(&self) -> Option<Opcode> {
        Some(match self.kind {
            TokenKind::PlusAssign => Opcode::Add,
            TokenKind::MinusAssign => Opcode::Subtract,
            TokenKind::DivideAssign => Opcode::Divide,
            TokenKind::MultiplyAssign => Opcode::Multiply,
            TokenKind::ModuloAssign => Opcode::Modulo,
            TokenKind::BitwiseAndAssign => Opcode::BitAnd,
            TokenKind::BitwiseOrAssign => Opcode::BitOr,
            TokenKind::BitwiseXorAssign => Opcode::BitXor,
            TokenKind::ShiftLeftAssign => Opcode::ShiftLeft,
            TokenKind::ShiftRightAssign => Opcode::ShiftRight,
            _ => None?,
        })
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            TokenKind::Identifier => write!(f, "{}", self.lexeme),
            TokenKind::String => write!(f, "{}", self.lexeme),
            TokenKind::Double(v) => write!(f, "{}", v),
            TokenKind::Integer(v) => write!(f, "{}", v),
            v => write!(f, "{:?}", v),
        }
    }
}

pub fn print_tokens(tokens: &[Token]) {
    let mut previous_line = 0;
    for token in tokens.iter() {
        println!(
            "{} {:?}{}",
            if token.pos.line != previous_line {
                previous_line = token.pos.line;
                format!("{:>4}", token.pos.line)
            } else {
                "   |".into()
            },
            token.kind,
            if token.kind == TokenKind::Identifier {
                format!(" {}", token.lexeme)
            } else {
                "".into()
            }
        );
    }
}
