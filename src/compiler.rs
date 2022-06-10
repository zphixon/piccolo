//! Modules for compiling Piccolo source code.

pub mod ast;
pub mod emitter;
pub mod parser;
pub mod scanner;

use crate::{ErrorKind, Module, PiccoloError};

use core::fmt;

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

pub fn compile_chunk(src: &str) -> Result<Module, Vec<PiccoloError>> {
    let mut scanner = super::Scanner::new(src);
    let ast = parser::parse(&mut scanner)?;
    emitter::compile(&ast)
}

pub fn scan_all(source: &str) -> Result<Vec<Token>, PiccoloError> {
    scanner::Scanner::new(source).scan_all()
}

pub(crate) fn escape_string(t: Token) -> Result<String, PiccoloError> {
    match t.kind {
        TokenKind::String => {
            let s = t.lexeme;
            let mut value = Vec::new();
            let line_start = t.line;
            let mut line = line_start;

            let mut i = 1;
            while i < s.as_bytes().len() - 1 {
                let byte = s.as_bytes()[i];
                if byte == b'\n' {
                    line += 1;
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
                            .line(line));
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

/// Represents a token in source code.
///
/// Maintains a reference to the original source.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'a str,
    pub(crate) line: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, line: usize) -> Self {
        Token { kind, lexeme, line }
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

    pub fn assign_by_mutate_op(&self) -> Option<crate::Opcode> {
        use crate::Opcode;
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
            if token.line != previous_line {
                previous_line = token.line;
                format!("{:>4}", token.line)
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
