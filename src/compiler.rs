//! Modules for compiling Piccolo source code.

pub mod ast;
pub mod emitter;
pub mod ns;
pub mod parser;
pub mod scanner;
pub mod typeck;

use crate::{
    compiler::scanner::Scanner,
    error::{ErrorKind, PiccoloError},
    runtime::{chunk, op::Opcode},
};
use std::{
    fmt,
    hash::{Hash, Hasher},
};

#[cfg(fuzzing)]
pub const MAX_DEPTH: usize = 32;

#[cfg(not(fuzzing))]
pub const MAX_DEPTH: usize = 120;

#[derive(Debug, Clone)]
pub(crate) enum Variable {
    Local { name: String, depth: u16, slot: u16 },
    Capture { name: String },
    Global { name: String, index: u16 },
}

impl Variable {
    pub fn name(&self) -> &str {
        match self {
            Variable::Local { name, .. } => name,
            Variable::Capture { name, .. } => name,
            Variable::Global { name, .. } => name,
        }
    }
}

pub fn compile_chunk(src: &str) -> Result<chunk::Module, Vec<PiccoloError>> {
    let ast = parser::parse(src)?;
    let mut interner = crate::runtime::interner::Interner::new();
    emitter::compile(&mut interner, &ast)
}

pub fn scan_all(source: &str) -> Result<Vec<Token>, PiccoloError> {
    Scanner::new(source).scan_all()
}

pub fn escape_string(s: &str) -> Result<String, PiccoloError> {
    let quoted = &s[1..s.len() - 1];
    let bytes = quoted.as_bytes();

    let mut i = 0;
    let mut value = Vec::with_capacity(bytes.len());
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            i += 1;

            let code = bytes[i];
            i += 1;
            match code {
                c @ (b'\\' | b'"' | b'\'') => value.push(c),

                b'n' => value.push(b'\n'),
                b'r' => value.push(b'\r'),
                b't' => value.push(b'\t'),

                b'\r' | b'\n' => {
                    while i < bytes.len() && scanner::is_whitespace(bytes[i]) {
                        i += 1;
                    }
                }

                b'u' => {
                    if bytes[i] != b'{' {
                        return Err(PiccoloError::new(ErrorKind::SyntaxError)
                            .msg("Invalid \\u format, must be \\u{hex code}"));
                    }

                    i += 1;
                    let start = i;

                    while i < bytes.len() && bytes[i] != b'}' {
                        i += 1;
                    }

                    if i == bytes.len() {
                        return Err(PiccoloError::new(ErrorKind::SyntaxError)
                            .msg("Invalid \\u format, must be \\u{hex code}"));
                    }

                    let hex = hex::decode(&quoted[start..i])?;
                    value.extend_from_slice(&hex);

                    i += 1;
                }

                b => {
                    return Err(PiccoloError::new(ErrorKind::UnknownFormatCode {
                        code: b as char,
                    }))
                }
            }
        } else {
            value.push(bytes[i]);
            i += 1;
        }
    }

    Ok(String::from_utf8(value)?)
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
    Return,   // return
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

#[derive(Default, Debug, Clone, Copy)]
pub enum Pos {
    Source {
        line: usize,
        col: usize,
    },
    #[default]
    Builtin,
}

impl Pos {
    pub(crate) fn line(&self) -> usize {
        match self {
            Pos::Source { line, .. } => *line,
            _ => panic!("called line on builtin pos"),
        }
    }

    pub(crate) fn inc_line(&mut self) {
        match self {
            Pos::Source { line, .. } => *line += 1,
            _ => panic!("called inc_line on builtin pos"),
        }
    }

    pub(crate) fn inc_col(&mut self) {
        match self {
            Pos::Source { col, .. } => *col += 1,
            _ => panic!("called inc_col on builtin pos"),
        }
    }

    pub(crate) fn reset_col(&mut self) {
        match self {
            Pos::Source { col, .. } => *col = 1,
            _ => panic!("called inc_col on builtin pos"),
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pos::Source { line, col } => write!(f, "{line}:{col}"),
            Pos::Builtin => write!(f, "builtin"),
        }
    }
}

/// Represents a token in source code.
///
/// Maintains a reference to the original source.
#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'a str,
    pub(crate) pos: Pos,
}

impl Default for Token<'_> {
    fn default() -> Self {
        Token {
            kind: TokenKind::Identifier,
            lexeme: "anon",
            pos: Pos::Builtin,
        }
    }
}

impl Hash for Token<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lexeme.hash(state);
    }
}

impl Eq for Token<'_> {}

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self.kind, other.kind) {
            (TokenKind::Identifier, TokenKind::Identifier) => self.lexeme == other.lexeme,
            (TokenKind::String, TokenKind::String) => self.lexeme == other.lexeme,
            _ => self.kind == other.kind,
        }
    }
}

impl<'a> Token<'a> {
    pub(crate) fn identifier(lexeme: &'a str) -> Self {
        Token::new(TokenKind::Identifier, lexeme, Pos::Builtin)
    }

    #[cfg(test)]
    pub(crate) fn test(kind: TokenKind) -> Self {
        Token::new(kind, "", Pos::Builtin)
    }

    pub fn new(kind: TokenKind, lexeme: &'a str, pos: Pos) -> Self {
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
            if token.pos.line() != previous_line {
                previous_line = token.pos.line();
                format!("{:>4}", token.pos.line())
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
