pub mod compiler;
pub mod parser_compiler;
pub mod scanner;

use crate::{Chunk, ErrorKind, PiccoloError};
use parser_compiler::ParserCompiler;

use core::fmt;

/// Compile some Piccolo source code into a chunk.
pub fn compile(chunk: Chunk, tokens: &[Token]) -> Result<Chunk, Vec<PiccoloError>> {
    let mut emitter = ParserCompiler::new(chunk, tokens);

    let mut errors = vec![];

    while !emitter.matches(TokenKind::Eof).map_err(|e| vec![e])? {
        if let Err(err) = emitter.declaration() {
            errors.push(err);
            emitter.stop_output();
            break;
        }
    }

    if errors.is_empty() {
        Ok(emitter.chunk())
    } else {
        Err(errors)
    }
}

pub fn compile2(src: &str) -> Result<Chunk, Vec<PiccoloError>> {
    let mut scanner = super::Scanner::new(src);
    let mut parser = crate::ast::parser::Parser::new();
    println!("****** parser");
    let ast = parser.parse(&mut scanner)?;
    println!("****** compiler");
    let mut emitter2 = compiler::Compiler(Chunk::default());
    emitter2.compile(&ast)
}

/// Scans all tokens at once from source.
pub fn scan_all(source: &str) -> Result<Vec<Token>, PiccoloError> {
    scanner::Scanner::new(source).scan_all()
}

#[allow(dead_code)]
pub(crate) fn escape_string(t: &Token) -> Result<String, PiccoloError> {
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
                        b'\n' => {
                            while i < s.as_bytes().len() - 1 && is_whitespace(s.as_bytes()[i]) {
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

            Ok(String::from_utf8(value)
                .map_err(|_| PiccoloError::new(ErrorKind::InvalidUTF8).line(line))?)
        }
        _ => {
            panic!("Cannot escape string from token {:?}", t);
        }
    }
}

fn is_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_whitespace(c: u8) -> bool {
    c == 0x09        // tab
        || c == 0x0A // line feed
        || c == 0x0B // line tab
        || c == 0x0C // form feed
        || c == 0x0D // carriage return
        || c == 0x20 // space
                     //  || c == 0x85 // next line      !! represented in utf-8 as C2 85
                     //  || c == 0xA0 // no-break space !! represented in utf-8 as C2 A0
}

fn is_non_identifier(c: u8) -> bool {
    is_whitespace(c)
        || c == 0x00
        || c == b'#'
        || c == b'['
        || c == b']'
        || c == b'('
        || c == b')'
        || c == b','
        || c == b'-'
        || c == b'+'
        || c == b'*'
        || c == b'/'
        || c == b'^'
        || c == b'%'
        || c == b'&'
        || c == b'|'
        || c == b'.'
        || c == b'!'
        || c == b':'
        || c == b'='
        || c == b'>'
        || c == b'<'
        || c == b'"'
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // keywords
    Do,     // do
    End,    // end
    Fn,     // fn
    If,     // if
    Else,   // else
    While,  // while
    For,    // for
    In,     // in
    Data,   // data
    Let,    // let
    Is,     // is
    Me,     // me
    New,    // new
    Err,    // err
    Retn,   // retn
    Assert, // nil
    Nil,    // nil

    // syntax
    LeftBracket,  // [
    RightBracket, // ]
    LeftParen,    // (
    RightParen,   // )
    // braces?
    Comma,          // ,
    Period,         // .
    ExclusiveRange, // ..
    InclusiveRange, // ...
    Assign,         // =
    Declare,        // =:

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

    // other syntax elements
    Identifier,
    String,
    True,
    False,
    Double(f64),
    Integer(i64),

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'a str,
    pub(crate) line: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, line: usize) -> Self {
        Token { kind, lexeme, line }
    }

    pub fn is_value(&self) -> bool {
        match self.kind {
            TokenKind::Nil => true,
            TokenKind::String => true,
            TokenKind::True => true,
            TokenKind::False => true,
            TokenKind::Double(_) => true,
            TokenKind::Integer(_) => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            TokenKind::Identifier => write!(f, "{}", self.lexeme),
            TokenKind::String => write!(f, "string \"{}\"", self.lexeme),
            TokenKind::Double(d) => write!(f, "double {}", d),
            TokenKind::Integer(i) => write!(f, "integer {}", i),
            k => write!(f, "{:?}", k),
        }
    }
}

#[cfg(feature = "pc-debug")]
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

fn into_keyword(s: &str) -> Option<TokenKind> {
    match s {
        "do" => Some(TokenKind::Do),
        "end" => Some(TokenKind::End),
        "fn" => Some(TokenKind::Fn),
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "while" => Some(TokenKind::While),
        "for" => Some(TokenKind::For),
        "in" => Some(TokenKind::In),
        "data" => Some(TokenKind::Data),
        "let" => Some(TokenKind::Let),
        "is" => Some(TokenKind::Is),
        "me" => Some(TokenKind::Me),
        "new" => Some(TokenKind::New),
        "err" => Some(TokenKind::Err),
        "retn" => Some(TokenKind::Retn),
        "assert" => Some(TokenKind::Assert),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "nil" => Some(TokenKind::Nil),
        _ => None,
    }
}

#[cfg(feature = "fuzzer")]
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Do => write!(f, "do"),
            TokenKind::End => write!(f, "end"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::Data => write!(f, "data"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Is => write!(f, "is"),
            TokenKind::Me => write!(f, "me"),
            TokenKind::New => write!(f, "new"),
            TokenKind::Err => write!(f, "err"),
            TokenKind::Retn => write!(f, "retn"),
            TokenKind::Assert => write!(f, "assert"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Period => write!(f, "."),
            TokenKind::ExclusiveRange => write!(f, ".."),
            TokenKind::InclusiveRange => write!(f, "..."),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Declare => write!(f, "=:"),
            TokenKind::Not => write!(f, "!"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Multiply => write!(f, "*"),
            TokenKind::Divide => write!(f, "/"),
            TokenKind::Modulo => write!(f, "%"),
            TokenKind::LogicalAnd => write!(f, "&&"),
            TokenKind::LogicalOr => write!(f, "||"),
            TokenKind::BitwiseAnd => write!(f, "&"),
            TokenKind::BitwiseOr => write!(f, "|"),
            TokenKind::BitwiseXor => write!(f, "^"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::ShiftLeft => write!(f, "<<"),
            TokenKind::ShiftRight => write!(f, ">>"),
            TokenKind::Identifier => write!(f, "ident"),
            TokenKind::String => write!(f, "\"str\""),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Double(f64) => write!(f, "{}", f64),
            TokenKind::Integer(i64) => write!(f, "{}", i64),
            TokenKind::Eof => write!(f, ""),
        }
    }
}
