use crate::error::PiccoloError;
use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // keywords
    Do,    // do
    End,   // end
    Fn,    // fn
    If,    // if
    Else,  // else
    While, // while
    For,   // for
    In,    // in
    Data,  // data
    Is,    // is
    Me,    // me
    New,   // new
    Err,   // err
    Retn,  // retn
    Nil,   // nil

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
    Newline,        // \n

    // operators
    Not,               // !
    Plus,              // +
    Minus,             // -
    Multiply,          // *
    Divide,            // /
    Modulo,            // %
    And,               // &&
    Or,                // ||
    BitwiseAnd,        // &
    BitwiseOr,         // |
    BitwiseXor,        // ^
    Equals,            // ==
    NotEquals,         // !=
    LessThan,          // <
    GreaterThan,       // >
    LessThanEquals,    // <=
    GreaterThanEquals, // >=
    ShiftLeft,         // <<
    ShiftRight,        // >>

    // other syntax elements
    Identifier,
    String(String),
    True,
    False,
    Double(f64),
    Integer(i64),

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
    line: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, line: usize) -> Self {
        Token { kind, lexeme, line }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn lexeme(&self) -> &str {
        self.lexeme
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            TokenKind::Identifier => write!(f, "{}", self.lexeme),
            TokenKind::String(s) => write!(f, "string \"{}\"", s),
            TokenKind::Double(d) => write!(f, "double {}", d),
            TokenKind::Integer(i) => write!(f, "integer {}", i),
            k => write!(f, "{:?}", k),
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
        "is" => Some(TokenKind::Is),
        "me" => Some(TokenKind::Me),
        "new" => Some(TokenKind::New),
        "err" => Some(TokenKind::Err),
        "retn" => Some(TokenKind::Retn),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "nil" => Some(TokenKind::Nil),
        _ => None,
    }
}

pub struct Scanner<'a> {
    source: &'a [u8],
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source.as_bytes(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> crate::Result<Vec<Token<'a>>> {
        let mut errors = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            if let Err(e) = self.scan_token() {
                errors.push(e);
            }
        }

        self.tokens.push(Token::new(TokenKind::Eof, "", self.line));

        if !errors.is_empty() {
            if errors.len() > 1 {
                let mut err_string = String::new();
                for error in errors.iter() {
                    err_string.push_str(&format!("\n{}", error));
                }
                Err(PiccoloError::Lots {
                    num: errors.len(),
                    err: err_string,
                }
                .into())
            } else {
                Err(PiccoloError::One {
                    err: format!("{}", errors[0]),
                }
                .into())
            }
        } else {
            Ok(self.tokens)
        }
    }

    fn scan_token(&mut self) -> crate::Result<()> {
        match self.advance() {
            b'#' => {
                while self.peek() != b'\n' && !self.is_at_end() {
                    self.advance();
                }
            }

            b'\n' => {
                self.add_token(TokenKind::Newline);
                self.line += 1;
            }

            b' ' | b'\t' | b'\r' => {}

            b'[' => self.add_token(TokenKind::LeftBracket),
            b']' => self.add_token(TokenKind::RightBracket),
            b'(' => self.add_token(TokenKind::LeftParen),
            b')' => self.add_token(TokenKind::RightParen),
            b',' => self.add_token(TokenKind::Comma),
            b'-' => self.add_token(TokenKind::Minus),
            b'+' => self.add_token(TokenKind::Plus),
            b'*' => self.add_token(TokenKind::Multiply),
            b'/' => self.add_token(TokenKind::Divide),
            b'^' => self.add_token(TokenKind::BitwiseXor),
            b'%' => self.add_token(TokenKind::Modulo),

            b'&' => {
                if self.peek() == b'&' {
                    self.advance();
                    self.add_token(TokenKind::And);
                } else {
                    self.add_token(TokenKind::BitwiseAnd);
                }
            }

            b'|' => {
                if self.peek() == b'|' {
                    self.advance();
                    self.add_token(TokenKind::Or);
                } else {
                    self.add_token(TokenKind::BitwiseOr);
                }
            }

            b'.' => {
                if self.peek() == b'.' {
                    self.advance();
                    if self.peek() == b'.' {
                        self.advance();
                        self.add_token(TokenKind::InclusiveRange);
                    } else {
                        self.add_token(TokenKind::ExclusiveRange);
                    }
                } else {
                    self.add_token(TokenKind::Period);
                }
            }

            b'!' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::NotEquals);
                } else {
                    self.add_token(TokenKind::Not);
                }
            }

            b'=' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::Equals);
                } else {
                    self.add_token(TokenKind::Assign);
                }
            }

            b'>' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::GreaterThanEquals);
                } else if self.peek() == b'>' {
                    self.advance();
                    self.add_token(TokenKind::ShiftRight);
                } else {
                    self.add_token(TokenKind::GreaterThan);
                }
            }

            b'<' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::LessThanEquals);
                } else if self.peek() == b'<' {
                    self.advance();
                    self.add_token(TokenKind::ShiftLeft);
                } else {
                    self.add_token(TokenKind::LessThan);
                }
            }

            b'"' => {
                self.string()?;
            }

            c => {
                if is_digit(c) {
                    self.number()?;
                } else {
                    self.identifier_or_keyword()?;
                }
            }
        }
        Ok(())
    }

    fn identifier_or_keyword(&mut self) -> crate::Result<()> {
        while !is_non_identifier(self.peek()) {
            self.advance();
        }

        let value = String::from_utf8(self.source[self.start..self.current].to_vec())
            .map_err(|_| PiccoloError::InvalidUTF8 { line: self.line })?;

        if let Some(tk) = into_keyword(&value) {
            self.add_token(tk);
        } else {
            self.add_token(TokenKind::Identifier);
        }

        Ok(())
    }

    fn string(&mut self) -> crate::Result<()> {
        let mut value = Vec::new();
        let line_start = self.line;
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1;
            }

            if self.peek() == b'\\' {
                self.advance();
                if self.is_at_end() {
                    return Err(PiccoloError::UnterminatedString { line: line_start }.into());
                }
                match self.advance() {
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
                        self.advance();
                        while self.peek() == b' ' || self.peek() == b'\t' {
                            self.advance();
                        }
                        self.reverse();
                    }
                    c => {
                        return Err(PiccoloError::UnknownFormatCode {
                            line: line_start,
                            code: c as char,
                        }
                        .into())
                    }
                }
            } else {
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            Err(PiccoloError::UnterminatedString { line: line_start }.into())
        } else {
            self.advance();
            self.add_token(TokenKind::String(
                String::from_utf8(value).expect("invalid utf-8 sequence"),
            ));
            Ok(())
        }
    }

    fn number(&mut self) -> crate::Result<()> {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' {
            let range = self.lookahead(1) == b'.';

            while self.current != 0 && is_digit(self.peek()) {
                self.reverse();
            }

            if !range {
                return self.float();
            }
        }

        let value = String::from_utf8(self.source[self.start..self.current].to_vec())
            .map_err(|_| PiccoloError::InvalidUTF8 { line: self.line })?;
        if let Ok(i) = value.parse::<i64>() {
            self.add_token(TokenKind::Integer(i));
            Ok(())
        } else {
            Err(PiccoloError::InvalidNumberLiteral {
                line: self.line,
                literal: value,
            }
            .into())
        }
    }

    fn float(&mut self) -> crate::Result<()> {
        while is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == b'.' {
            self.advance();
            while is_digit(self.peek()) {
                self.advance();
            }
        }

        let value = String::from_utf8(self.source[self.start..self.current].to_vec())
            .map_err(|_| PiccoloError::InvalidUTF8 { line: self.line })?;
        if let Ok(f) = value.parse::<f64>() {
            self.add_token(TokenKind::Double(f));
            Ok(())
        } else {
            Err(PiccoloError::InvalidNumberLiteral {
                line: self.line,
                literal: value,
            }
            .into())
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token::new(
            kind,
            std::str::from_utf8(&self.source[self.start..self.current]).unwrap(),
            self.line,
        ));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> u8 {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn reverse(&mut self) -> u8 {
        self.current -= 1;
        self.source[self.current]
    }

    fn peek(&mut self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.source[self.current]
        }
    }

    fn lookahead(&mut self, n: usize) -> u8 {
        if self.is_at_end() || self.current + n >= self.source.len() {
            b'\0'
        } else {
            self.source[self.current + n]
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

pub fn is_non_identifier(c: u8) -> bool {
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
        || c == b'='
        || c == b'>'
        || c == b'<'
        || c == b'"'
}
