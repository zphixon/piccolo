use crate::{ErrorKind, PiccoloError, Token, TokenKind};

use std::collections::VecDeque;

/// Converts a piccolo source into a list of tokens.
#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a [u8],
    tokens: VecDeque<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source.as_bytes(),
            tokens: VecDeque::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub(super) fn scan_all(mut self) -> Result<Vec<Token<'a>>, PiccoloError> {
        while self.next()?.kind != TokenKind::Eof {}
        Ok(self.tokens.drain(0..).collect())
    }

    /// Take the next token from the scanner.
    pub fn next_token(&mut self) -> Result<Token<'a>, PiccoloError> {
        if self.tokens.is_empty() {
            self.next()?;
        }

        Ok(self.tokens.pop_front().unwrap())
    }

    /// Looks ahead in the token stream. Generates tokens if they do not exist. Duplicates
    /// TokenKind::Eof if necessary.
    pub fn peek_token<'b>(&'b mut self, idx: usize) -> Result<&'b Token<'a>, PiccoloError> {
        if self.tokens.is_empty() {
            self.next()?;
        }

        if self.tokens.len() <= idx {
            while self.tokens.len() <= idx {
                self.next()?;
            }
        }

        Ok(&self.tokens[idx])
    }

    fn slurp_whitespace(&mut self) {
        while self.peek_char() == b'#' || is_whitespace(self.peek_char()) {
            if self.peek_char() == b'#' {
                while !self.is_at_end() && self.peek_char() != b'\n' {
                    self.advance_char();
                }
            }
            while !self.is_at_end() && is_whitespace(self.peek_char()) {
                if self.advance_char() == b'\n' {
                    self.line += 1;
                }
            }
        }
    }

    fn next<'b>(&'b mut self) -> Result<&'b Token<'a>, PiccoloError> {
        self.slurp_whitespace();
        if self.is_at_end() {
            self.add_token(TokenKind::Eof)?;
            return Ok(&self.tokens[self.tokens.len() - 1]);
        }

        self.start = self.current;
        let tk = match self.advance_char() {
            b'[' => TokenKind::LeftBracket,
            b']' => TokenKind::RightBracket,
            b'(' => TokenKind::LeftParen,
            b')' => TokenKind::RightParen,
            b',' => TokenKind::Comma,
            b'-' => TokenKind::Minus,
            b'+' => TokenKind::Plus,
            b'*' => TokenKind::Multiply,
            b'/' => TokenKind::Divide,
            b'^' => TokenKind::BitwiseXor,
            b'%' => TokenKind::Modulo,

            b'@' => TokenKind::At,
            b'$' => TokenKind::Dollar,
            b'\'' => TokenKind::SingleQuote,
            b'`' => TokenKind::Grave,
            b'{' => TokenKind::LeftBrace,
            b'}' => TokenKind::RightBrace,
            b':' => TokenKind::Colon,
            b'?' => TokenKind::Question,
            b'\\' => TokenKind::Backslash,
            b';' => TokenKind::Semicolon,
            b'~' => TokenKind::Tilde,

            b'&' => {
                if self.peek_char() == b'&' {
                    self.advance_char();
                    TokenKind::LogicalAnd
                } else {
                    TokenKind::BitwiseAnd
                }
            }

            b'|' => {
                if self.peek_char() == b'|' {
                    self.advance_char();
                    TokenKind::LogicalOr
                } else {
                    TokenKind::BitwiseOr
                }
            }

            b'.' => {
                if self.peek_char() == b'.' {
                    self.advance_char();
                    if self.peek_char() == b'.' {
                        self.advance_char();
                        TokenKind::InclusiveRange
                    } else {
                        TokenKind::ExclusiveRange
                    }
                } else {
                    TokenKind::Period
                }
            }

            b'!' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::NotEqual
                } else {
                    TokenKind::Not
                }
            }

            b'=' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::Equal
                } else if self.peek_char() == b':' {
                    self.advance_char();
                    TokenKind::Declare
                } else {
                    TokenKind::Assign
                }
            }

            b'>' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::GreaterEqual
                } else if self.peek_char() == b'>' {
                    self.advance_char();
                    TokenKind::ShiftRight
                } else {
                    TokenKind::Greater
                }
            }

            b'<' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::LessEqual
                } else if self.peek_char() == b'<' {
                    self.advance_char();
                    TokenKind::ShiftLeft
                } else {
                    TokenKind::Less
                }
            }

            b'"' => self.scan_string()?,

            c => {
                if is_digit(c) {
                    self.scan_number()?
                } else if is_whitespace(c) {
                    panic!("found whitespace where there shouldn't be any");
                } else {
                    self.identifier_or_keyword()?
                }
            }
        };
        self.add_token(tk)?;
        Ok(&self.tokens[self.tokens.len() - 1])
    }

    fn identifier_or_keyword(&mut self) -> Result<TokenKind, PiccoloError> {
        while !is_non_identifier(self.peek_char()) {
            self.advance_char();
        }

        if let Some(tk) = into_keyword(self.lexeme()?) {
            Ok(tk)
        } else {
            Ok(TokenKind::Identifier)
        }
    }

    fn scan_string(&mut self) -> Result<TokenKind, PiccoloError> {
        let line_start = self.line;
        while self.peek_char() != b'"' && !self.is_at_end() {
            if self.peek_char() == b'\n' {
                self.line += 1;
            }

            if self.peek_char() == b'\\' {
                self.advance_char();
                self.line += 1;
            }

            self.advance_char();
        }

        if self.is_at_end() {
            Err(PiccoloError::new(ErrorKind::UnterminatedString).line(line_start))
        } else {
            self.advance_char();
            Ok(TokenKind::String)
        }
    }

    fn scan_number(&mut self) -> Result<TokenKind, PiccoloError> {
        while is_digit(self.peek_char()) {
            self.advance_char();
        }

        if self.peek_char() == b'.' {
            let range = self.lookahead_char(1) == b'.';

            while self.current != 0 && is_digit(self.peek_char()) {
                self.reverse_char();
            }

            if !range {
                return self.scan_float();
            }
        }

        let value = self.lexeme()?;
        if let Ok(i) = value.parse::<i64>() {
            Ok(TokenKind::Integer(i))
        } else {
            Err(PiccoloError::new(ErrorKind::InvalidNumberLiteral {
                literal: value.to_owned(),
            })
            .line(self.line))
        }
    }

    fn scan_float(&mut self) -> Result<TokenKind, PiccoloError> {
        while is_digit(self.peek_char()) {
            self.advance_char();
        }
        if self.peek_char() == b'.' {
            self.advance_char();
            while is_digit(self.peek_char()) {
                self.advance_char();
            }
        }

        let value = self.lexeme()?;
        if let Ok(f) = value.parse::<f64>() {
            Ok(TokenKind::Double(f))
        } else {
            Err(PiccoloError::new(ErrorKind::InvalidNumberLiteral {
                literal: value.to_owned(),
            })
            .line(self.line))
        }
    }

    fn add_token(&mut self, kind: TokenKind) -> Result<(), PiccoloError> {
        self.tokens
            .push_back(Token::new(kind, self.lexeme()?, self.line));

        Ok(())
    }

    fn lexeme(&self) -> Result<&'a str, PiccoloError> {
        Ok(core::str::from_utf8(
            &self.source[self.start..self.current],
        )?)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance_char(&mut self) -> u8 {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn reverse_char(&mut self) -> u8 {
        self.current -= 1;
        self.source[self.current]
    }

    fn peek_char(&mut self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.source[self.current]
        }
    }

    fn lookahead_char(&mut self, n: usize) -> u8 {
        if self.is_at_end() || self.current + n >= self.source.len() {
            b'\0'
        } else {
            self.source[self.current + n]
        }
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

fn is_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

pub(super) fn is_whitespace(c: u8) -> bool {
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
        || c == b'@'
        || c == b'$'
        || c == b'\''
        || c == b'`'
        || c == b'{'
        || c == b'}'
        || c == b':'
        || c == b'?'
        || c == b'\\'
        || c == b';'
        || c == b'~'
}
