use crate::{ErrorKind, PiccoloError, Token, TokenKind};

use super::{is_digit, is_whitespace};

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
                while self.peek_char() != b'\n' {
                    self.advance_char();
                }
            }
            while is_whitespace(self.peek_char()) {
                if self.advance_char() == b'\n' {
                    self.line += 1;
                }
            }
        }
    }

    fn next<'b>(&'b mut self) -> Result<&'b Token<'a>, PiccoloError> {
        self.slurp_whitespace();
        if self.is_at_end() {
            self.add_token(TokenKind::Eof);
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
        self.add_token(tk);
        Ok(&self.tokens[self.tokens.len() - 1])
    }

    fn identifier_or_keyword(&mut self) -> Result<TokenKind, PiccoloError> {
        while !super::is_non_identifier(self.peek_char()) {
            self.advance_char();
        }

        if let Some(tk) = super::into_keyword(
            core::str::from_utf8(&self.source[self.start..self.current])
                .map_err(|_| PiccoloError::new(ErrorKind::InvalidUTF8).line(self.line))?,
        ) {
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

        let value = core::str::from_utf8(&self.source[self.start..self.current])
            .map_err(|_| PiccoloError::new(ErrorKind::InvalidUTF8).line(self.line))?;
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

        let value = core::str::from_utf8(&self.source[self.start..self.current])
            .map_err(|_| PiccoloError::new(ErrorKind::InvalidUTF8).line(self.line))?;
        if let Ok(f) = value.parse::<f64>() {
            Ok(TokenKind::Double(f))
        } else {
            Err(PiccoloError::new(ErrorKind::InvalidNumberLiteral {
                literal: value.to_owned(),
            })
            .line(self.line))
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push_back(Token::new(
            kind,
            core::str::from_utf8(&self.source[self.start..self.current]).unwrap(),
            self.line,
        ));
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
