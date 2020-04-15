use crate::{Token, TokenKind, ErrorKind, PiccoloError};

use super::{is_digit, is_whitespace};

/// Converts a piccolo source into a list of tokens.
pub struct Scanner<'a> {
    source: &'a [u8],
    tokens: Vec<Option<Token<'a>>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    /// Create a new scanner from a source.
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source.as_bytes(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token<'a>>, PiccoloError> {
        while self.next_token()?.kind != TokenKind::Eof {}
        Ok(self.tokens.into_iter().map(Option::unwrap).collect())
    }

    pub(crate) fn current(&self) -> &Token<'a> {
        self.tokens[self.tokens.len() - 1].as_ref().unwrap()
    }

    pub(crate) fn previous(&self) -> &Token<'a> {
        self.tokens[self.tokens.len() - 2].as_ref().unwrap()
    }

    pub(crate) fn take_current(&mut self) -> Token<'a> {
        let l = self.tokens.len() - 1;
        self.tokens[l].take().unwrap()
    }

    pub(crate) fn take_previous(&mut self) -> Token<'a> {
        let l = self.tokens.len() - 2;
        self.tokens[l].take().unwrap()
    }

    fn slurp_whitespace(&mut self) {
        while self.peek() == b'#' || is_whitespace(self.peek()) {
            if self.peek() == b'#' {
                while self.peek() != b'\n' {
                    self.advance();
                }
            }
            while is_whitespace(self.peek()) {
                if self.advance() == b'\n' {
                    self.line += 1;
                }
            }
        }
    }

    pub(crate) fn next_token<'b>(&'b mut self) -> Result<&'b Token<'a>, PiccoloError> {
        self.slurp_whitespace();
        if self.is_at_end() {
            self.add_token(TokenKind::Eof);
            return Ok(self.current());
        }

        self.start = self.current;
        let tk = match self.advance() {
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
                if self.peek() == b'&' {
                    self.advance();
                    TokenKind::LogicalAnd
                } else {
                    TokenKind::BitwiseAnd
                }
            }

            b'|' => {
                if self.peek() == b'|' {
                    self.advance();
                    TokenKind::LogicalOr
                } else {
                    TokenKind::BitwiseOr
                }
            }

            b'.' => {
                if self.peek() == b'.' {
                    self.advance();
                    if self.peek() == b'.' {
                        self.advance();
                        TokenKind::InclusiveRange
                    } else {
                        TokenKind::ExclusiveRange
                    }
                } else {
                    TokenKind::Period
                }
            }

            b'!' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::NotEqual
                } else {
                    TokenKind::Not
                }
            }

            b'=' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::Equal
                } else {
                    TokenKind::Assign
                }
            }

            b'>' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::GreaterEqual
                } else if self.peek() == b'>' {
                    self.advance();
                    TokenKind::ShiftRight
                } else {
                    TokenKind::Greater
                }
            }

            b'<' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::LessEqual
                } else if self.peek() == b'<' {
                    self.advance();
                    TokenKind::ShiftLeft
                } else {
                    TokenKind::Less
                }
            }

            b'"' => self.string()?,

            c => {
                if is_digit(c) {
                    self.number()?
                } else if is_whitespace(c) {
                    panic!("found whitespace where there shouldn't be any");
                } else {
                    self.identifier_or_keyword()?
                }
            }
        };
        self.add_token(tk);
        Ok(self.current())
    }

    fn identifier_or_keyword(&mut self) -> Result<TokenKind, PiccoloError> {
        while !super::is_non_identifier(self.peek()) {
            self.advance();
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

    fn string(&mut self) -> Result<TokenKind, PiccoloError> {
        let line_start = self.line;
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1;
            }

            if self.peek() == b'\\' {
                self.advance();
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            Err(PiccoloError::new(ErrorKind::UnterminatedString).line(line_start))
        } else {
            self.advance();
            Ok(TokenKind::String)
        }
    }

    fn number(&mut self) -> Result<TokenKind, PiccoloError> {
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

    fn float(&mut self) -> Result<TokenKind, PiccoloError> {
        while is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == b'.' {
            self.advance();
            while is_digit(self.peek()) {
                self.advance();
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
        self.tokens.push(Some(Token::new(
            kind,
            core::str::from_utf8(&self.source[self.start..self.current]).unwrap(),
            self.line,
        )));
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
