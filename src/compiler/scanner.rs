//! Contains `Scanner`, an on-demand producer of tokens.

use crate::{
    compiler::{Pos, Token, TokenKind},
    error::PiccoloError,
    make_error,
};
use std::collections::VecDeque;

/// Converts a piccolo source into a stream of [`Token`].
///
/// Operates in a scan-on-demand fashion. A consumer of tokens calls the [`next_token`]
/// method, which produces the next token from the source. A consumer can also
/// look ahead in the stream using [`peek_token`], which produces tokens as
/// required.
///
/// [`Token`]s internally are stored in a [`VecDeque`] which increases in size if
/// the scanner produces new tokens when [`next_token`] is called. In Piccolo, the
/// parser calls `peek_token` with a maximum of 2, so the size of the token buffer
/// is never larger than 2 tokens.
///
/// [`VecDeque`]: https://doc.rust-lang.org/stable/std/collections/struct.VecDeque.html
/// [`Token`]: ../struct.Token.html
/// [`next_token`]: ./struct.Scanner.html#method.next_token
/// [`peek_token`]: ./struct.Scanner.html#method.peek_token
#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a [u8],
    tokens: VecDeque<Token<'a>>,
    start: usize,
    current: usize,
    start_pos: Pos,
    current_pos: Pos,
}

impl<'a> Scanner<'a> {
    /// Create a new `Scanner` from a source.
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source.as_bytes(),
            tokens: VecDeque::new(),
            start: 0,
            current: 0,
            start_pos: Pos::Source { line: 1, col: 1 },
            current_pos: Pos::Source { line: 1, col: 1 },
        }
    }

    pub fn scan_all(mut self) -> Result<Vec<Token<'a>>, PiccoloError> {
        while self.next()?.kind != TokenKind::Eof {}
        Ok(self.tokens.drain(0..).collect())
    }

    /// Produce the next token, moving it out of the scanner. Returns [`TokenKind::Eof`]
    /// if the scanner is at the end of the source.
    ///
    /// [`TokenKind::Eof`]: ../struct.TokenKind.html
    pub fn next_token(&mut self) -> Result<Token<'a>, PiccoloError> {
        if self.tokens.is_empty() {
            self.next()?;
        }

        Ok(self.tokens.pop_front().unwrap())
    }

    /// Looks ahead in the token stream, generating tokens if they do not exist.
    pub fn peek_token<'b>(&'b mut self, index: usize) -> Result<&'b Token<'a>, PiccoloError> {
        if self.tokens.is_empty() {
            self.next()?;
        }

        while self.tokens.len() <= index {
            self.next()?;
        }

        Ok(&self.tokens[index])
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
                    self.advance_line();
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

        self.set_start();
        let tk = match self.advance_char() {
            b'[' => TokenKind::LeftBracket,
            b']' => TokenKind::RightBracket,
            b'(' => TokenKind::LeftParen,
            b')' => TokenKind::RightParen,
            b',' => TokenKind::Comma,

            b'+' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::PlusAssign
                } else {
                    TokenKind::Plus
                }
            }
            b'-' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::MinusAssign
                } else {
                    TokenKind::Minus
                }
            }
            b'*' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::MultiplyAssign
                } else {
                    TokenKind::Multiply
                }
            }
            b'/' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::DivideAssign
                } else {
                    TokenKind::Divide
                }
            }
            b'^' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::BitwiseXorAssign
                } else {
                    TokenKind::BitwiseXor
                }
            }
            b'%' => {
                if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::ModuloAssign
                } else {
                    TokenKind::Modulo
                }
            }

            b'@' => TokenKind::At,
            b'$' => TokenKind::Dollar,
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
                } else if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::BitwiseAndAssign
                } else {
                    TokenKind::BitwiseAnd
                }
            }

            b'|' => {
                if self.peek_char() == b'|' {
                    self.advance_char();
                    TokenKind::LogicalOr
                } else if self.peek_char() == b'=' {
                    self.advance_char();
                    TokenKind::BitwiseOrAssign
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
                    if self.peek_char() == b'=' {
                        self.advance_char();
                        TokenKind::ShiftRightAssign
                    } else {
                        TokenKind::ShiftRight
                    }
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
                    if self.peek_char() == b'=' {
                        self.advance_char();
                        TokenKind::ShiftLeftAssign
                    } else {
                        TokenKind::ShiftLeft
                    }
                } else {
                    TokenKind::Less
                }
            }

            c @ (b'"' | b'\'') => self.scan_string(c)?,

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

    fn scan_string(&mut self, quote: u8) -> Result<TokenKind, PiccoloError> {
        while self.peek_char() != quote && !self.is_at_end() {
            if self.peek_char() == b'\n' {
                self.advance_line();
            }

            if self.peek_char() == b'\\' {
                self.advance_char();
                self.advance_line();
            }

            if !self.is_at_end() {
                self.advance_char();
            }
        }

        if self.is_at_end() {
            Err(make_error!(UnterminatedString).pos(self.start_pos))
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
            Err(make_error!(InvalidNumberLiteral {
                literal: value.to_owned(),
            })
            .pos(self.current_pos))
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
            Err(make_error!(InvalidNumberLiteral {
                literal: value.to_owned(),
            })
            .pos(self.current_pos))
        }
    }

    fn add_token(&mut self, kind: TokenKind) -> Result<(), PiccoloError> {
        self.tokens
            .push_back(Token::new(kind, self.lexeme()?, self.start_pos));

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

    fn set_start(&mut self) {
        self.start = self.current;
        self.start_pos = self.current_pos;
    }

    fn advance_line(&mut self) {
        self.current_pos.inc_line();
        self.current_pos.reset_col();
    }

    fn advance_char(&mut self) -> u8 {
        self.current_pos.inc_col();
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
        "elseif" => Some(TokenKind::ElseIf),
        "while" => Some(TokenKind::While),
        "for" => Some(TokenKind::For),
        "in" => Some(TokenKind::In),
        "data" => Some(TokenKind::Data),
        "let" => Some(TokenKind::Let),
        "me" => Some(TokenKind::Me),
        "new" => Some(TokenKind::New),
        "err" => Some(TokenKind::Err),
        "break" => Some(TokenKind::Break),
        "continue" => Some(TokenKind::Continue),
        "return" => Some(TokenKind::Return),
        "assert" => Some(TokenKind::Assert),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "nil" => Some(TokenKind::Nil),
        _ => None,
    }
}

fn is_digit(c: u8) -> bool {
    (b'0'..=b'9').contains(&c)
}

pub(super) fn is_whitespace(c: u8) -> bool {
    c == 0x09 || c == 0x0A || c == 0x0B || c == 0x0C || c == 0x0D || c == 0x20
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn multi_char_ops() {
        let src = "++=--=//=**=%%=";

        let mut scanner = Scanner::new(src);
        while scanner.next().unwrap().kind != TokenKind::Eof {}
        let tokens: Vec<TokenKind> = scanner.tokens.drain(0..).map(|t| t.kind).collect();

        assert_eq!(
            tokens,
            &[
                TokenKind::Plus,
                TokenKind::PlusAssign,
                TokenKind::Minus,
                TokenKind::MinusAssign,
                TokenKind::Divide,
                TokenKind::DivideAssign,
                TokenKind::Multiply,
                TokenKind::MultiplyAssign,
                TokenKind::Modulo,
                TokenKind::ModuloAssign,
                TokenKind::Eof,
            ],
        );

        let src = "&&&=&|||=|";

        let mut scanner = Scanner::new(src);
        while scanner.next().unwrap().kind != TokenKind::Eof {}
        let tokens: Vec<TokenKind> = scanner.tokens.drain(0..).map(|t| t.kind).collect();

        assert_eq!(
            tokens,
            &[
                TokenKind::LogicalAnd,
                TokenKind::BitwiseAndAssign,
                TokenKind::BitwiseAnd,
                TokenKind::LogicalOr,
                TokenKind::BitwiseOrAssign,
                TokenKind::BitwiseOr,
                TokenKind::Eof,
            ]
        );

        let src = "<<<=<<==<";

        let mut scanner = Scanner::new(src);
        while scanner.next().unwrap().kind != TokenKind::Eof {}
        let tokens: Vec<TokenKind> = scanner.tokens.drain(0..).map(|t| t.kind).collect();

        assert_eq!(
            tokens,
            &[
                TokenKind::ShiftLeft,
                TokenKind::LessEqual,
                TokenKind::ShiftLeftAssign,
                TokenKind::Assign,
                TokenKind::Less,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn scanner() {
        let src = "a = 3\nio.prln(a)\n";
        let mut scanner = Scanner::new(src);
        assert_eq!(scanner.peek_token(0).unwrap(), &Token::identifier("a"));
        assert_eq!(
            scanner.peek_token(1).unwrap(),
            &Token::test(TokenKind::Assign)
        );
        assert_eq!(scanner.next_token().unwrap(), Token::identifier("a"));

        assert_eq!(
            scanner.peek_token(0).unwrap(),
            &Token::test(TokenKind::Assign)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::test(TokenKind::Assign)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::test(TokenKind::Integer(3))
        );
        assert_eq!(scanner.next_token().unwrap(), Token::identifier("io"));
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::test(TokenKind::Period)
        );
        assert_eq!(scanner.next_token().unwrap(), Token::identifier("prln"));
    }

    #[test]
    fn scanner2() {
        //               0 1 2  3 45   678
        let src = "a = 3\nio.prln(a)\n";
        let mut scanner = Scanner::new(src);

        assert_eq!(scanner.peek_token(0).unwrap(), &Token::identifier("a"));
        assert_eq!(
            scanner.peek_token(1).unwrap(),
            &Token::test(TokenKind::Assign)
        );
        assert_eq!(scanner.peek_token(0).unwrap(), &Token::identifier("a"));
        assert_eq!(
            scanner.peek_token(6).unwrap(),
            &Token::test(TokenKind::LeftParen)
        );
        assert_eq!(
            scanner.peek_token(8).unwrap(),
            &Token::test(TokenKind::RightParen)
        );

        assert_eq!(scanner.next_token().unwrap(), Token::identifier("a"));
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::test(TokenKind::Assign)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::test(TokenKind::Integer(3))
        );
        assert_eq!(scanner.next_token().unwrap(), Token::identifier("io"));
    }
}
