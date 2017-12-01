
use token::{Token, TokenKind};

pub struct Scanner {
    source: Vec<u8>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u64,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source: source.into_bytes(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, String> {
        let mut err = String::new();

        while !self.is_at_end() {
            self.start = self.current;
            if let Err(e) = self.scan_token() {
                err = format!("{}\n\n{}", err, e);
            }
        }

        self.tokens.push(Token::new(TokenKind::Eof, "".into(), self.line));

        if err.len() > 0 {
            Err(err)
        } else {
            Ok(self.tokens)
        }
    }

    pub fn scan_token(&mut self) -> Result<(), String> {
        let mut err = Ok(());
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

            b',' => self.add_token(TokenKind::Comma),
            b'.' => self.add_token(TokenKind::Dot),
            b'-' => self.add_token(TokenKind::Hyphen),
            b'+' => self.add_token(TokenKind::Plus),
            b'*' => self.add_token(TokenKind::Star),

            b'=' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::Equals);
                } else {
                    self.add_token(TokenKind::Assign);
                }
                self.advance();
            }

            b'>' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::GreaterThanEquals);
                } else {
                    self.add_token(TokenKind::GreaterThan);
                }
                self.advance();
            }

            b'<' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::LessThanEquals);
                } else {
                    self.add_token(TokenKind::LessThan);
                }
                self.advance();
            }

            _ => {
                if is_alpha(self.peek()) {
                    self.ident();
                } else if is_digit(self.peek()) {
                    err = self.number();
                } else {
                    err = Err("dunno".into());
                }
            }
        }
        err
    }

    fn ident(&mut self) {
        while is_alpha(self.peek()) {
            self.advance();
        }
        self.add_token(TokenKind::Ident);
    }

    fn number(&mut self) -> Result<(), String> {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' {
            // XXX: sketchy
            self.reverse();
            while is_digit(self.peek()) {
                self.reverse();
            }
            self.advance();
            return self.float();
        }

        let value = String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
        if let Ok(i) = value.parse::<i64>() {
            self.add_token(TokenKind::Integer(i));
            Ok(())
        } else {
            Err("could not parse number".into())
        }
    }

    fn float(&mut self) -> Result<(), String> {
        while is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == b'.' {
            self.advance();
            while is_digit(self.peek()) {
                self.advance();
            }
        }

        let value = String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
        if let Ok(f) = value.parse::<f64>() {
            self.add_token(TokenKind::Double(f));
            Ok(())
        } else {
            Err("could not parse number".into())
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        let text = String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
        self.tokens.push(Token::new(kind, text, self.line));
    }

    fn add_token_string(&mut self, kind: TokenKind, value: String) {
        self.tokens.push(Token::new(kind, value, self.line));
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
}

fn is_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}

fn is_alpha(c: u8) -> bool {
    b'A' <= c && c <= b'Z'
        || b'a' <= c && c <= b'z'
        || c == b'_'
}

fn is_alphanumeric(c: u8) -> bool {
    is_digit(c) || is_alpha(c)
}

