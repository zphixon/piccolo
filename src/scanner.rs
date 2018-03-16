use token::{Token, TokenKind};

fn keywords(s: &str) -> Option<TokenKind> {
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
        //"pub" => Some(TokenKind::Pub),
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
                err.push_str(&e);
                err.push('\n');
            }
        }

        self.tokens
            .push(Token::new(TokenKind::Eof, "".into(), self.line));

        if !err.is_empty() {
            Err(err)
        } else {
            Ok(self.tokens)
        }
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let mut err = Ok(());
        match self.advance() {
            b'#' => while self.peek() != b'\n' && !self.is_at_end() {
                self.advance();
            },

            b'\n' => {
                self.add_token(TokenKind::Newline);
                self.line += 1;
            }

            b' ' | b'\t' | b'\r' => {}

            b'[' => self.add_token(TokenKind::LBracket),
            b']' => self.add_token(TokenKind::RBracket),
            b'(' => self.add_token(TokenKind::LParen),
            b')' => self.add_token(TokenKind::RParen),
            b',' => self.add_token(TokenKind::Comma),
            b'-' => self.add_token(TokenKind::Minus),
            b'+' => self.add_token(TokenKind::Plus),
            b'*' => self.add_token(TokenKind::Star),
            b'/' => self.add_token(TokenKind::Divide),
            b'^' => self.add_token(TokenKind::BXor),
            b'%' => self.add_token(TokenKind::Mod),

            b'&' => {
                if self.peek() == b'&' {
                    self.advance();
                    self.add_token(TokenKind::And);
                } else {
                    self.add_token(TokenKind::BAnd);
                }
            }

            b'|' => {
                if self.peek() == b'|' {
                    self.advance();
                    self.add_token(TokenKind::Or);
                } else {
                    self.add_token(TokenKind::BOr);
                }
            }

            b'.' => {
                if self.peek() == b'.' {
                    self.advance();
                    if self.peek() == b'.' {
                        self.advance();
                        self.add_token(TokenKind::IRange);
                    } else {
                        self.add_token(TokenKind::ERange);
                    }
                } else {
                    self.add_token(TokenKind::Dot);
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
                } else {
                    self.add_token(TokenKind::GreaterThan);
                }
            }

            b'<' => {
                if self.peek() == b'=' {
                    self.advance();
                    self.add_token(TokenKind::LessThanEquals);
                } else {
                    self.add_token(TokenKind::LessThan);
                }
            }

            b'"' => {
                if let Err(e) = self.string() {
                    err = Err(format!("{}: Bad string: {}", self.line, e));
                }
            }

            c => {
                if is_alpha(c) {
                    self.ident();
                } else if is_digit(c) {
                    err = self.number();
                } else {
                    err = Err(format!("{}: Unexpected char: {}", self.line, c as char));
                }
            }
        }
        err
    }

    fn ident(&mut self) {
        while is_alphanumeric(self.peek()) {
            self.advance();
        }

        let value = String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();

        if let Some(tk) = keywords(&value) {
            self.add_token(tk);
        } else {
            self.add_token(TokenKind::Ident);
        }
    }

    fn string(&mut self) -> Result<(), String> {
        let mut value = String::new();
        let line_start = self.line;
        while self.peek() != b'"' && !self.is_at_end() {
            if self.peek() == b'\n' {
                self.line += 1;
            }

            if self.peek() == b'\\' {
                self.advance();
                match self.advance() {
                    b'n' => {
                        value.push('\n');
                    }
                    b'r' => {
                        value.push('\r');
                    }
                    b'\\' => {
                        value.push('\\');
                    }
                    b'"' => {
                        value.push('"');
                    }
                    b'\n' => {
                        self.advance();
                        while self.peek() == b' ' || self.peek() == b'\t' {
                            self.advance();
                        }
                        self.reverse();
                    }
                    c => return Err(format!("{} unknown format code: {}", self.line, c as char)),
                }
            } else {
                value.push(self.advance() as char);
            }
        }

        if self.is_at_end() {
            Err(format!(
                "{} Unterminated string starting at {}",
                self.line, line_start
            ))
        } else {
            self.advance();
            self.add_token_string(TokenKind::String, value);
            Ok(())
        }
    }

    fn number(&mut self) -> Result<(), String> {
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

fn is_alpha(c: u8) -> bool {
    b'A' <= c && c <= b'Z' || b'a' <= c && c <= b'z' || c == b'_'
}

fn is_alphanumeric(c: u8) -> bool {
    is_digit(c) || is_alpha(c)
}
