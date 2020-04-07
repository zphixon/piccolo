use crate::chunk::Chunk;
use crate::error::{ErrorKind, PiccoloError};
use crate::op::Opcode;
use crate::scanner::{Token, TokenKind};
use crate::value::Value;

macro_rules! prec {
    ($name:ident => $($item:ident = $num:expr,)*) => {
        #[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
        pub(crate) enum $name {
            $($item = $num,)*
        }

        impl Into<u8> for $name {
            fn into(self) -> u8 {
                match self {
                    $($name::$item => $num,)*
                }
            }
        }

        impl From<u8> for $name {
            fn from(u: u8) -> $name {
                match u {
                    $($num => $name::$item,)*
                    n => panic!("{} does not correspond to any item in {}", n, stringify!($name))
                }
            }
        }

        impl std::ops::Add<u8> for $name {
            type Output = $name;
            fn add(self, rhs: u8) -> $name {
                let s: u8 = self.into();
                (s + rhs).into()
            }
        }
    };
}

prec!(Precedence =>
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
);

/// Compiles a list of tokens to Piccolo bytecode.
// TODO: scan on demand
pub struct Compiler<'a> {
    chunk: Chunk,
    previous: usize,
    current: usize,
    tokens: &'a [Token<'a>],
    output: bool,
    rules: &'a [(TokenKind, Option<ParseRule>, Option<ParseRule>, Precedence)],
}

type ParseRule = fn(&mut Compiler) -> Result<(), PiccoloError>;

// TODO: We need some sort of error reporting struct
// right now, when we encounter an error, we return all the way back up to
// the main loop in compile where errors are collected sequentially.
// what would make more sense is to have a flag that says whether or not
// we should still be outputting bytecode, and still attempt to parse
// the entire program regardless.
impl<'a> Compiler<'a> {
    /// Parses and compiles a list of tokens. Uses a Pratt parser.
    pub fn compile(chunk: Chunk, s: &[Token]) -> Result<Chunk, Vec<PiccoloError>> {
        let mut compiler = Compiler {
            chunk,
            previous: 0,
            current: 0,
            tokens: s,
            output: true,
            rules: &[
                // token, prefix, infix, precedence
                (TokenKind::Do, None, None, Precedence::None),
                (TokenKind::End, None, None, Precedence::None),
                (TokenKind::Fn, None, None, Precedence::None),
                (TokenKind::If, None, None, Precedence::None),
                (TokenKind::Else, None, None, Precedence::None),
                (TokenKind::While, None, None, Precedence::None),
                (TokenKind::For, None, None, Precedence::None),
                (TokenKind::In, None, None, Precedence::None),
                (TokenKind::Data, None, None, Precedence::None),
                (TokenKind::Is, None, None, Precedence::None),
                (TokenKind::Me, None, None, Precedence::None),
                (TokenKind::New, None, None, Precedence::None),
                (TokenKind::Err, None, None, Precedence::None),
                (TokenKind::Retn, None, None, Precedence::None),
                (
                    TokenKind::Nil,
                    Some(|c| Compiler::literal(c)),
                    None,
                    Precedence::None,
                ),
                (TokenKind::LeftBracket, None, None, Precedence::None),
                (TokenKind::RightBracket, None, None, Precedence::None),
                (
                    TokenKind::LeftParen,
                    Some(|c| Compiler::grouping(c)),
                    None,
                    Precedence::None,
                ),
                (TokenKind::RightParen, None, None, Precedence::None),
                (TokenKind::Comma, None, None, Precedence::None),
                (TokenKind::Period, None, None, Precedence::None),
                (TokenKind::ExclusiveRange, None, None, Precedence::None),
                (TokenKind::InclusiveRange, None, None, Precedence::None),
                (TokenKind::Assign, None, None, Precedence::None),
                (TokenKind::Declare, None, None, Precedence::None),
                (
                    TokenKind::Not,
                    Some(|c| Compiler::unary(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::Plus,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Term,
                ),
                (
                    TokenKind::Minus,
                    Some(|c| Compiler::unary(c)),
                    Some(|c| Compiler::binary(c)),
                    Precedence::Term,
                ),
                (
                    TokenKind::Multiply,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Factor,
                ),
                (
                    TokenKind::Divide,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Factor,
                ),
                (
                    TokenKind::Modulo,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Factor,
                ),
                (TokenKind::LogicalAnd, None, None, Precedence::None),
                (TokenKind::LogicalOr, None, None, Precedence::None),
                (TokenKind::BitwiseAnd, None, None, Precedence::None),
                (TokenKind::BitwiseOr, None, None, Precedence::None),
                (TokenKind::BitwiseXor, None, None, Precedence::None),
                (
                    TokenKind::Equal,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Equality,
                ),
                (
                    TokenKind::NotEqual,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Equality,
                ),
                (
                    TokenKind::Less,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::Greater,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::LessEqual,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::GreaterEqual,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (TokenKind::ShiftLeft, None, None, Precedence::None),
                (TokenKind::ShiftRight, None, None, Precedence::None),
                (
                    TokenKind::Identifier,
                    Some(|c| Compiler::variable(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::True,
                    Some(|c| Compiler::literal(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::False,
                    Some(|c| Compiler::literal(c)),
                    None,
                    Precedence::None,
                ),
                (TokenKind::Eof, None, None, Precedence::None),
                (
                    TokenKind::String,
                    Some(|c| Compiler::string(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::Double(0.0),
                    Some(|c| Compiler::number(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::Integer(0),
                    Some(|c| Compiler::number(c)),
                    None,
                    Precedence::None,
                ),
            ],
        };

        let mut errors = vec![];

        while !compiler.is_at_end() && !compiler.matches(TokenKind::Eof) {
            if let Err(err) = compiler.declaration() {
                errors.push(err);
                compiler.output = false;
            }
        }

        if errors.is_empty() {
            Ok(compiler.chunk)
        } else {
            Err(errors)
        }
    }

    fn consume(&mut self, token: TokenKind) -> Result<(), PiccoloError> {
        if self.current().kind != token {
            self.advance();
            Err(PiccoloError::new(ErrorKind::UnexpectedToken {
                exp: format!("{:?}", token),
                got: format!("{}", self.previous()),
            })
            .line(self.previous().line))
        } else {
            self.advance();
            Ok(())
        }
    }

    fn matches(&mut self, kind: TokenKind) -> bool {
        if !self.check(kind) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    fn is_at_end(&self) -> bool {
        self.current == self.tokens.len()
    }

    fn declaration(&mut self) -> Result<(), PiccoloError> {
        if self.can_lookahead(1) && self.lookahead(1).kind == TokenKind::Declare {
            self.var_declaration()
        } else if self.can_lookahead(1) && self.lookahead(1).kind == TokenKind::Assign {
            self.var_assign()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<(), PiccoloError> {
        let global = self.parse_variable()?;
        self.consume(TokenKind::Declare)?;
        self.expression()?;
        self.define_variable(global);
        Ok(())
    }

    fn var_assign(&mut self) -> Result<(), PiccoloError> {
        let global = self.parse_variable()?;
        self.consume(TokenKind::Assign)?;
        self.expression()?;
        self.set_variable(global);
        Ok(())
    }

    fn parse_variable(&mut self) -> Result<u16, PiccoloError> {
        self.consume(TokenKind::Identifier)?;
        Ok(self.identifier_constant(&self.tokens[self.previous]))
    }

    fn define_variable(&mut self, var: u16) {
        self.emit3(Opcode::DefineGlobal, var);
    }

    fn set_variable(&mut self, var: u16) {
        self.emit3(Opcode::AssignGlobal, var);
    }

    fn statement(&mut self) -> Result<(), PiccoloError> {
        if self.matches(TokenKind::Retn) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<(), PiccoloError> {
        self.expression()?;
        self.emit(Opcode::Return);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), PiccoloError> {
        self.expression()?;
        self.emit(Opcode::Pop);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), PiccoloError> {
        if self.check(TokenKind::Eof) {
            Err(PiccoloError::new(ErrorKind::ExpectedExpression {
                got: self.current().lexeme.to_owned(),
            })
            .line(self.previous().line))
        } else {
            self.precedence(Precedence::Assignment)
        }
    }

    fn precedence(&mut self, prec: Precedence) -> Result<(), PiccoloError> {
        self.advance();
        if let (Some(prefix), _, _) = self.get_rule(&self.previous().kind) {
            // TODO: refactor to check if assignment is allowed
            // prefix needs to take a bool argument, pass it to named_variable where
            // if we match an assignment token and you can assign to the target, then
            // parse an expression, although targets like a*b=c+d already fail.
            //prefix(self, prec <= Precedence::Assignment)?;
            prefix(self)?;
        } else {
            return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                from: self.previous().lexeme.to_owned(),
            })
            .line(self.previous().line));
        }
        while prec <= *self.get_rule(&self.current().kind).2 {
            self.advance();
            if let (_, Some(infix), _) = self.get_rule(&self.previous().kind) {
                infix(self)?;
            } else {
                panic!("no infix rule for {:?}", self.previous().kind);
            }
        }
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), PiccoloError> {
        self.expression()?;
        self.consume(TokenKind::RightParen)
    }

    fn unary(&mut self) -> Result<(), PiccoloError> {
        let kind = self.previous().kind.clone();
        self.precedence(Precedence::Unary)?;
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),
            TokenKind::Not => self.emit(Opcode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), PiccoloError> {
        let kind = self.previous().kind.clone();
        let (_, _, &prec) = self.get_rule(&kind);
        self.precedence((prec as u8 + 1).into())?;
        match kind {
            TokenKind::Plus => self.emit(Opcode::Add),
            TokenKind::Minus => self.emit(Opcode::Subtract),
            TokenKind::Divide => self.emit(Opcode::Divide),
            TokenKind::Multiply => self.emit(Opcode::Multiply),
            TokenKind::Equal => self.emit(Opcode::Equal),
            TokenKind::NotEqual => self.emit2(Opcode::Equal, Opcode::Not),
            TokenKind::Greater => self.emit(Opcode::Greater),
            TokenKind::GreaterEqual => self.emit2(Opcode::Less, Opcode::Not),
            TokenKind::Less => self.emit(Opcode::Less),
            TokenKind::LessEqual => self.emit2(Opcode::Greater, Opcode::Not),
            _ => {}
        }
        Ok(())
    }

    fn advance(&mut self) {
        // if we wanted to do on-demand scanning we would make a call to the scanner here
        self.previous = self.current;
        self.current += 1;
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.previous]
    }

    fn current(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn can_lookahead(&self, num: usize) -> bool {
        self.tokens.len() > self.current + num
    }

    fn lookahead(&self, num: usize) -> &Token {
        &self.tokens[self.current + num]
    }

    fn number(&mut self) -> Result<(), PiccoloError> {
        if let Ok(value) = self.previous().lexeme.parse::<i64>() {
            self.emit_constant(Value::Integer(value));
            Ok(())
        } else if let Ok(value) = self.previous().lexeme.parse::<f64>() {
            self.emit_constant(Value::Double(value));
            Ok(())
        } else {
            Err(PiccoloError::new(ErrorKind::InvalidNumberLiteral {
                literal: self.previous().lexeme.to_owned(),
            })
            .line(self.previous().line))
        }
    }

    fn literal(&mut self) -> Result<(), PiccoloError> {
        match self.previous().kind {
            TokenKind::Nil => self.emit(Opcode::Nil),
            TokenKind::True => self.emit(Opcode::True),
            TokenKind::False => self.emit(Opcode::False),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn string(&mut self) -> Result<(), PiccoloError> {
        let s = match &self.previous().kind {
            TokenKind::String => {
                let s = self.previous().lexeme;
                let mut value = Vec::new();
                let line_start = self.previous().line;
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
                                while i < s.as_bytes().len() - 1
                                    && crate::scanner::is_whitespace(s.as_bytes()[i])
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

                String::from_utf8(value)
                    .map_err(|_| PiccoloError::new(ErrorKind::InvalidUTF8).line(line))?
            }
            _ => unreachable!(),
        };
        self.emit_constant(Value::String(s));
        Ok(())
    }

    fn variable(&mut self) -> Result<(), PiccoloError> {
        self.named_variable(&self.tokens[self.previous])
    }

    fn named_variable(&mut self, token: &Token) -> Result<(), PiccoloError> {
        let arg = self.identifier_constant(token);
        if self.matches(TokenKind::Assign) {
            return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                from: token.lexeme.to_owned(),
            })
            .line(self.previous().line));
        } else {
            self.emit3(Opcode::GetGlobal, arg);
        }
        Ok(())
    }

    fn identifier_constant(&mut self, token: &Token) -> u16 {
        if self.output {
            if !self
                .chunk
                .has_constant(&Value::String(token.lexeme.to_owned()))
            {
                self.chunk
                    .make_constant(Value::String(token.lexeme.to_owned()))
            } else {
                self.chunk.get_constant(token.lexeme)
            }
        } else {
            0
        }
    }

    fn emit_constant(&mut self, c: Value) {
        let c = self.chunk.make_constant(c);
        self.emit3(Opcode::Constant, c);
    }

    fn emit<T: Into<u8>>(&mut self, byte: T) {
        if self.output {
            self.chunk.write(byte, self.previous().line);
        }
    }

    fn emit2<T: Into<u8>, U: Into<u8>>(&mut self, byte1: T, byte2: U) {
        if self.output {
            self.chunk.write(byte1, self.previous().line);
            self.chunk.write(byte2, self.previous().line);
        }
    }

    fn emit3<T: Into<u8>, U: Into<u16>>(&mut self, byte1: T, bytes: U) {
        if self.output {
            let (low, high) = crate::decode_bytes(bytes.into());
            self.chunk.write(byte1, self.previous().line);
            self.chunk.write(low, self.previous().line);
            self.chunk.write(high, self.previous().line);
        }
    }

    fn get_rule(
        &'a self,
        kind: &TokenKind,
    ) -> (&'a Option<ParseRule>, &'a Option<ParseRule>, &'a Precedence) {
        for (k, infix, prefix, precedence) in self.rules.iter() {
            let rule = (infix, prefix, precedence);
            if k == kind {
                return rule;
            } else {
                if let TokenKind::Double(_) = kind {
                    if let TokenKind::Double(_) = k {
                        return rule;
                    }
                }

                if let TokenKind::Integer(_) = kind {
                    if let TokenKind::Integer(_) = k {
                        return rule;
                    }
                }

                if let TokenKind::String = kind {
                    if let TokenKind::String = k {
                        return rule;
                    }
                }
            }
        }

        panic!("no rule for {:?}", kind);
    }
}
