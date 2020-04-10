use crate::chunk::Chunk;
use crate::error::{ErrorKind, PiccoloError};
use crate::op::Opcode;
use crate::scanner::{Token, TokenKind};
use crate::value::Value;

use std::collections::HashMap;

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

pub struct Compiler<'a> {
    chunk: Chunk,
    output: bool,
    assign: bool,
    current: usize,
    previous: usize,
    tokens: &'a [Token<'a>],
    identifiers: HashMap<&'a str, u16>,
    strings: HashMap<String, u16>,
    rules: Vec<(TokenKind, Option<PrefixRule>, Option<InfixRule>, Precedence)>,
}

type PrefixRule = fn(&mut Compiler, bool) -> Result<(), PiccoloError>;
type InfixRule = fn(&mut Compiler, bool) -> Result<(), PiccoloError>;

pub fn compile(chunk: Chunk, tokens: &[Token]) -> Result<Chunk, Vec<PiccoloError>> {
    let mut compiler = Compiler {
        chunk,
        output: true,
        assign: false,
        current: 0,
        previous: 0,
        tokens,
        identifiers: HashMap::new(),
        strings: HashMap::new(),
        rules: vec![
            (TokenKind::Do, None, None, Precedence::None),
            (TokenKind::End, None, None, Precedence::None),
            (TokenKind::Fn, None, None, Precedence::None),
            (TokenKind::If, None, None, Precedence::None),
            (TokenKind::Else, None, None, Precedence::None),
            (TokenKind::While, None, None, Precedence::None),
            (TokenKind::For, None, None, Precedence::None),
            (TokenKind::In, None, None, Precedence::None),
            (TokenKind::Data, None, None, Precedence::None),
            (TokenKind::Let, None, None, Precedence::None),
            (TokenKind::Is, None, None, Precedence::None),
            (TokenKind::Me, None, None, Precedence::None),
            (TokenKind::New, None, None, Precedence::None),
            (TokenKind::Err, None, None, Precedence::None),
            (TokenKind::Retn, None, None, Precedence::None),
            (
                TokenKind::Nil,
                Some(|c, _can_assign| Compiler::literal(c)),
                None,
                Precedence::None,
            ),
            (TokenKind::LeftBracket, None, None, Precedence::None),
            (TokenKind::RightBracket, None, None, Precedence::None),
            (
                TokenKind::LeftParen,
                Some(|c, _can_assign| Compiler::grouping(c)),
                None,
                Precedence::None,
            ),
            (TokenKind::RightParen, None, None, Precedence::None),
            (TokenKind::Comma, None, None, Precedence::None),
            (TokenKind::Period, None, None, Precedence::None),
            (TokenKind::ExclusiveRange, None, None, Precedence::None),
            (TokenKind::InclusiveRange, None, None, Precedence::None),
            (TokenKind::Assign, None, None, Precedence::None),
            (
                TokenKind::Not,
                Some(|c, _can_assign| Compiler::unary(c)),
                None,
                Precedence::None,
            ),
            (
                TokenKind::Plus,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Term,
            ),
            (
                TokenKind::Minus,
                Some(|c, _can_assign| Compiler::unary(c)),
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Term,
            ),
            (
                TokenKind::Multiply,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Factor,
            ),
            (
                TokenKind::Divide,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Factor,
            ),
            (
                TokenKind::Modulo,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
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
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Equality,
            ),
            (
                TokenKind::NotEqual,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Equality,
            ),
            (
                TokenKind::Less,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Comparison,
            ),
            (
                TokenKind::Greater,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Comparison,
            ),
            (
                TokenKind::LessEqual,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Comparison,
            ),
            (
                TokenKind::GreaterEqual,
                None,
                Some(|c, _can_assign| Compiler::binary(c)),
                Precedence::Comparison,
            ),
            (TokenKind::ShiftLeft, None, None, Precedence::None),
            (TokenKind::ShiftRight, None, None, Precedence::None),
            (
                TokenKind::Identifier,
                Some(|c, can_assign| Compiler::variable(c, can_assign)),
                None,
                Precedence::None,
            ),
            (
                TokenKind::True,
                Some(|c, _can_assign| Compiler::literal(c)),
                None,
                Precedence::None,
            ),
            (
                TokenKind::False,
                Some(|c, _can_assign| Compiler::literal(c)),
                None,
                Precedence::None,
            ),
            (TokenKind::Eof, None, None, Precedence::None),
            (
                TokenKind::String,
                Some(|c, _can_assign| Compiler::string(c)),
                None,
                Precedence::None,
            ),
            (
                TokenKind::Double(0.0),
                Some(|c, _can_assign| Compiler::number(c)),
                None,
                Precedence::None,
            ),
            (
                TokenKind::Integer(0),
                Some(|c, _can_assign| Compiler::number(c)),
                None,
                Precedence::None,
            ),
        ],
    };

    let mut errors = vec![];

    //compiler.advance().map_err(|e| vec![e])?;
    while !compiler.matches(TokenKind::Eof).map_err(|e| vec![e])? {
        if let Err(err) = compiler.declaration() {
            errors.push(err);
            compiler.output = false;
            break;
        }
    }

    if errors.is_empty() {
        Ok(compiler.chunk)
    } else {
        Err(errors)
    }
}

// TODO: We need some sort of error reporting struct
// right now, when we encounter an error, we return all the way back up to
// the main loop in compile where errors are collected sequentially.
// what would make more sense is to have a flag that says whether or not
// we should still be outputting bytecode, and still attempt to parse
// the entire program regardless.
impl<'a> Compiler<'a> {
    fn consume(&mut self, token: TokenKind) -> Result<(), PiccoloError> {
        if self.current().kind != token {
            self.advance()?;
            Err(PiccoloError::new(ErrorKind::UnexpectedToken {
                exp: format!("{:?}", token),
                got: format!("{}", self.previous()),
            })
            .line(self.previous().line))
        } else {
            self.advance()?;
            Ok(())
        }
    }

    fn matches(&mut self, kind: TokenKind) -> Result<bool, PiccoloError> {
        if !self.check(kind) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    fn advance(&mut self) -> Result<(), PiccoloError> {
        self.previous = self.current;
        self.current += 1;
        if self.current <= self.tokens.len() {
            Ok(())
        } else {
            Err(
                PiccoloError::new(ErrorKind::ExpectedExpression { got: "EOF".into() })
                    .line(self.previous().line),
            )
        }
    }

    fn current(&self) -> &Token<'a> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'a> {
        &self.tokens[self.current - 1]
    }

    fn declaration(&mut self) -> Result<(), PiccoloError> {
        // TODO: implement lookahead to change syntax back to :=
        if self.matches(TokenKind::Let)? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<(), PiccoloError> {
        let global = self.parse_variable()?;
        if self.matches(TokenKind::Assign)? {
            self.expression()?;
        } else {
            self.emit(Opcode::Nil);
        }
        self.define_variable(global);
        Ok(())
    }

    fn parse_variable(&mut self) -> Result<u16, PiccoloError> {
        self.consume(TokenKind::Identifier)?;
        Ok(self.identifier_constant())
    }

    fn define_variable(&mut self, var: u16) {
        self.emit3(Opcode::DefineGlobal, var);
    }

    fn statement(&mut self) -> Result<(), PiccoloError> {
        if self.matches(TokenKind::Retn)? {
            self.return_statement()?;
        } else {
            self.expression_statement()?;
        }
        self.assign = false;
        Ok(())
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
        self.advance()?;
        let can_assign = prec <= Precedence::Assignment;
        if let (Some(prefix), _, _) = self.get_rule(self.previous().kind) {
            prefix(self, can_assign)?;
        } else {
            return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                from: self.previous().lexeme.to_owned(),
            })
            .line(self.previous().line));
        }
        while prec <= self.get_rule(self.current().kind).2 {
            self.advance()?;
            if let (_, Some(infix), _) = self.get_rule(self.previous().kind) {
                infix(self, can_assign)?;
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
        let kind = self.previous().kind;
        self.precedence(Precedence::Unary)?;
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),
            TokenKind::Not => self.emit(Opcode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), PiccoloError> {
        let kind = self.previous().kind;
        let (_, _, prec) = self.get_rule(kind);
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
        let s = match self.previous().kind {
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

        if let Some(&idx) = self.strings.get(&s) {
            self.emit3(Opcode::Constant, idx);
        } else {
            let idx = self.chunk.make_constant(Value::String(s.clone()));
            self.emit3(Opcode::Constant, idx);
            self.strings.insert(s, idx);
        }
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), PiccoloError> {
        self.named_variable(can_assign)
    }

    fn named_variable(&mut self, can_assign: bool) -> Result<(), PiccoloError> {
        let arg = self.identifier_constant();
        if self.matches(TokenKind::Assign)? {
            if can_assign {
                if !self.assign {
                    self.assign = true;
                    self.expression()?;
                    self.emit3(Opcode::AssignGlobal, arg);
                } else {
                    return Err(PiccoloError::new(ErrorKind::ExpectedExpression {
                        got: String::from("= (Assignment is not an expression)"),
                    }));
                }
            } else {
                return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                    from: self.previous().lexeme.to_owned(),
                })
                .line(self.previous().line));
            }
        } else {
            self.emit3(Opcode::GetGlobal, arg);
        }
        Ok(())
    }

    fn identifier_constant(&mut self) -> u16 {
        if self.output {
            self.identifiers
                .get(self.previous().lexeme)
                .map(|idx| *idx)
                .unwrap_or_else(|| {
                    let idx = self
                        .chunk
                        .make_constant(Value::String(self.previous().lexeme.to_owned()));
                    self.identifiers.insert(self.previous().lexeme, idx);
                    idx
                })
        } else {
            0
        }
    }

    fn emit_constant(&mut self, c: Value) {
        if self.output {
            let c = self.chunk.make_constant(c);
            self.emit3(Opcode::Constant, c);
        }
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

    fn get_rule(&self, kind: TokenKind) -> (&Option<PrefixRule>, &Option<InfixRule>, Precedence) {
        for (k, prefix, infix, precedence) in self.rules.iter() {
            let rule = (prefix, infix, *precedence);
            if *k == kind {
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
