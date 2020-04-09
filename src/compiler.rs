use crate::chunk::Chunk;
use crate::error::{ErrorKind, PiccoloError};
use crate::op::Opcode;
use crate::scanner::{Token, TokenKind};
use crate::value::Value;

use crate::Scanner;
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

/// Compiles a list of tokens to Piccolo bytecode.
// TODO: scan on demand
pub struct Compiler<'a> {
    chunk: Chunk,
    output: bool,
    scanner: Scanner<'a>,
    identifiers: HashMap<&'a str, u16>,
    strings: HashMap<String, u16>,
    //tokens: &'a [Token<'a>],
    rules: Vec<(TokenKind, Option<PrefixRule>, Option<InfixRule>, Precedence)>,
}

type PrefixRule = fn(&mut Compiler, bool) -> Result<(), PiccoloError>;
type InfixRule = fn(&mut Compiler, bool) -> Result<(), PiccoloError>;

pub fn compile(chunk: Chunk, scanner: Scanner) -> Result<Chunk, Vec<PiccoloError>> {
    let mut compiler = Compiler {
        chunk,
        output: true,
        scanner,
        identifiers: HashMap::new(),
        strings: HashMap::new(),
        //tokens: s,
        rules: vec![
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
            (
                TokenKind::Assign,
                None, None,
                //Some(|c| Compiler::var_assign(c)),
                Precedence::Primary,
            ),
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

    //compiler.scanner.next_token();
    compiler.advance();
    while !compiler.matches(TokenKind::Eof) {
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
    /// Parses and compiles a list of tokens. Uses a Pratt parser.
    fn consume(&mut self, token: TokenKind) -> Result<(), PiccoloError> {
        if self.scanner.current().kind != token {
            self.advance();
            Err(PiccoloError::new(ErrorKind::UnexpectedToken {
                exp: format!("{:?}", token),
                got: format!("{}", self.scanner.previous()),
            })
            .line(self.scanner.previous().line))
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
        self.scanner.current().kind == kind
    }

    fn declaration(&mut self) -> Result<(), PiccoloError> {
        // if self.can_lookahead(1) && self.lookahead(1).kind == TokenKind::Declare {
        //     self.var_declaration()
        // } else if self.can_lookahead(1) && self.lookahead(1).kind == TokenKind::Assign {
        //     self.var_assign()
        // } else {
        if self.matches(TokenKind::Let) {
            self.var_declaration()
        } else {
            self.statement()
        }
        // }
    }

    fn var_declaration(&mut self) -> Result<(), PiccoloError> {
        let global = self.parse_variable()?;
        if self.matches(TokenKind::Assign) {
            self.expression()?;
        } else {
            self.emit(Opcode::Nil);
        }
        self.define_variable(global);
        Ok(())
    }

    fn parse_variable(&mut self) -> Result<u16, PiccoloError> {
        self.consume(TokenKind::Identifier)?;
        Ok(self.identifier_constant(&self.scanner.previous().clone()))
    }

    fn define_variable(&mut self, var: u16) {
        self.emit3(Opcode::DefineGlobal, var);
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
                got: self.scanner.current().lexeme.to_owned(),
            })
            .line(self.scanner.previous().line))
        } else {
            self.precedence(Precedence::Assignment)
        }
    }

    fn precedence(&mut self, prec: Precedence) -> Result<(), PiccoloError> {
        self.advance();
        let can_assign = prec <= Precedence::Assignment;
        if let (Some(prefix), _, _) = self.get_rule(&self.scanner.previous().kind) {
            prefix(self, can_assign)?;
        } else {
            return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                from: self.scanner.previous().lexeme.to_owned(),
            })
            .line(self.scanner.previous().line));
        }
        while prec <= *self.get_rule(&self.scanner.current().kind).2 {
            self.advance();
            if let (_, Some(infix), _) = self.get_rule(&self.scanner.previous().kind) {
                infix(self, can_assign)?;
            } else {
                panic!("no infix rule for {:?}", self.scanner.previous().kind);
            }
        }
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), PiccoloError> {
        self.expression()?;
        self.consume(TokenKind::RightParen)
    }

    fn unary(&mut self) -> Result<(), PiccoloError> {
        let kind = self.scanner.previous().kind.clone();
        self.precedence(Precedence::Unary)?;
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),
            TokenKind::Not => self.emit(Opcode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), PiccoloError> {
        let kind = self.scanner.previous().kind.clone();
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
        // TODO: propagate
        self.scanner.next_token().unwrap();
    }

    fn number(&mut self) -> Result<(), PiccoloError> {
        if let Ok(value) = self.scanner.previous().lexeme.parse::<i64>() {
            self.emit_constant(Value::Integer(value));
            Ok(())
        } else if let Ok(value) = self.scanner.previous().lexeme.parse::<f64>() {
            self.emit_constant(Value::Double(value));
            Ok(())
        } else {
            Err(PiccoloError::new(ErrorKind::InvalidNumberLiteral {
                literal: self.scanner.previous().lexeme.to_owned(),
            })
            .line(self.scanner.previous().line))
        }
    }

    fn literal(&mut self) -> Result<(), PiccoloError> {
        match self.scanner.previous().kind {
            TokenKind::Nil => self.emit(Opcode::Nil),
            TokenKind::True => self.emit(Opcode::True),
            TokenKind::False => self.emit(Opcode::False),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn string(&mut self) -> Result<(), PiccoloError> {
        let s = match &self.scanner.previous().kind {
            TokenKind::String => {
                let s = self.scanner.previous().lexeme;
                let mut value = Vec::new();
                let line_start = self.scanner.previous().line;
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
        self.named_variable(&self.scanner.previous().clone(), can_assign)
    }

    fn named_variable<'b>(&'b mut self, token: &Token<'a>, can_assign: bool) -> Result<(), PiccoloError> {
        let arg = self.identifier_constant(token);
        if self.matches(TokenKind::Assign) {
            if can_assign {
                self.expression()?;
                self.emit3(Opcode::AssignGlobal, arg);
            } else {
                return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                    from: token.lexeme.to_owned(),
                }).line(token.line));
            }
        } else {
            self.emit3(Opcode::GetGlobal, arg);
        }
        Ok(())
    }

    fn identifier_constant<'b>(&'b mut self, token: &Token<'a>) -> u16 {
        if self.output {
            self.identifiers
                .get(token.lexeme)
                .map(|idx| *idx)
                .unwrap_or_else(|| {
                    let idx = self
                        .chunk
                        .make_constant(Value::String(token.lexeme.to_owned()));
                    self.identifiers.insert(token.lexeme, idx);
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
            self.chunk.write(byte, self.scanner.previous().line);
        }
    }

    fn emit2<T: Into<u8>, U: Into<u8>>(&mut self, byte1: T, byte2: U) {
        if self.output {
            self.chunk.write(byte1, self.scanner.previous().line);
            self.chunk.write(byte2, self.scanner.previous().line);
        }
    }

    fn emit3<T: Into<u8>, U: Into<u16>>(&mut self, byte1: T, bytes: U) {
        if self.output {
            let (low, high) = crate::decode_bytes(bytes.into());
            self.chunk.write(byte1, self.scanner.previous().line);
            self.chunk.write(low, self.scanner.previous().line);
            self.chunk.write(high, self.scanner.previous().line);
        }
    }

    fn get_rule(
        &self,
        kind: &TokenKind,
    ) -> (
        &Option<PrefixRule>,
        &Option<InfixRule>,
        &Precedence,
    ) {
        for (k, prefix, infix, precedence) in self.rules.iter() {
            let rule = (prefix, infix, precedence);
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
