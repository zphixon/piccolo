use crate::chunk::Chunk;
use crate::error::PiccoloError;
use crate::op::Opcode;
use crate::scanner::{Token, TokenKind};
use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub(crate) enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

pub struct Compiler<'a> {
    chunk: Chunk,
    previous: usize,
    current: usize,
    tokens: &'a [Token<'a>],
    rules: &'a [(
        TokenKind,
        Option<fn(&mut Compiler) -> crate::Result<()>>,
        Option<fn(&mut Compiler) -> crate::Result<()>>,
        Precedence,
    )],
}

impl<'a> Compiler<'a> {
    pub fn compile(chunk: Chunk, s: &[Token]) -> crate::Result<Chunk> {
        let mut compiler = Compiler {
            chunk,
            previous: 0,
            current: 0,
            tokens: s,
            rules: &[
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
                (TokenKind::Newline, None, None, Precedence::None),
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
                (TokenKind::And, None, None, Precedence::None),
                (TokenKind::Or, None, None, Precedence::None),
                (TokenKind::BitwiseAnd, None, None, Precedence::None),
                (TokenKind::BitwiseOr, None, None, Precedence::None),
                (TokenKind::BitwiseXor, None, None, Precedence::None),
                (
                    TokenKind::Equals,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Equality,
                ),
                (
                    TokenKind::NotEquals,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Equality,
                ),
                (
                    TokenKind::LessThan,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::GreaterThan,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::LessThanEquals,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::GreaterThanEquals,
                    None,
                    Some(|c| Compiler::binary(c)),
                    Precedence::Comparison,
                ),
                (TokenKind::ShiftLeft, None, None, Precedence::None),
                (TokenKind::ShiftRight, None, None, Precedence::None),
                (TokenKind::Identifier, None, None, Precedence::None),
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
                    TokenKind::String(String::new()),
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

        //compiler.advance();
        compiler.expression()?;
        compiler.consume(TokenKind::Eof)?;
        compiler.emit(Opcode::Return);
        Ok(compiler.chunk)
    }

    fn consume(&mut self, token: TokenKind) -> crate::Result<()> {
        if self.current().kind != token {
            Err(PiccoloError::UnexpectedToken {
                exp: format!("{:?}", token),
                got: format!("{}", self.current()),
                line: self.previous().line,
            }
            .into())
        } else {
            self.advance();
            Ok(())
        }
    }

    fn expression(&mut self) -> crate::Result<()> {
        self.precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn precedence(&mut self, prec: Precedence) -> crate::Result<()> {
        self.advance();
        if let (Some(prefix), _, _) = self.get_rule(&self.previous().kind) {
            prefix(self)?;
        }
        while prec <= *self.get_rule(&self.current().kind).2 {
            self.advance();
            if let (_, Some(infix), _) = self.get_rule(&self.previous().kind) {
                infix(self)?;
            }
        }
        Ok(())
    }

    fn grouping(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.consume(TokenKind::RightParen)
    }

    fn unary(&mut self) -> crate::Result<()> {
        let kind = self.previous().kind.clone();
        self.precedence(Precedence::Unary)?;
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),
            TokenKind::Not => self.emit(Opcode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> crate::Result<()> {
        let kind = self.previous().kind.clone();
        let (_, _, &prec) = self.get_rule(&kind);
        self.precedence(prec)?;
        match kind {
            TokenKind::Plus => self.emit(Opcode::Add),
            TokenKind::Minus => self.emit(Opcode::Subtract),
            TokenKind::Divide => self.emit(Opcode::Divide),
            TokenKind::Multiply => self.emit(Opcode::Multiply),
            TokenKind::Equals => self.emit(Opcode::Equal),
            TokenKind::NotEquals => self.emit2(Opcode::Equal, Opcode::Not),
            TokenKind::GreaterThan => self.emit(Opcode::Greater),
            TokenKind::GreaterThanEquals => self.emit2(Opcode::Less, Opcode::Not),
            TokenKind::LessThan => self.emit(Opcode::Less),
            TokenKind::LessThanEquals => self.emit2(Opcode::Greater, Opcode::Not),
            _ => unreachable!(),
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

    fn number(&mut self) -> crate::Result<()> {
        if let Ok(value) = self.previous().lexeme.parse::<i64>() {
            self.emit_constant(Value::Integer(value));
            Ok(())
        } else if let Ok(value) = self.previous().lexeme.parse::<f64>() {
            self.emit_constant(Value::Double(value));
            Ok(())
        } else {
            Err(PiccoloError::InvalidNumberLiteral {
                line: self.previous().line,
                literal: self.previous().lexeme.to_owned(),
            }
            .into())
        }
    }

    fn literal(&mut self) -> crate::Result<()> {
        match self.previous().kind {
            TokenKind::Nil => self.emit(Opcode::Nil),
            TokenKind::True => self.emit(Opcode::True),
            TokenKind::False => self.emit(Opcode::False),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn string(&mut self) -> crate::Result<()> {
        let s = match &self.previous().kind {
            TokenKind::String(s) => s.clone(),
            _ => unreachable!(),
        };
        self.emit_constant(Value::String(s));
        Ok(())
    }

    fn emit_constant(&mut self, c: Value) {
        let c = self.chunk.make_constant(c);
        self.emit2(Opcode::Constant, c as u8);
    }

    fn emit<T: Into<u8>>(&mut self, byte: T) {
        self.chunk.write(byte, self.previous().line);
    }

    fn emit2<T: Into<u8>, U: Into<u8>>(&mut self, byte1: T, byte2: U) {
        self.chunk.write(byte1, self.previous().line);
        self.chunk.write(byte2, self.previous().line);
    }

    fn get_rule(
        &'a self,
        kind: &TokenKind,
    ) -> (
        &'a Option<fn(&mut Compiler) -> crate::Result<()>>,
        &'a Option<fn(&mut Compiler) -> crate::Result<()>>,
        &'a Precedence,
    ) {
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

                if let TokenKind::String(_) = kind {
                    if let TokenKind::String(_) = k {
                        return rule;
                    }
                }
            }
        }

        panic!("no rule for {:?}", kind);
    }
}
