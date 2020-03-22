use crate::chunk::Chunk;
use crate::error::PiccoloError;
use crate::error::PiccoloError::UnexpectedToken;
use crate::op::Opcode;
use crate::scanner::{Token, TokenKind};
use crate::value::Value;
use crate::rules::{get_rule, Precedence};

pub struct Compiler<'a> {
    chunk: Chunk,
    previous: &'a Token<'a>,
    current: &'a Token<'a>,
}

impl<'a> Compiler<'a> {
    pub fn compile(chunk: Chunk, s: Vec<Token>) -> crate::Result<Chunk> {
        let mut compiler = Compiler {
            chunk,
            previous: &s[0],
            current: &s[0],
        };

        compiler.advance();
        compiler.expression()?;
        compiler.consume(TokenKind::Eof)?;
        compiler.emit(Opcode::Return);
        Ok(compiler.chunk)
    }

    pub fn consume(&mut self, token: TokenKind) -> crate::Result<()> {
        if *self.current.kind() != token {
            Err(PiccoloError::UnexpectedToken {
                exp: format!("{:?}", token),
                got: format!("{:?}", self.current.kind()),
                line: self.previous.line(),
            }
            .into())
        } else {
            self.advance();
            Ok(())
        }
    }

    pub fn expression(&mut self) -> crate::Result<()> {
        self.precedence(Precedence::Assignment)?;
        Ok(())
    }

    pub(crate) fn precedence(&mut self, prec: Precedence) -> crate::Result<()> {
        Ok(())
    }

    pub fn grouping(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.consume(TokenKind::LeftParen)
    }

    pub fn unary(&mut self) -> crate::Result<()> {
        let kind = self.previous.kind();
        self.precedence(Precedence::Unary)?;
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),
            TokenKind::Not => unimplemented!(),
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn binary(&mut self) -> crate::Result<()> {
        let kind = self.previous.kind();
        let rule = get_rule(kind);
        self.precedence(rule.precedence)?;
        match kind {
            TokenKind::Plus => self.emit(Opcode::Add),
            TokenKind::Minus => self.emit(Opcode::Subtract),
            TokenKind::Divide => self.emit(Opcode::Divide),
            TokenKind::Multiply => self.emit(Opcode::Multiply),
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn advance(&mut self) {
        self.previous = self.current;
        while self.scan_token().is_err() {}
    }

    pub fn scan_token(&mut self) -> crate::Result<()> {
        Ok(())
    }

    pub fn number(&mut self) -> crate::Result<()> {
        let value = self
            .previous
            .lexeme()
            .parse::<f32>()
            .expect("incorrect number format: file a bug report");
        self.emit_constant(Value(value));
        Ok(())
    }

    pub fn emit_constant(&mut self, c: Value) {
        let c = self.chunk.constant(c);
        self.emit2(Opcode::Constant, c as u8);
    }

    pub fn emit<T: Into<u8>>(&mut self, byte: T) {
        self.chunk.write(byte, self.previous.line());
    }

    pub fn emit2<T: Into<u8>, U: Into<u8>>(&mut self, byte1: T, byte2: U) {
        self.chunk.write(byte1, self.previous.line());
        self.chunk.write(byte2, self.previous.line());
    }
}
