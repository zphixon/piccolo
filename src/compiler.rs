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

/// Compiles a list of tokens to Piccolo bytecode.
// TODO: scan on demand
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
    /// Parses and compiles a list of tokens. Uses a Pratt parser.
    pub fn compile(chunk: Chunk, s: &[Token]) -> crate::Result<Chunk> {
        let mut compiler = Compiler {
            chunk,
            previous: 0,
            current: 0,
            tokens: s,
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

        while !compiler.matches(TokenKind::Eof) {
            compiler.declaration()?;
        }
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

    fn declaration(&mut self) -> crate::Result<()> {
        if self.lookahead(1).kind == TokenKind::Declare {
            self.var_declaration()
        } else if self.lookahead(1).kind == TokenKind::Assign {
            self.var_assign()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> crate::Result<()> {
        let global = self.parse_variable()?;
        self.consume(TokenKind::Declare)?;
        self.expression()?;
        self.define_variable(global);
        Ok(())
    }

    fn var_assign(&mut self) -> crate::Result<()> {
        let global = self.parse_variable()?;
        self.consume(TokenKind::Assign)?;
        self.expression()?;
        self.set_variable(global);
        Ok(())
    }

    fn parse_variable(&mut self) -> crate::Result<usize> {
        self.consume(TokenKind::Identifier)?;
        Ok(self.identifier_constant(&self.tokens[self.previous]))
    }

    fn define_variable(&mut self, var: usize) {
        self.emit2(Opcode::DefineGlobal, var as u8);
    }

    fn set_variable(&mut self, var: usize) {
        self.emit2(Opcode::SetGlobal, var as u8);
    }

    fn statement(&mut self) -> crate::Result<()> {
        if self.matches(TokenKind::Retn) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.emit(Opcode::Return);
        Ok(())
    }

    fn expression_statement(&mut self) -> crate::Result<()> {
        self.expression()?;
        self.emit(Opcode::Pop);
        Ok(())
    }

    fn expression(&mut self) -> crate::Result<()> {
        self.precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn precedence(&mut self, prec: Precedence) -> crate::Result<()> {
        self.advance();
        if let (Some(prefix), _, _) = self.get_rule(&self.previous().kind) {
            prefix(self)?;
        } else {
            return Err(PiccoloError::MalformedExpression {
                from: self.previous().lexeme.to_owned(),
                line: self.previous().line,
            }
            .into());
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

    fn lookahead(&self, num: usize) -> &Token {
        &self.tokens[self.current + num]
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

    fn variable(&mut self) -> crate::Result<()> {
        self.named_variable(&self.tokens[self.previous])
    }

    fn named_variable(&mut self, token: &Token) -> crate::Result<()> {
        let arg = self.identifier_constant(token);
        if self.matches(TokenKind::Assign) {
            return Err(PiccoloError::MalformedExpression {
                from: "=".into(),
                line: self.previous().line,
            }
            .into());
        } else {
            self.emit2(Opcode::GetGlobal, arg as u8);
        }
        Ok(())
    }

    fn identifier_constant(&mut self, token: &Token) -> usize {
        self.chunk
            .make_constant(Value::String(token.lexeme.to_owned()))
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
