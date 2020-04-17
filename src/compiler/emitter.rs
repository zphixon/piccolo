use crate::runtime::op::Opcode;
use crate::{Chunk, ErrorKind, PiccoloError, Value};

use super::{Token, TokenKind};

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

/// Struct that ad-hoc parses and compiles Piccolo source code into a Chunk.
/// This will be replaced by a more formal parser and AST-walking compiler.
pub struct Emitter<'a> {
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

type PrefixRule = fn(&mut Emitter, bool) -> Result<(), PiccoloError>;
type InfixRule = fn(&mut Emitter, bool) -> Result<(), PiccoloError>;

// TODO: We need some sort of error reporting struct
// right now, when we encounter an error, we return all the way back up to
// the main loop in compile where errors are collected sequentially.
// what would make more sense is to have a flag that says whether or not
// we should still be outputting bytecode, and still attempt to parse
// the entire program regardless.
impl<'a> Emitter<'a> {
    pub fn new(chunk: Chunk, tokens: &'a [Token<'a>]) -> Emitter<'a> {
        Emitter {
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
                (TokenKind::Assert, None, None, Precedence::None),
                (
                    TokenKind::Nil,
                    Some(|c, _can_assign| Emitter::literal(c)),
                    None,
                    Precedence::None,
                ),
                (TokenKind::LeftBracket, None, None, Precedence::None),
                (TokenKind::RightBracket, None, None, Precedence::None),
                (
                    TokenKind::LeftParen,
                    Some(|c, _can_assign| Emitter::grouping(c)),
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
                    Some(|c, _can_assign| Emitter::unary(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::Plus,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Term,
                ),
                (
                    TokenKind::Minus,
                    Some(|c, _can_assign| Emitter::unary(c)),
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Term,
                ),
                (
                    TokenKind::Multiply,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Factor,
                ),
                (
                    TokenKind::Divide,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Factor,
                ),
                (
                    TokenKind::Modulo,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
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
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Equality,
                ),
                (
                    TokenKind::NotEqual,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Equality,
                ),
                (
                    TokenKind::Less,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::Greater,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::LessEqual,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Comparison,
                ),
                (
                    TokenKind::GreaterEqual,
                    None,
                    Some(|c, _can_assign| Emitter::binary(c)),
                    Precedence::Comparison,
                ),
                (TokenKind::ShiftLeft, None, None, Precedence::None),
                (TokenKind::ShiftRight, None, None, Precedence::None),
                (
                    TokenKind::Identifier,
                    Some(|c, can_assign| Emitter::variable(c, can_assign)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::True,
                    Some(|c, _can_assign| Emitter::literal(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::False,
                    Some(|c, _can_assign| Emitter::literal(c)),
                    None,
                    Precedence::None,
                ),
                (TokenKind::Eof, None, None, Precedence::None),
                (
                    TokenKind::String,
                    Some(|c, _can_assign| Emitter::string(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::Double(0.0),
                    Some(|c, _can_assign| Emitter::number(c)),
                    None,
                    Precedence::None,
                ),
                (
                    TokenKind::Integer(0),
                    Some(|c, _can_assign| Emitter::number(c)),
                    None,
                    Precedence::None,
                ),
            ],
        }
    }

    pub(super) fn stop_output(&mut self) {
        self.output = false;
    }

    pub(super) fn chunk(self) -> Chunk {
        self.chunk
    }

    // consumes one token, reports an error if it isn't the token we wanted
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

    // checks if the next token matches, advances if it does
    pub(super) fn matches(&mut self, kind: TokenKind) -> Result<bool, PiccoloError> {
        if !self.check(kind) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    // peeks at the next token
    fn check(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    // unconditionally advances, except at EOF
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

    pub(super) fn declaration(&mut self) -> Result<(), PiccoloError> {
        // TODO: implement lookahead to change syntax back to :=
        if self.matches(TokenKind::Let)? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    // declaration always is a statement, it will never be entered from the parsing table
    fn var_declaration(&mut self) -> Result<(), PiccoloError> {
        let global = self.parse_variable()?;
        if self.matches(TokenKind::Assign)? {
            self.expression()?;
        } else {
            // assign nil to a variable if no value is specified
            self.emit(Opcode::Nil);
        }
        self.define_variable(global);
        Ok(())
    }

    // parses a variable reference
    fn parse_variable(&mut self) -> Result<u16, PiccoloError> {
        self.consume(TokenKind::Identifier)?;
        Ok(self.identifier_constant())
    }

    fn define_variable(&mut self, var: u16) {
        self.emit3(Opcode::DefineGlobal, var);
    }

    // matches the beginning of statements, e.g. data, fn, retn, declaration
    fn statement(&mut self) -> Result<(), PiccoloError> {
        if self.matches(TokenKind::Retn)? {
            self.return_statement()?;
        } else if self.matches(TokenKind::Assert)? {
            self.assert_statement()?;
        // } else if matches identifier {
        // if matches assign or declare
        //     make variable
        // else reverse, expression statement
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

    fn assert_statement(&mut self) -> Result<(), PiccoloError> {
        self.expression()?;
        self.emit(Opcode::Assert);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), PiccoloError> {
        self.expression()?;
        self.emit(Opcode::Pop);
        Ok(())
    }

    // matches an expression, returns an error if at EOF
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

    // parse an expression by operators of decreasing precedence starting from prec
    fn precedence(&mut self, prec: Precedence) -> Result<(), PiccoloError> {
        self.advance()?;

        // we can do assignment if there aren't any tokens to the left with a higher
        // precedence than assignment. this is only the case if we are currently to
        // the right of any operand other than assignment itself.
        let can_assign = prec <= Precedence::Assignment;
        //let can_assign = prec <= Precedence::Or; // TODO

        if let (Some(prefix), _, _) = self.get_rule(self.previous().kind) {
            // if there exists a prefix rule for the previous token, we execute it.
            // this will always be an operator like logical/mathematical/bitwise negation,
            // string/number literals, or variable references.
            prefix(self, can_assign)?;
        } else {
            return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                from: self.previous().lexeme.to_owned(),
            })
            .line(self.previous().line));
        }

        // recurse while there is a token with higher precedence than the current, return if not
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

    // parses parenthetical expressions
    fn grouping(&mut self) -> Result<(), PiccoloError> {
        // we've already advanced past the left paren in `precedence`,
        // now we're in the prefix rule for TokenKind::LeftParen
        self.expression()?;
        self.consume(TokenKind::RightParen)
    }

    fn unary(&mut self) -> Result<(), PiccoloError> {
        // we've already advanced past the unary operator, we're here
        // from the call to `prefix` in `precedence`
        let kind = self.previous().kind;

        // parse any operators to the right with a higher precedence and emit their opcode
        self.precedence(Precedence::Unary)?;

        // emit the correct opcode
        match kind {
            TokenKind::Minus => self.emit(Opcode::Negate),
            TokenKind::Not => self.emit(Opcode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), PiccoloError> {
        // we've already advanced past the left operand and the operator
        // the call to `infix` brought us here
        let kind = self.previous().kind;
        let (_, _, prec) = self.get_rule(kind);

        // parse operators to the right with the next higher precedence and emit their opcodes
        self.precedence((prec as u8 + 1).into())?;

        // emit our own opcode
        match kind {
            TokenKind::Plus => self.emit(Opcode::Add),
            TokenKind::Minus => self.emit(Opcode::Subtract),
            TokenKind::Divide => self.emit(Opcode::Divide),
            TokenKind::Multiply => self.emit(Opcode::Multiply),
            TokenKind::Equal => self.emit(Opcode::Equal),
            TokenKind::NotEqual => self.emit2(Opcode::Equal, Opcode::Not),
            TokenKind::Greater => self.emit(Opcode::Greater),
            TokenKind::GreaterEqual => self.emit(Opcode::GreaterEqual),
            TokenKind::Less => self.emit(Opcode::Less),
            TokenKind::LessEqual => self.emit(Opcode::LessEqual),
            _ => {}
        }
        Ok(())
    }

    fn number(&mut self) -> Result<(), PiccoloError> {
        if let TokenKind::Integer(value) = self.previous().kind {
            self.emit_constant(Value::Integer(value));
        } else if let TokenKind::Double(value) = self.previous().kind {
            self.emit_constant(Value::Double(value));
        } else {
            unreachable!("number literal that isn't a number");
        }

        Ok(())
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

    // parses string escapes
    fn string(&mut self) -> Result<(), PiccoloError> {
        let s = super::escape_string(self.previous())?;

        // intern the string constant
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

    // parse variable assignment and reference
    fn named_variable(&mut self, can_assign: bool) -> Result<(), PiccoloError> {
        let arg = self.identifier_constant();
        if self.matches(TokenKind::Assign)? {
            // if the target is valid (say, setting a property on an instance)
            if can_assign {
                // TODO: maybe unnecessary if we check against a precedence <= or in `precedence`
                if !self.assign {
                    self.assign = true;
                    self.expression()?;
                    self.emit3(Opcode::AssignGlobal, arg);
                } else {
                    return Err(PiccoloError::new(ErrorKind::ExpectedExpression {
                        got: String::from("= (Assignment is not an expression)"),
                    })
                    .line(self.previous().line));
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

    // emit identifier constant
    fn identifier_constant(&mut self) -> u16 {
        if self.output {
            // intern identifier constants - we might want to unify self.identifiers
            // and self.strings just to save a hash table
            self.identifiers
                .get(self.previous().lexeme)
                .copied()
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
