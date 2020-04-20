use crate::ast::expr::{Expr, ExprAccept, ExprVisitor};
use crate::ast::stmt::{Stmt, StmtAccept, StmtVisitor};
use crate::ast::Arity;
use crate::runtime::op::Opcode;
use crate::{Chunk, ErrorKind, PiccoloError, Token, TokenKind, Value};

use std::collections::HashMap;

pub struct Emitter {
    chunk: Chunk,
    strings: HashMap<String, u16>,
    scope_depth: u16,
    locals: Vec<(String, u16)>,
}

impl Emitter {
    pub fn new(chunk: Chunk) -> Self {
        Emitter {
            chunk,
            strings: HashMap::new(),
            scope_depth: 0,
            locals: Vec::new(),
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn compile(&mut self, stmts: &[Stmt]) -> Result<Chunk, Vec<PiccoloError>> {
        let mut errs = Vec::new();
        for stmt in stmts {
            match stmt.accept(self) {
                Ok(_) => {}
                Err(e) => errs.push(e),
            }
        }

        if errs.is_empty() {
            Ok(std::mem::take(&mut self.chunk))
        } else {
            Err(errs)
        }
    }

    fn resolve_local(&self, name: &str) -> Option<u16> {
        for (i, (k, _)) in self.locals.iter().enumerate().rev() {
            if k == name {
                return Some(i as u16);
            }
        }
        None
    }
}

impl ExprVisitor for Emitter {
    type Output = Result<(), PiccoloError>;

    fn visit_atom(&mut self, token: &Token) -> Self::Output {
        let i = if token.kind == TokenKind::String && self.strings.contains_key(token.lexeme) {
            *self.strings.get(token.lexeme).unwrap()
        } else {
            let i = self
                .chunk
                .make_constant(Value::try_from(token.clone()).unwrap());
            self.strings.insert(token.lexeme.to_string(), i);
            i
        };

        self.chunk.write_arg_u16(Opcode::Constant, i, token.line);

        Ok(())
    }

    fn visit_paren(&mut self, value: &Expr) -> Self::Output {
        value.accept(self)
    }

    fn visit_variable(&mut self, name: &Token) -> Self::Output {
        match self.resolve_local(name.lexeme) {
            Some(idx) => self.chunk.write_arg_u16(Opcode::GetLocal, idx, name.line),
            None => {
                let i = if self.strings.contains_key(name.lexeme) {
                    *self.strings.get(name.lexeme).unwrap()
                } else {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.lexeme.to_owned(),
                    })
                    .line(name.line));
                };

                self.chunk.write_arg_u16(Opcode::GetGlobal, i, name.line);
            }
        }
        Ok(())
    }

    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> Self::Output {
        rhs.accept(self)?;
        match op.kind {
            TokenKind::Not => self.chunk.write_u8(Opcode::Not, op.line),
            TokenKind::Minus => self.chunk.write_u8(Opcode::Negate, op.line),
            _ => unreachable!("unrecognized unary op {:?}", op),
        }
        Ok(())
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        lhs.accept(self)?;
        rhs.accept(self)?;

        match op.kind {
            TokenKind::Plus => self.chunk.write_u8(Opcode::Add, 1),
            TokenKind::Minus => self.chunk.write_u8(Opcode::Subtract, 1),
            TokenKind::Divide => self.chunk.write_u8(Opcode::Divide, 1),
            TokenKind::Multiply => self.chunk.write_u8(Opcode::Multiply, 1),
            TokenKind::Equal => self.chunk.write_u8(Opcode::Equal, 1),
            TokenKind::NotEqual => {
                self.chunk.write_u8(Opcode::Equal, 1);
                self.chunk.write_u8(Opcode::Not, 1);
            }
            TokenKind::Greater => self.chunk.write_u8(Opcode::Greater, 1),
            TokenKind::GreaterEqual => self.chunk.write_u8(Opcode::GreaterEqual, 1),
            TokenKind::Less => self.chunk.write_u8(Opcode::Less, 1),
            TokenKind::LessEqual => self.chunk.write_u8(Opcode::LessEqual, 1),
            _ => unreachable!("unrecognized binary op {:?}", op),
        }

        Ok(())
    }

    fn visit_assign(&mut self, _name: &Token, _value: &Expr) -> Self::Output {
        unimplemented!("visit_assign")
    }

    fn visit_logical(&mut self, lhs: &Expr, _op: &Token, rhs: &Expr) -> Self::Output {
        lhs.accept(self)?;
        rhs.accept(self)
    }

    fn visit_call(
        &mut self,
        _callee: &Expr,
        _paren: &Token,
        _arity: Arity,
        _args: &[Expr],
    ) -> Self::Output {
        unimplemented!("visit_call")
    }

    fn visit_new(&mut self, _name: &Token, _args: &[(Token, Box<Expr>)]) -> Self::Output {
        unimplemented!("visit_new")
    }

    fn visit_get(&mut self, _object: &Expr, _name: &Token) -> Self::Output {
        unimplemented!("visit_get")
    }

    fn visit_set(&mut self, _object: &Expr, _name: &Token, _value: &Expr) -> Self::Output {
        unimplemented!("visit_set")
    }

    fn visit_index(&mut self, _rb: &Token, _object: &Expr, _idx: &Expr) -> Self::Output {
        unimplemented!("visit_index")
    }

    fn visit_func(
        &mut self,
        _name: &Token,
        _args: &[Token],
        _arity: Arity,
        _body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        unimplemented!("visit_func")
    }
}

impl StmtVisitor for Emitter {
    type Output = Result<(), PiccoloError>;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        expr.accept(self)?;
        self.chunk.write_u8(Opcode::Pop, 1); // TODO: line
        Ok(())
    }

    fn visit_block(&mut self, body: &[Stmt]) -> Self::Output {
        self.scope_depth += 1;
        for stmt in body {
            stmt.accept(self)?;
        }
        self.scope_depth -= 1;
        while self.locals.len() > 0 && self.locals[self.locals.len() - 1].1 > self.scope_depth {
            self.chunk.write_u8(Opcode::Pop, 4);
            self.locals.pop().unwrap();
        }
        Ok(())
    }

    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output {
        value.accept(self)?;
        if self.scope_depth > 0 {
            if op.kind == TokenKind::Assign {
                match self.resolve_local(name.lexeme) {
                    Some(idx) => self.chunk.write_arg_u16(Opcode::AssignLocal, idx, op.line),
                    None => {
                        let i = if self.strings.contains_key(name.lexeme) {
                            *self.strings.get(name.lexeme).unwrap()
                        } else {
                            return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                                name: name.lexeme.to_owned(),
                            })
                            .line(name.line));
                        };
                        self.chunk.write_arg_u16(Opcode::AssignGlobal, i, name.line);
                    }
                }
            } else if op.kind == TokenKind::Declare {
                self.locals.push((name.lexeme.to_owned(), self.scope_depth));
            }
        } else if op.kind == TokenKind::Assign {
            let idx = self
                .chunk
                .make_constant(Value::String(name.lexeme.to_owned()));
            self.chunk
                .write_arg_u16(Opcode::AssignGlobal, idx, name.line);
        } else if op.kind == TokenKind::Declare {
            let idx = self
                .chunk
                .make_constant(Value::String(name.lexeme.to_owned()));
            self.strings.insert(name.lexeme.to_owned(), idx);
            self.chunk
                .write_arg_u16(Opcode::DeclareGlobal, idx, name.line);
        }
        Ok(())
    }

    fn visit_if(
        &mut self,
        _cond: &Expr,
        _then: &[Stmt],
        _else_: Option<&Vec<Stmt>>,
    ) -> Self::Output {
        unimplemented!("visit_if")
    }

    fn visit_while(&mut self, _cond: &Expr, _body: &[Stmt]) -> Self::Output {
        unimplemented!("visit_while")
    }

    fn visit_for(&mut self, _name: &Token, _iter: &Expr, _body: &[Stmt]) -> Self::Output {
        unimplemented!("visit_for")
    }

    fn visit_func(
        &mut self,
        _name: &Token,
        _args: &[Token],
        _arity: Arity,
        _body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        unimplemented!("visit_func")
    }

    fn visit_retn(&mut self, keyword: &Token, value: Option<&Expr>) -> Self::Output {
        if let Some(expr) = value {
            expr.accept(self)?;
        }
        self.chunk.write_u8(Opcode::Return, keyword.line);
        Ok(())
    }

    fn visit_assert(&mut self, keyword: &Token, value: &Expr) -> Self::Output {
        value.accept(self)?;
        self.chunk.write_u8(Opcode::Assert, keyword.line);
        Ok(())
    }

    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[Stmt],
        _fields: &[(Token, Expr)],
    ) -> Self::Output {
        unimplemented!("visit_data")
    }
}
