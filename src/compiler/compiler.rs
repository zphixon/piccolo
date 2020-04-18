use crate::ast::expr::{Expr, ExprAccept, ExprVisitor};
use crate::ast::stmt::{Stmt, StmtAccept, StmtVisitor};
use crate::ast::Arity;
use crate::runtime::op::Opcode;
use crate::{Chunk, PiccoloError, Token, TokenKind, Value};

pub struct Compiler(pub Chunk);

impl Compiler {
    pub fn compile(&mut self, stmts: &[Stmt]) -> Result<Chunk, Vec<PiccoloError>> {
        let mut errs = vec![];
        for stmt in stmts {
            match stmt.accept(self) {
                Ok(_) => {}
                Err(e) => errs.push(e),
            }
        }

        if errs.is_empty() {
            Ok(std::mem::take(&mut self.0))
        } else {
            Err(errs)
        }
    }
}

impl ExprVisitor for Compiler {
    type Output = Result<(), PiccoloError>;

    fn visit_value(&mut self, value: &Value) -> Self::Output {
        let i = self.0.make_constant(value.try_clone().unwrap());
        let (low, high) = crate::decode_bytes(i);

        self.0.write(Opcode::Constant, 1);
        self.0.write(low, 1);
        self.0.write(high, 1);

        Ok(())
    }

    fn visit_unary(&mut self, _op: &Token, rhs: &Expr) -> Self::Output {
        rhs.accept(self)
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        lhs.accept(self)?;
        rhs.accept(self)?;

        match op.kind {
            TokenKind::Plus => self.0.write(Opcode::Add, 1),
            TokenKind::Minus => self.0.write(Opcode::Subtract, 1),
            TokenKind::Divide => self.0.write(Opcode::Divide, 1),
            TokenKind::Multiply => self.0.write(Opcode::Multiply, 1),
            TokenKind::Equal => self.0.write(Opcode::Equal, 1),
            TokenKind::NotEqual => {
                self.0.write(Opcode::Equal, 1);
                self.0.write(Opcode::Not, 1);
            }
            TokenKind::Greater => self.0.write(Opcode::Greater, 1),
            TokenKind::GreaterEqual => self.0.write(Opcode::GreaterEqual, 1),
            TokenKind::Less => self.0.write(Opcode::Less, 1),
            TokenKind::LessEqual => self.0.write(Opcode::LessEqual, 1),
            _ => {}
        }

        Ok(())
    }

    fn visit_paren(&mut self, value: &Expr) -> Self::Output {
        value.accept(self)
    }

    fn visit_variable(&mut self, _name: &Token) -> Self::Output {
        unimplemented!("visit_variable")
    }

    fn visit_assign(&mut self, _name: &Token, value: &Expr) -> Self::Output {
        value.accept(self)
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
        unimplemented!()
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

impl StmtVisitor for Compiler {
    type Output = Result<(), PiccoloError>;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        expr.accept(self)?;
        self.0.write(Opcode::Pop, 1); // TODO: line
        Ok(())
    }

    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output {
        value.accept(self)?;
        if op.kind == TokenKind::Assign {
            let idx = self.0.make_constant(Value::String(name.lexeme.to_owned()));
            let (low, high) = crate::decode_bytes(idx);
            self.0.write(Opcode::AssignGlobal, name.line);
            self.0.write(low, name.line);
            self.0.write(high, name.line);
        } else if op.kind == TokenKind::Declare {
            let idx = self.0.make_constant(Value::String(name.lexeme.to_owned()));
            let (low, high) = crate::decode_bytes(idx);
            self.0.write(Opcode::DeclareGlobal, name.line);
            self.0.write(low, name.line);
            self.0.write(high, name.line);
        }
        Ok(())
    }

    fn visit_block(&mut self, _stmts: &[Stmt]) -> Self::Output {
        unimplemented!("visit_block")
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

        self.0.write(Opcode::Return, keyword.line);

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
