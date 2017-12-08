
use std::any::Any;

use ::ast;
use ast::{Expr, ExprVisitor, Accept};

#[derive(Debug, Clone)]
pub struct Interpreter {
}

impl ast::ExprVisitor for Interpreter {
    type Output = Box<Any>;

    fn visit_literal(&mut self, e: &ast::Literal) -> Box<Any> {
        Box::new(e.clone())
    }

    fn visit_paren(&mut self, e: &ast::Paren) -> Box<Any> {
        self.evaluate(&Expr::Paren(e.clone()))
    }

    fn visit_unary(&mut self, e: &ast::Unary) -> Box<Any> {
        let rhs = self.evaluate(&*e.rhs);
        if rhs.is::<ast::Literal>() {
            Box::new(true)
        } else {
            Box::new(false)
        }
        //match e.op.kind {
        //    ::token::TokenKind::Minus => Box::new(-rhs),
        //}
    }

    fn visit_binary(&mut self, e: &ast::Binary) -> Box<Any> {
        panic!("shiet");
    }
}

impl Interpreter {
    fn evaluate(&mut self, e: &Expr) -> Box<Any> {
        e.accept(&mut *self)
        //Accept::accept(e, &mut self)
    }
}

