
use std::any::Any;

use ::ast;
use ast::Accept;
use value::Value;
use token::TokenKind;

#[derive(Debug, Clone)]
pub struct Interpreter {
}

impl ast::ExprVisitor for Interpreter {
    type Output = Value;

    fn visit_literal(&mut self, e: &ast::Literal) -> Value {
        Value::Nil
        //(::std::mem::replace(&mut e.clone(), ast::Literal::Nil)).into()
    }

    fn visit_paren(&mut self, e: &ast::Paren) -> Value {
        self.evaluate(&ast::Expr::Paren(e.clone()))
    }

    fn visit_unary(&mut self, e: &ast::Unary) -> Value {
        let rhs = self.evaluate(&e.rhs);
        match e.op.kind {
            TokenKind::Minus => match rhs {
                Value::Integer(n) => (-n).into(),
                Value::Float(n) => (-n).into(),
                _ => panic!("negative'd non-number")
            },
            TokenKind::Not => (!is_truthy(&rhs)).into(),
            k => panic!("unreachable: {:?} {}", k, e.op.line)
        }
    }

    fn visit_binary(&mut self, e: &ast::Binary) -> Value {
        let lhs = self.evaluate(&e.lhs);
        let rhs = self.evaluate(&e.rhs);

        match e.op.kind {
            TokenKind::Minus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l - r),
                    Value::Integer(r) => Value::Float(l - r as f64),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 - r),
                    Value::Integer(r) => Value::Integer(l - r),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                v => panic!("unreachable: {:?} {}", v, e.op.line)
            },
            TokenKind::Plus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l + r),
                    Value::Integer(r) => Value::Float(l + r as f64),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 + r),
                    Value::Integer(r) => Value::Integer(l + r),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::String(mut l) => match rhs {
                    Value::String(r) => Value::String({l.push_str(&r); l}),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                v => panic!("unreachable: {:?} {}", v, e.op.line)
            },
            TokenKind::FSlash => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l / r),
                    Value::Integer(r) => Value::Float(l / r as f64),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 / r),
                    Value::Integer(r) => Value::Integer(l / r),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                v => panic!("unreachable: {:?} {}", v, e.op.line)
            },
            TokenKind::Star => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l * r),
                    Value::Integer(r) => Value::Float(l * r as f64),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 * r),
                    Value::Integer(r) => Value::Integer(l * r),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                v => panic!("unreachable: {:?} {}", v, e.op.line)
            },
            TokenKind::Mod => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l % r),
                    Value::Integer(r) => Value::Float(l % r as f64),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 % r),
                    Value::Integer(r) => Value::Integer(l % r),
                    v => panic!("unreachable: {:?} {}", v, e.op.line)
                },
                v => panic!("unreachable: {:?} {}", v, e.op.line)
            },
            v => panic!("unreachable: {:?} {}", v, e.op.line)
        }
    }
}

impl Interpreter {
    fn evaluate(&mut self, e: &ast::Expr) -> Value {
        e.accept(&mut *self)
        //Accept::accept(e, &mut self)
    }
}

fn is_truthy(e: &Value) -> bool {
    match e {
        &Value::Bool(b) => b,
        &Value::Nil => false,
        _ => true,
    }
}

