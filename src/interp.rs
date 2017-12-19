
use ::*;

use expr::ExprAccept;
use stmt::StmtAccept;
use value::{Value, is_equal, is_truthy};
use err::ErrorKind;
use token::TokenKind;

use std::rc::Rc;
use std::cell::RefCell;

pub struct Interpreter {
    env: env::Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: env::Env::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[stmt::Stmt]) -> Result<Option<Rc<RefCell<Value>>>, String> {
        let mut r = None;
        for stmt in stmts {
            r = self.execute(stmt)?;
        }
        Ok(r)
    }

    pub fn execute(&mut self, stmt: &stmt::Stmt) -> Result<Option<Rc<RefCell<Value>>>, String> {
        stmt.accept(&mut *self)
    }

    pub fn evaluate(&mut self, expr: &expr::Expr) -> Result<Rc<RefCell<Value>>, String> {
        expr.accept(&mut *self)
    }

    fn error(&mut self, line: u64, kind: ErrorKind, why: &str) -> String {
        // PiccoloError::new(kind, line, why)
        format!("Error, line {}: {:?} - {}", line, kind, why)
    }
}

impl expr::ExprVisitor for Interpreter {
    type Output = Result<Rc<RefCell<Value>>, String>;

    fn visit_binary(&mut self, e: &expr::Binary) -> Self::Output {
        let lhs = &mut self.evaluate(&e.lhs)?;
        let mut lhs = Rc::make_mut(lhs).borrow_mut().clone();
        let rhs = &mut self.evaluate(&e.rhs)?;
        let mut rhs = Rc::make_mut(rhs).borrow_mut().clone();

        Ok(Rc::new(RefCell::new(match e.op.kind {
            TokenKind::Minus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l - r),
                    Value::Integer(r) => Value::Float(l - r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to subtract {:?} from {:?}", rhs, lhs)))

                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 - r),
                    Value::Integer(r) => Value::Integer(l - r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to subtract {:?} from {:?}", rhs, lhs)))

                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to subtract {:?} from {:?}", rhs, lhs)))
            },

            TokenKind::Plus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l + r),
                    Value::Integer(r) => Value::Float(l + r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to add {:?} to {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 + r),
                    Value::Integer(r) => Value::Integer(l + r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to add {:?} to {:?}", lhs, rhs)))
                },
                Value::String(ref mut l) => match rhs {
                    Value::String(ref mut r) => Value::String({l.push_str(&r); l.clone()}),
                    r => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to add {:?} to {:?}", l, r)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to add {:?} to {:?}", lhs, rhs)))
            },

            TokenKind::Divide => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l / r),
                    Value::Integer(r) => Value::Float(l / r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to divide {:?} by {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 / r),
                    Value::Integer(r) => Value::Integer(l / r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to divide {:?} by {:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to divide {:?} by {:?}", lhs, rhs)))
            },

            TokenKind::Star => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l * r),
                    Value::Integer(r) => Value::Float(l * r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to multiply {:?} by {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 * r),
                    Value::Integer(r) => Value::Integer(l * r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to multiply {:?} by {:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to multiply {:?} by {:?}", lhs, rhs)))
            },

            TokenKind::Mod => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l % r),
                    Value::Integer(r) => Value::Float(l % r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to modulo {:?} by {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 % r),
                    Value::Integer(r) => Value::Integer(l % r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to modulo {:?} by {:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to modulo {:?} by {:?}", lhs, rhs)))
            },

            TokenKind::GreaterThan => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l > r),
                    Value::Integer(r) => Value::Bool(l > r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 > r),
                    Value::Integer(r) => Value::Bool(l > r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                }
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
            },

            TokenKind::LessThan => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l < r),
                    Value::Integer(r) => Value::Bool(l < r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool((l as f64) < r),
                    Value::Integer(r) => Value::Bool(l < r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                }
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
            },

            TokenKind::GreaterThanEquals => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l >= r),
                    Value::Integer(r) => Value::Bool(l >= r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 >= r),
                    Value::Integer(r) => Value::Bool(l >= r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                }
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
            },

            TokenKind::LessThanEquals => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l <= r),
                    Value::Integer(r) => Value::Bool(l <= r as f64),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 <= r),
                    Value::Integer(r) => Value::Bool(l <= r),
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
                }
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to compare {:?} to {:?}", lhs, rhs)))
            },

            TokenKind::Equals => Value::Bool(is_equal(&lhs, &rhs)),
            TokenKind::NotEquals => Value::Bool(!is_equal(&lhs, &rhs)),

            TokenKind::ERange => match lhs {
                Value::Integer(l) => match rhs {
                    Value::Integer(r) => {
                        Value::Array((l..r).map(|n| Rc::new(RefCell::new(n.into()))).collect())
                    },
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}..{:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}..{:?}", lhs, rhs)))
            },

            TokenKind::IRange => match lhs {
                Value::Integer(l) => match rhs {
                    Value::Integer(r) => {
                        Value::Array((l..r + 1).map(|n| Rc::new(RefCell::new(n.into()))).collect())
                    },
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}...{:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}...{:?}", lhs, rhs)))
            },

            v => panic!("unreachable: {:?} {}", v, e.op.line)
        })))
    }

    fn visit_unary(&mut self, e: &expr::Unary) -> Self::Output {
        Err(self.error(e.op.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_paren(&mut self, e: &expr::Paren) -> Self::Output {
        Err(self.error(0, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_literal(&mut self, e: &expr::Literal) -> Self::Output {
        match e {
            &expr::Literal::Array(expr::Array { ref inner, .. }) => {
                let mut new = Vec::new();
                for item in inner.iter() {
                    let new_item = self.evaluate(item)?;
                    new.push(new_item);
                }
                Ok(Rc::new(RefCell::new(Value::Array(new))))
            },
            l => Ok(Rc::new(RefCell::new(std::mem::replace(&mut l.clone(), expr::Literal::Nil).into())))
        }
    }

    fn visit_variable(&mut self, e: &expr::Variable) -> Self::Output {
        Err(self.error(0, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_assign(&mut self, e: &expr::Assignment) -> Self::Output {
        Err(self.error(e.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_logical(&mut self, e: &expr::Logical) -> Self::Output {
        let lhs = self.evaluate(&e.lhs)?;

        if e.op.kind == token::TokenKind::Or && is_truthy(&lhs.borrow()) {
            Ok(lhs)
        } else if !is_truthy(&lhs.borrow()) {
            Ok(lhs)
        } else {
            self.evaluate(&e.rhs)
        }
    }

    fn visit_call(&mut self, e: &expr::Call) -> Self::Output {
        Err(self.error(e.paren.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_new(&mut self, e: &expr::New) -> Self::Output {
        Err(self.error(e.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_get(&mut self, e: &expr::Get) -> Self::Output {
        Err(self.error(e.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_set(&mut self, e: &expr::Set) -> Self::Output {
        Err(self.error(e.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }
}

impl stmt::StmtVisitor for Interpreter {
    type Output = Result<Option<Rc<RefCell<Value>>>, String>;

    fn visit_expr(&mut self, s: &stmt::StmtExpr) -> Self::Output {
        Ok(Some(s.0.accept(&mut *self)?))
    }

    fn visit_assignment(&mut self, s: &stmt::Assignment) -> Self::Output {
        Err(self.error(s.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_block(&mut self, s: &stmt::Block) -> Self::Output {
        Err(self.error(0, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_if(&mut self, s: &stmt::If) -> Self::Output {
        Err(self.error(0, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_while(&mut self, s: &stmt::While) -> Self::Output {
        Err(self.error(0, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_for(&mut self, s: &stmt::For) -> Self::Output {
        Err(self.error(s.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_func(&mut self, s: &stmt::Func) -> Self::Output {
        Err(self.error(s.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_retn(&mut self, s: &stmt::Retn) -> Self::Output {
        Err(self.error(s.keyword.line, ErrorKind::Unimplemented, "not yet implemented"))
    }

    fn visit_data(&mut self, s: &stmt::Data) -> Self::Output {
        Err(self.error(s.name.line, ErrorKind::Unimplemented, "not yet implemented"))
    }
}

