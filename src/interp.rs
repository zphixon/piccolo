
use std::any::Any;

use ::ast;
use ast::Accept;
use value::Value;
use token::TokenKind;
use err::ErrorKind;

#[derive(Debug, Clone)]
pub struct Interpreter {
    err: String,
    had_err: bool,
}

impl ast::ExprVisitor for Interpreter {
    type Output = Value;

    fn visit_literal(&mut self, e: &ast::Literal) -> Value {
        (::std::mem::replace(&mut e.clone(), ast::Literal::Nil)).into()
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
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to negate {:?}", rhs));
                    Value::Nil
                }
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
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to subtract {:?} from {:?}", rhs, lhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 - r),
                    Value::Integer(r) => Value::Integer(l - r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to subtract {:?} from {:?}", rhs, lhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to subtract {:?} from {:?}", rhs, lhs));
                    Value::Nil
                }
            },

            TokenKind::Plus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l + r),
                    Value::Integer(r) => Value::Float(l + r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to add {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }//panic!("unreachable: {:?} {}", v, e.op.line)
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 + r),
                    Value::Integer(r) => Value::Integer(l + r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to add {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::String(mut l) => match rhs {
                    Value::String(r) => Value::String({l.push_str(&r); l}),
                    r => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to add {:?} to {:?}", l, r));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to add {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Divide => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l / r),
                    Value::Integer(r) => Value::Float(l / r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to divide {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 / r),
                    Value::Integer(r) => Value::Integer(l / r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to divide {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to divide {:?} by {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Star => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l * r),
                    Value::Integer(r) => Value::Float(l * r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to multiply {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 * r),
                    Value::Integer(r) => Value::Integer(l * r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to multiply {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to multiply {:?} by {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Mod => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l % r),
                    Value::Integer(r) => Value::Float(l % r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to modulo {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 % r),
                    Value::Integer(r) => Value::Integer(l % r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to modulo {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to modulo {:?} by {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::GreaterThan => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l > r),
                    Value::Integer(r) => Value::Bool(l > r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 > r),
                    Value::Integer(r) => Value::Bool(l > r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::LessThan => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l < r),
                    Value::Integer(r) => Value::Bool(l < r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool((l as f64) < r),
                    Value::Integer(r) => Value::Bool(l < r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::GreaterThanEquals => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l >= r),
                    Value::Integer(r) => Value::Bool(l >= r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 >= r),
                    Value::Integer(r) => Value::Bool(l >= r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::LessThanEquals => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l <= r),
                    Value::Integer(r) => Value::Bool(l <= r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 <= r),
                    Value::Integer(r) => Value::Bool(l <= r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Equals => Value::Bool(is_equal(&lhs, &rhs)),
            TokenKind::NotEquals => Value::Bool(!is_equal(&lhs, &rhs)),

            TokenKind::And => Value::Bool(is_truthy(&lhs) && is_truthy(&rhs)),
            TokenKind::Or => Value::Bool(is_truthy(&lhs) && is_truthy(&rhs)),

            v => panic!("unreachable: {:?} {}", v, e.op.line)
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            had_err: false,
            err: String::new(),
        }
    }

    pub fn interpret(&mut self, e: &ast::Expr) -> Result<Value, String> {
        let v = self.evaluate(e);
        if self.had_err {
            Err(self.err.clone())
        } else {
            Ok(v)
        }
    }

    fn evaluate(&mut self, e: &ast::Expr) -> Value {
        e.accept(&mut *self)
    }

    fn error(&mut self, kind: ErrorKind, line: u64, msg: String) {
        self.had_err = true;
        self.err.push_str(&format!("Line {} - {:?}: {}\n", line, kind, msg));
    }
}

pub fn is_truthy(e: &Value) -> bool {
    match e {
        &Value::Bool(b) => b,
        &Value::Nil => false,
        _ => true,
    }
}

pub fn is_equal(lhs: &Value, rhs: &Value) -> bool {
    if lhs == &Value::Nil && rhs == &Value::Nil {
        true
    } else if lhs == &Value::Nil || rhs == &Value::Nil {
        false
    } else {
        lhs == rhs
    }
}

