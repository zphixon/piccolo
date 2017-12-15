
use ::*;
use expr::ExprAccept;
use stmt::StmtAccept;
use value::Value;
use token::TokenKind;
use err::ErrorKind;

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub err: String,
    pub had_err: bool,
    pub env: env::Env,
}

impl expr::ExprVisitor for Interpreter {
    type Output = Value;

    fn visit_literal(&mut self, e: &expr::Literal) -> Value {
        match e {
            &expr::Literal::Array(expr::Array { len, ref inner }) => {
                Value::Array(inner.iter().cloned().map(|e| self.evaluate(&e)).collect())
            }
            l => (::std::mem::replace(&mut l.clone(), expr::Literal::Nil)).into(),
        }
    }

    fn visit_paren(&mut self, e: &expr::Paren) -> Value {
        self.evaluate(&expr::Expr::Paren(e.clone()))
    }

    fn visit_unary(&mut self, e: &expr::Unary) -> Value {
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

    fn visit_logical(&mut self, e: &expr::Logical) -> Value {
        let left = self.evaluate(&e.lhs);

        if e.op.kind == token::TokenKind::Or {
            if is_truthy(&left) {
                return left
            }
        } else {
            if !is_truthy(&left) {
                return left
            }
        }

        self.evaluate(&e.rhs)
    }

    fn visit_binary(&mut self, e: &expr::Binary) -> Value {
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
                    }
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

            v => panic!("unreachable: {:?} {}", v, e.op.line)
        }
    }

    fn visit_variable(&mut self, e: &expr::Variable) -> Value {
        if let Some(v) = self.env.get(&e.0) {
            v
        } else {
            self.error(ErrorKind::UndefinedVariable, e.0.line, format!("variable {} is undefined", e.0.lexeme));
            Value::Nil
        }
    }

    fn visit_assign(&mut self, e: &expr::Assignment) -> Value {
        let value = self.evaluate(&e.value);
        self.env.define(&e.name.lexeme, value.clone());
        //Value::Nil // TODO
        value
    }
}

impl stmt::StmtVisitor for Interpreter {
    type Output = ();

    fn visit_expr(&mut self, s: &stmt::StmtExpr) {
        self.evaluate(&s.0);
    }

    fn visit_me_tmp(&mut self, s: &stmt::MeTmp) {
        let value = self.evaluate(&s.0);
        println!("{}", value);
    }

    fn visit_assignment(&mut self, s: &stmt::Assignment) {
        let value = self.evaluate(&s.value);
        self.env.define(&s.name.lexeme, value);
    }

    fn visit_block(&mut self, s: &stmt::Block) {
        self.execute_block(&s.0);
    }

    fn visit_if(&mut self, s: &stmt::If) {
        if is_truthy(&self.evaluate(&s.cond)) {
            self.execute_block(&s.then);
        } else if s.else_.is_some() {
            self.execute_block(s.else_.as_ref().unwrap());
        }
    }

    fn visit_while(&mut self, s: &stmt::While) {
        while is_truthy(&self.evaluate(&s.cond)) {
            self.execute_block(&s.body);
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            had_err: false,
            err: String::new(),
            env: env::Env::new(),
        }
    }

    pub fn reset_err(&mut self) {
        self.had_err = false;
        self.err = String::new();
    }

    pub fn interpret(&mut self, statements: &[stmt::Stmt]) -> Result<(), String> {
        for stmt in statements.iter() {
            self.execute(&stmt);
            if self.had_err {
                return Err(self.err.clone())
            }
        }
        Ok(())
    }

    pub fn eval(&mut self, e: &expr::Expr) -> Result<Value, String> {
        let v = self.evaluate(e);
        if self.had_err {
            return Err(self.err.clone())
        }
        Ok(v)
    }

    fn execute(&mut self, s: &stmt::Stmt) {
        s.accept(&mut *self);
    }

    fn execute_block(&mut self, stmts: &[stmt::Stmt]) {
        self.env.push();
        for stmt in stmts {
            self.execute(stmt);
        }
        self.env.pop();
    }

    fn evaluate(&mut self, e: &expr::Expr) -> Value {
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

