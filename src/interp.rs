
extern crate time;
extern crate rustyline;

use self::rustyline::Editor;

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
        let env = env::Env::new();

        env.new_native_func("clock", 0, |_, _| {
            let ts = time::now().to_timespec();
            Ok((ts.sec * 1_000 + ts.nsec as i64 / 1_000_000).into())
        });

        env.new_native_func("prln", 1, |_, args| {
            println!("{}", args[0]);
            Ok(Value::Nil)
        });

        env.new_native_func("panic", 1, |_, args| {
            eprintln!("piccolo panic! {}", args[0]);
            std::process::exit(1);
        });

        env.new_native_func("str", 1, |_, args| {
            Ok(Value::String(format!("{}", args[0])))
        });

        env.new_native_func("assert", 1, |_, args| {
            if !is_truthy(&args[0]) {
                eprintln!("assert failed: {}", args[0]);
                std::process::exit(1);
            }
            Ok(Value::Bool(true))
        });

        env.new_native_func("show_env", 0, |i, _| {
            println!("{}", i.env);
            Ok(Value::Nil)
        });

        env.new_native_func("input", 0, |_, _| {
            let mut rl = Editor::<()>::new();
            if let Ok(input) = rl.readline("") {
                Ok(Value::String(input))
            } else {
                Ok(Value::Nil)
            }
        });

        Interpreter { env }
    }

    pub fn interpret(&mut self, stmts: &[stmt::Stmt]) -> Result<Option<Value>, String> {
        for stmt in stmts {
            if let Some(v) = self.execute(stmt)? {
                return Ok(Some(v))
            }
        }

        Ok(None)
    }

    pub fn execute_list(&mut self, stmts: &[stmt::Stmt]) -> Result<Option<Value>, String> {
        self.env.push();
        let result = self.interpret(stmts);
        self.env.pop();
        result
    }

    pub fn execute(&mut self, stmt: &stmt::Stmt) -> Result<Option<Value>, String> {
        stmt.accept(&mut *self)
    }

    pub fn evaluate(&mut self, expr: &expr::Expr) -> Result<Value, String> {
        expr.accept(&mut *self)
    }

    fn error(&mut self, line: u64, kind: ErrorKind, why: &str) -> String {
        // PiccoloError::new(kind, line, why)
        format!("Error, line {}: {:?} - {}", line, kind, why)
    }
}

impl expr::ExprVisitor for Interpreter {
    type Output = Result<Value, String>;

    fn visit_binary(&mut self, e: &expr::Binary) -> Self::Output {
        let mut lhs = self.evaluate(&e.lhs)?;
        let mut rhs = self.evaluate(&e.rhs)?;

        Ok(match e.op.kind {
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
                        Value::Array((l..r).map(|n| n.into()).collect())
                    },
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}..{:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}..{:?}", lhs, rhs)))
            },

            TokenKind::IRange => match lhs {
                Value::Integer(l) => match rhs {
                    Value::Integer(r) => {
                        Value::Array((l..r + 1).map(|n| n.into()).collect())
                    },
                    _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}...{:?}", lhs, rhs)))
                },
                _ => return Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to create range {:?}...{:?}", lhs, rhs)))
            },

            v => panic!("unreachable: {:?} {}", v, e.op.line)
        })
    }

    fn visit_unary(&mut self, e: &expr::Unary) -> Self::Output {
        let rhs = self.evaluate(&e.rhs)?;

        match e.op.kind {
            TokenKind::Minus => match rhs {
                Value::Integer(ref n) => Ok((-n).into()),
                Value::Float(ref n) => Ok((-n).into()),
                v => Err(self.error(e.op.line, ErrorKind::MathError, &format!("Tried to negate non-bool/number {:?}", v)))
            },
            TokenKind::Not => {
                let b: bool = is_truthy(&rhs);
                Ok(Value::Bool(!b))
            },
            _ => Err(self.error(e.op.line, ErrorKind::MathError, &format!("Not a unary operator: \"{}\"", e.op.lexeme)))
        }
    }

    fn visit_paren(&mut self, e: &expr::Paren) -> Self::Output {
        self.evaluate(&e.0)
    }

    fn visit_literal(&mut self, e: &expr::Literal) -> Self::Output {
        match e {
            &expr::Literal::Array(expr::Array { ref inner, .. }) => {
                let mut new = Vec::new();
                for item in inner.iter() {
                    let new_item = self.evaluate(item)?;
                    new.push(new_item);
                }
                Ok(Value::Array(new))
            },
            l => Ok(std::mem::replace(&mut l.clone(), expr::Literal::Nil).into())
        }
    }

    fn visit_variable(&mut self, e: &expr::Variable) -> Self::Output {
        if let Some(v) = self.env.get(&e.0.lexeme) {
            Ok(v)
        } else {
            Err(self.error(e.0.line, ErrorKind::UndefinedVariable, &format!("{} is undefined", e.0.lexeme)))
        }
    }

    fn visit_assign(&mut self, e: &expr::Assignment) -> Self::Output {
        let value = self.evaluate(&e.value)?;
        // function hack?
        self.env.set(&e.name.lexeme, value.clone());
        Ok(value)
    }

    fn visit_logical(&mut self, e: &expr::Logical) -> Self::Output {
        let lhs = self.evaluate(&e.lhs)?;

        if e.op.kind == token::TokenKind::Or {
            if is_truthy(&lhs) {
                return Ok(lhs)
            }
        } else if !is_truthy(&lhs) {
            return Ok(lhs)
        }

        self.evaluate(&e.rhs)
    }

    fn visit_call(&mut self, e: &expr::Call) -> Self::Output {
        let callee = self.evaluate(&e.callee)?;

        let mut func = match callee {
            Value::Func(f) => f,
            v => {
                return Err(self.error(e.paren.line, ErrorKind::NonFunction, &format!("attempt to call non-function {:?}", v)));
            }
        };

        let args: Result<Vec<Value>, String> = e.args.iter().map(|arg| self.evaluate(arg)).collect();
        let args = args?;

        if args.len() != func.arity() {
            return Err(self.error(e.paren.line, ErrorKind::IncorrectArity, &format!("expected {} args, got {}", func.arity(), args.len())));
        }

        func.call(&mut *self, args)
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
    type Output = Result<Option<Value>, String>;

    fn visit_expr(&mut self, s: &stmt::StmtExpr) -> Self::Output {
        s.0.accept(&mut *self)?;
        Ok(None)
    }

    fn visit_assignment(&mut self, s: &stmt::Assignment) -> Self::Output {
        let value = self.evaluate(&s.value)?;
        self.env.set(&s.name.lexeme, value);
        Ok(None)
    }

    fn visit_block(&mut self, s: &stmt::Block) -> Self::Output {
        self.execute_list(&s.0)
    }

    fn visit_if(&mut self, s: &stmt::If) -> Self::Output {
        let cond = self.evaluate(&s.cond)?;

        let result = if is_truthy(&cond) {
            self.execute_list(&s.then)?
        } else if let Some(ref else_) = s.else_ {
            self.execute_list(&else_)?
        } else {
            None
        };

        Ok(result)
    }

    fn visit_while(&mut self, s: &stmt::While) -> Self::Output {
        let mut cond = self.evaluate(&s.cond)?;
        while is_truthy(&cond) {
            if let Some(v) = self.execute_list(&s.body)? {
                return Ok(Some(v))
            }
            cond = self.evaluate(&s.cond)?;
        }
        Ok(None)
    }

    fn visit_for(&mut self, s: &stmt::For) -> Self::Output {
        let iter = self.evaluate(&s.iter)?;
        self.env.push();
        match iter {
            Value::Array(ref a) => {
                for item in a {
                    self.env.set(&s.name.lexeme, item.clone());
                    if let Some(r) = self.interpret(&s.body)? {
                        self.env.pop();
                        return Ok(Some(r))
                    }
                }
            }
            _ => return Err(self.error(s.name.line, ErrorKind::NonIterator, &format!("Cannot be iterated over: {:?}", iter))),
        }
        self.env.pop();
        Ok(None)
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

