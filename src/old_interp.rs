
extern crate time;

use ::*;
use expr::ExprAccept;
use stmt::StmtAccept;
use value::Value;
use token::TokenKind;
use err::ErrorKind;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
pub struct Interpreter {
    pub err: String,
    pub had_err: bool,
    pub env: env::Env,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env = env::Env::new();

        env.define("clock", func::new_native_func("clock", 0, |_, _| {
            time::now().to_timespec().sec.into()
        }));

        env.define("prln", func::new_native_func("prln", 1, |_, args| {
            println!("{}", args[0]);
            Value::Nil
        }));

        env.define("panic", func::new_native_func("panic", 1, |_, args| {
            eprintln!("piccolo panic! {}", args[0]);
            std::process::exit(1);
        }));

        env.define("str", func::new_native_func("str", 1, |_, args| {
            Value::String(format!("{}", args[0]))
        }));

        env.define("assert", func::new_native_func("assert", 1, |_, args| {
            if !is_truthy(&args[0]) {
                eprintln!("assert failed: {}", args[0]);
                std::process::exit(1);
            }
            Value::Bool(true)
        }));

        env.define("show_env", func::new_native_func("show_env", 0, |i, _| {
            println!("{}", i.env);
            Value::Nil
        }));

        //env.define("show_closure", func::new_native_func("show_closure", 1, |_, args| {
        //    match args[0] {
        //        Value::Func(ref f) => println!("{}", f.closure),
        //        ref v => println!("not a function: {}", v)
        //    }
        //    Value::Nil
        //}));

        env.push();

        Interpreter {
            had_err: false,
            err: String::new(),
            env
        }
    }

    pub fn reset_err(&mut self) {
        self.had_err = false;
        self.err = String::new();
    }

    pub fn interpret(&mut self, statements: &[stmt::Stmt]) -> Result<Option<Rc<RefCell<Value>>>, String> {
        for stmt in statements.iter() {
            let ret = self.execute(stmt);
            if self.had_err {
                return Err(self.err.clone())
            }
            if ret.is_some() {
                return Ok(ret)
            }
        }
        Ok(None)
    }

    pub fn eval(&mut self, e: &expr::Expr) -> Result<Rc<RefCell<Value>>, String> {
        let v = self.evaluate(e);
        if self.had_err {
            return Err(self.err.clone())
        }
        Ok(v)
    }

    pub fn execute(&mut self, s: &stmt::Stmt) -> Option<Rc<RefCell<Value>>> {
        s.accept(&mut *self)
    }

    pub fn execute_block(&mut self, stmts: &[stmt::Stmt]) -> Option<Rc<RefCell<Value>>> {
        self.env.push();
        for stmt in stmts {
            if let Some(r) = self.execute(stmt) {
                self.env.pop();
                return Some(r)
            }
        }
        self.env.pop();
        None
    }

    pub fn execute_block_local(&mut self, stmts: &[stmt::Stmt]) -> Option<Rc<RefCell<Value>>> {
        for stmt in stmts {
            if let Some(r) = self.execute(stmt) {
                return Some(r)
            }
        }
        None
    }

    //pub fn execute_block_local_closure(&mut self, stmts: &[stmt::Stmt], mut env: &mut env::Env) -> Option<Value> {
    //    env.push_parent(self.env.clone());
    //    std::mem::swap(&mut self.env, &mut env);
    //    let r = self.execute_block_local(stmts);
    //    std::mem::swap(&mut self.env, &mut env);
    //    *env = env.split();
    //    r
    //}

    fn evaluate(&mut self, e: &expr::Expr) -> Rc<RefCell<Value>> {
        e.accept(&mut *self)
    }

    fn error(&mut self, kind: ErrorKind, line: u64, msg: &str) {
        self.had_err = true;
        self.err.push_str(&format!("Line {} - {:?}: {}\n", line, kind, msg));
    }
}

impl expr::ExprVisitor for Interpreter {
    type Output = Rc<RefCell<Value>>;

    fn visit_literal(&mut self, e: &expr::Literal) -> Rc<RefCell<Value>> {
        match e {
            &expr::Literal::Array(expr::Array { ref inner, .. }) => {
                Value::Array(inner.iter().cloned().map(|e| self.evaluate(&e)).collect())
            },
            l => (::std::mem::replace(&mut l.clone(), expr::Literal::Nil)).into(),
        }
    }

    fn visit_paren(&mut self, e: &expr::Paren) -> Rc<RefCell<Value>> {
        self.evaluate(&expr::Expr::Paren(e.clone()))
    }

    fn visit_unary(&mut self, e: &expr::Unary) -> Rc<RefCell<Value>> {
        let rhs = self.evaluate(&e.rhs);
        match e.op.kind {
            TokenKind::Minus => match rhs {
                Value::Integer(n) => (-n).into(),
                Value::Float(n) => (-n).into(),
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to negate {:?}", rhs));
                    Value::Nil
                }
            },

            TokenKind::Not => (!is_truthy(&rhs)).into(),

            k => panic!("unreachable: {:?} {}", k, e.op.line)
        }
    }

    fn visit_logical(&mut self, e: &expr::Logical) -> Rc<RefCell<Value>> {
        let left = self.evaluate(&e.lhs);

        if e.op.kind == token::TokenKind::Or {
            if is_truthy(&left) {
                return left
            }
        } else if !is_truthy(&left) {
            return left
        }

        self.evaluate(&e.rhs)
    }

    #[allow(cyclomatic_complexity)]
    fn visit_binary(&mut self, e: &expr::Binary) -> Rc<RefCell<Value>> {
        let lhs = self.evaluate(&e.lhs);
        let rhs = self.evaluate(&e.rhs);

        match e.op.kind {
            TokenKind::Minus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l - r),
                    Value::Integer(r) => Value::Float(l - r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to subtract {:?} from {:?}", rhs, lhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 - r),
                    Value::Integer(r) => Value::Integer(l - r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to subtract {:?} from {:?}", rhs, lhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to subtract {:?} from {:?}", rhs, lhs));
                    Value::Nil
                }
            },

            TokenKind::Plus => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l + r),
                    Value::Integer(r) => Value::Float(l + r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to add {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 + r),
                    Value::Integer(r) => Value::Integer(l + r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to add {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::String(mut l) => match rhs {
                    Value::String(r) => Value::String({l.push_str(&r); l}),
                    r => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to add {:?} to {:?}", l, r));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to add {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Divide => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l / r),
                    Value::Integer(r) => Value::Float(l / r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to divide {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 / r),
                    Value::Integer(r) => Value::Integer(l / r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to divide {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to divide {:?} by {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Star => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l * r),
                    Value::Integer(r) => Value::Float(l * r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to multiply {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 * r),
                    Value::Integer(r) => Value::Integer(l * r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to multiply {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to multiply {:?} by {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Mod => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Float(l % r),
                    Value::Integer(r) => Value::Float(l % r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to modulo {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Float(l as f64 % r),
                    Value::Integer(r) => Value::Integer(l % r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to modulo {:?} by {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to modulo {:?} by {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::GreaterThan => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l > r),
                    Value::Integer(r) => Value::Bool(l > r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 > r),
                    Value::Integer(r) => Value::Bool(l > r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::LessThan => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l < r),
                    Value::Integer(r) => Value::Bool(l < r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool((l as f64) < r),
                    Value::Integer(r) => Value::Bool(l < r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::GreaterThanEquals => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l >= r),
                    Value::Integer(r) => Value::Bool(l >= r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 >= r),
                    Value::Integer(r) => Value::Bool(l >= r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::LessThanEquals => match lhs {
                Value::Float(l) => match rhs {
                    Value::Float(r) => Value::Bool(l <= r),
                    Value::Integer(r) => Value::Bool(l <= r as f64),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                Value::Integer(l) => match rhs {
                    Value::Float(r) => Value::Bool(l as f64 <= r),
                    Value::Integer(r) => Value::Bool(l <= r),
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                        Value::Nil
                    }
                }
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to compare {:?} to {:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::Equals => Value::Bool(is_equal(&lhs, &rhs)),
            TokenKind::NotEquals => Value::Bool(!is_equal(&lhs, &rhs)),

            TokenKind::ERange => match lhs {
                Value::Integer(l) => match rhs {
                    Value::Integer(r) => {
                        Value::Array((l..r).map(|n| n.into()).collect())
                    },
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to create range {:?}..{:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to create range {:?}..{:?}", lhs, rhs));
                    Value::Nil
                }
            },

            TokenKind::IRange => match lhs {
                Value::Integer(l) => match rhs {
                    Value::Integer(r) => {
                        Value::Array((l..r + 1).map(|n| n.into()).collect())
                    },
                    _ => {
                        self.error(ErrorKind::MathError, e.op.line, &format!("Tried to create range {:?}...{:?}", lhs, rhs));
                        Value::Nil
                    }
                },
                _ => {
                    self.error(ErrorKind::MathError, e.op.line, &format!("Tried to create range {:?}...{:?}", lhs, rhs));
                    Value::Nil
                }
            },

            v => panic!("unreachable: {:?} {}", v, e.op.line)
        }
    }

    fn visit_variable(&mut self, e: &expr::Variable) -> Rc<RefCell<Value>> {
        if let Some(v) = self.env.get(&e.0.lexeme) {
            v
        } else {
            self.error(ErrorKind::UndefinedVariable, e.0.line, &format!("variable {} is undefined", e.0.lexeme));
            Value::Nil
        }
    }

    fn visit_assign(&mut self, e: &expr::Assignment) -> Rc<RefCell<Value>> {
        let value = self.evaluate(&e.value);
        let value = match value { // HACK also not ideal
            Value::Func(mut f) => {
                f.name = e.name.lexeme.clone();
                Value::Func(f)
            },
            v => v
        };
        self.env.define(&e.name.lexeme, value.clone());
        //Value::Nil // TODO
        value
    }

    fn visit_call(&mut self, e: &expr::Call) -> Rc<RefCell<Value>> {
        let callee = self.evaluate(&e.callee);

        let mut func = match callee {
            Value::Func(f) => f,
            v => {
                self.error(ErrorKind::NonFunction, e.paren.line, &format!("attempt to call non-function {:?}", v));
                return Value::Nil;
            }
        };

        let args: Vec<Value> = e.args.iter().map(|arg| self.evaluate(arg)).collect();

        if args.len() != func.arity() {
            self.error(ErrorKind::IncorrectArity, e.paren.line, &format!("expected {} args, got {}", func.arity(), args.len()));
            return Value::Nil;
        }

        func.call(&mut *self, args)
    }

    fn visit_new(&mut self, e: &expr::New) -> Rc<RefCell<Value>> {
        if let Some(Value::Data(data)) = self.env.get(&e.name.lexeme) {
            let mut vars = std::collections::HashMap::new();
            vars.insert("test".into(), Value::Integer(13));
            Value::Instance(data::Instance {
                data,
                vars,
            })
        } else {
            self.error(ErrorKind::UndefinedVariable, e.name.line, &format!("undefined data name: {}", e.name.lexeme));
            Value::Nil
        }
    }

    fn visit_get(&mut self, e: &expr::Get) -> Rc<RefCell<Value>> {
        let value = self.evaluate(&*e.object);
        if let Value::Instance(mut value) = value {
            // TODO - move everything to RefCell
            value.get(&e.name.lexeme).unwrap()
        } else {
            self.error(ErrorKind::NonInstance, e.name.line, &format!("not an instance of data: {:?}", value));
            Value::Nil
        }
    }

    fn visit_set(&mut self, e: &expr::Set) -> Rc<RefCell<Value>> {
        if let Value::Instance(mut object) = self.evaluate(&*e.object) {
            let value = self.evaluate(&*e.value);
            object.set(&e.name.lexeme, value);
        } else {
            self.error(ErrorKind::NonInstance, e.name.line, &format!("only instances have fields"));
        }
        Value::Nil
    }
}

impl stmt::StmtVisitor for Interpreter {
    type Output = Option<Rc<RefCell<Value>>>;

    fn visit_expr(&mut self, s: &stmt::StmtExpr) -> Option<Rc<RefCell<Value>>> {
        self.evaluate(&s.0);
        None
    }

    fn visit_assignment(&mut self, s: &stmt::Assignment) -> Option<Rc<RefCell<Value>>> {
        let value = self.evaluate(&s.value);
        let value = match value { // HACK this is not ideal
            Value::Func(mut f) => {
                f.name = s.name.lexeme.clone();
                Value::Func(f)
            },
            v => v
        };
        self.env.define(&s.name.lexeme, value.clone());
        None
    }

    fn visit_block(&mut self, s: &stmt::Block) -> Option<Rc<RefCell<Value>>> {
        self.execute_block(&s.0);
        None
    }

    fn visit_if(&mut self, s: &stmt::If) -> Option<Rc<RefCell<Value>>> {
        if is_truthy(&self.evaluate(&s.cond)) {
            if let Some(r) = self.execute_block(&s.then) {
                Some(r)
            } else {
                None
            }
        } else if s.else_.is_some() {
            self.execute_block(s.else_.as_ref().unwrap());
            None
        } else {
            None
        }
    }

    fn visit_while(&mut self, s: &stmt::While) -> Option<Rc<RefCell<Value>>> {
        while is_truthy(&self.evaluate(&s.cond)) {
            if let Some(r) = self.execute_block(&s.body) {
                return Some(r)
            }
        }
        None
    }

    fn visit_for(&mut self, s: &stmt::For) -> Option<Rc<RefCell<Value>>> {
        let mut iter = self.evaluate(&s.iter);
        self.env.push();
        match iter {
            Value::Array(a) => {
                for item in a {
                    self.env.define(&s.name.lexeme, item);
                    if let Some(r) = self.execute_block(&s.body) {
                        self.env.pop();
                        return Some(r)
                    }
                }
            },
            Value::Func(ref mut f) => {
                while f.call(&mut *self, vec![]) != Value::Nil {
                    if let Some(r) = self.execute_block(&s.body) {
                        self.env.pop();
                        return Some(r)
                    }
                }
            },
            //Value::Nil => { // TODO
            //    return
            //}
            // fn -> repeatedly call until nil
            //i => panic!("unimplemented: {:?}", i)
            _ => {}
        }
        self.env.pop();
        None
    }

    fn visit_func(&mut self, s: &stmt::Func) -> Option<Rc<RefCell<Value>>> {
        let func = Value::Func(func::Func::new(s.clone()));
        //let func = Value::Func(func::Func::with_closure(s.clone(), self.env.children()));
        //let func = Value::Func(func::Func::with_closure(s.clone(), env::Env { inner: vec![self.env.inner[self.env.inner.len() - 1].clone()], splits: Vec::new() }));//self.env.children()));
        self.env.define(&s.name.lexeme, func);
        None
    }

    fn visit_retn(&mut self, s: &stmt::Retn) -> Option<Rc<RefCell<Value>>> {
        let value = if s.value != None {
            self.evaluate(s.value.as_ref().unwrap())
        } else {
            Value::Nil
        };
        Some(value)
    }

    fn visit_data(&mut self, s: &stmt::Data) -> Option<Rc<RefCell<Value>>> {
        let data = data::Data::new(&s.name.lexeme);
        self.env.define(&s.name.lexeme, Value::Data(data));
        None
    }
}

pub fn is_truthy(e: &Value) -> bool {
    match *e {
        Value::Bool(b) => b,
        Value::Nil => false,
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
