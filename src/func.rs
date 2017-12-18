
use ::*;

#[derive(Clone, PartialEq, Debug)]
pub struct Func {
    pub native: Option<NativeFunc>,
    pub decl: Option<stmt::Func>,
    pub name: String,
    pub closure: env::Env, // TODO
}

impl Func {
    pub fn new(decl: stmt::Func) -> Self {
        Func {
            native: None,
            name: decl.name.lexeme.clone(),
            decl: Some(decl),
            closure: env::Env::new()
        }
    }

    pub fn new_native(native: NativeFunc) -> Self {
        Func {
            name: native.name(),
            native: Some(native),
            decl: None,
            closure: env::Env::new()
        }
    }

    pub fn with_closure(decl: stmt::Func, closure: env::Env) -> Self {
        Func {
            native: None,
            name: decl.name.lexeme.clone(),
            decl: Some(decl),
            closure
        }
    }

    pub fn arity(&self) -> usize {
        if self.native.is_some() { self.native.as_ref().unwrap().arity() }
        else if self.decl.is_some() { self.decl.as_ref().unwrap().args.len() }
        else { panic!("arity on empty function") }
    }

    pub fn call(&mut self, interp: &mut interp::Interpreter, args: Vec<value::Value>) -> value::Value {
        if self.native.is_some() {
            self.native.as_ref().unwrap().call(interp, args)
        } else if self.decl.is_some() {
            //interp.env.push();
            for (n, arg) in args.iter().enumerate() {
                interp.env.set_local(&self.decl.as_ref().unwrap().args[n].lexeme, arg.clone());
            }
            //println!("closure before: {}", self.closure);
            let value = interp.execute_block_local_closure(&self.decl.as_ref().unwrap().body, &mut self.closure);
            println!("{}", self.name);
            interp.env.define(&self.name, value::Value::Func(self.clone()));
            //println!("cloned after: {:?}", self.clone());
            //interp.env.pop();
            value.unwrap_or(value::Value::Nil)
        } else {
            panic!("empty function called!")
        }
    }

    pub fn is_native(&self) -> bool {
        self.native.is_some()
    }
}

#[derive(Clone)]
pub struct NativeFunc {
    arity: usize,
    inner: fn(&mut interp::Interpreter, Vec<value::Value>) -> value::Value,
    name: String,
}

impl std::fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "NativeFunc {{ inner: ..., arity: {} }}", self.arity())
    }
}

impl std::cmp::PartialEq for NativeFunc {
    fn eq(&self, _other: &NativeFunc) -> bool { false }
}

impl NativeFunc {
    pub fn new(name: String, arity: usize, inner: fn(&mut interp::Interpreter, Vec<value::Value>) -> value::Value) -> Self {
        NativeFunc {
            arity,
            inner,
            name,
        }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

impl NativeFunc {
    fn call(&self, mut interp: &mut interp::Interpreter, args: Vec<value::Value>) -> value::Value {
        let inner = self.inner;
        inner(&mut interp, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

pub fn new_native_func(name: &str, arity: usize, inner: fn(&mut interp::Interpreter, Vec<value::Value>) -> value::Value) -> value::Value {
    value::Value::Func(Func::new_native(NativeFunc::new(name.to_owned(), arity, inner)))
}

