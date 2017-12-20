
use ::*;

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, PartialEq, Debug)]
pub struct Func {
    pub kind: FuncKind,
    pub name: String,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FuncKind {
    Native(NativeFunc),
    Normal(NormalFunc),
}

impl Func {
    pub fn new(decl: stmt::Func) -> Self {
        Func {
            name: decl.name.lexeme.clone(),
            kind: FuncKind::Normal(NormalFunc { decl }),
        }
    }

    pub fn new_native(name: &str, native: NativeFunc) -> Self {
        Func {
            name: name.to_owned(),
            kind: FuncKind::Native(native)
            //closure: env::Env::new()
        }
    }

    //pub fn with_closure(decl: stmt::Func, closure: env::Env) -> Self {
    //    Func {
    //        name: decl.name.lexeme.clone(),
    //        kind: FuncKind::Normal(NormalFunc { decl, closure })
    //    }
    //    //Func {
    //    //    native: None,
    //    //    name: decl.name.lexeme.clone(),
    //    //    decl: Some(decl),
    //    //    closure
    //    //}
    //}

    pub fn arity(&self) -> usize {
        match self.kind {
            FuncKind::Native(ref n) => n.arity(),
            FuncKind::Normal(ref n) => n.arity(),
        }
    }

    pub fn call(&mut self, interp: &mut interp::Interpreter, args: Vec<value::Value>) -> Result<value::Value, String> {
        match self.kind {
            FuncKind::Normal(ref mut f) => f.call(interp, args),
            FuncKind::Native(ref mut f) => f.call(interp, args),
        }
        //if self.native.is_some() {
        //    self.native.as_ref().unwrap().call(interp, args)
        //} else if self.decl.is_some() {
        //    //interp.env.push();
        //    //for (n, arg) in args.iter().enumerate() {
        //    //    interp.env.set_local(&self.decl.as_ref().unwrap().args[n].lexeme, arg.clone());
        //    //}
        //    //let value = interp.execute_block_local(&self.decl.as_ref().unwrap().body);
        //    //interp.env.pop();
        //    //let value = interp.execute_block_local_closure(&self.decl.as_ref().unwrap().body, &mut self.closure);
        //    //interp.env.define(&self.name, value::Value::Func(self.clone()));
        //    //value.unwrap_or(value::Value::Nil)
        //    Rc::new(RefCell::new(value::Value::Nil))
        //} else {
        //    panic!("empty function called!")
        //}
    }

    pub fn is_native(&self) -> bool {
        match self.kind {
            FuncKind::Native(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NormalFunc {
    pub decl: stmt::Func,
    //closure: env::Env, // TODO
}

impl NormalFunc {
    pub fn arity(&self) -> usize {
        self.decl.args.len()
    }

    pub fn call(&mut self, interp: &mut interp::Interpreter, args: Vec<value::Value>) -> Result<value::Value, String> {
        Err("not implemented".into())
    }
}

pub type NativeFuncType = fn(&mut interp::Interpreter, Vec<value::Value>) -> Result<value::Value, String>;

#[derive(Clone)]
pub struct NativeFunc {
    pub arity: usize,
    pub inner: NativeFuncType,
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
    pub fn new(arity: usize, inner: NativeFuncType) -> Self {
        NativeFunc {
            arity,
            inner,
        }
    }
}

impl NativeFunc {
    fn call(&self, mut interp: &mut interp::Interpreter, args: Vec<value::Value>) -> Result<value::Value, String> {
        let inner = self.inner;
        inner(&mut interp, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

