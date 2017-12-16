
use ::*;

#[derive(Clone)]
pub struct Func {
    inner: fn(&mut interp::Interpreter, Vec<value::Value>) -> value::Value,
    arity: usize,
}

impl std::fmt::Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Func {{ inner: ..., arity: {} }}", self.arity)
    }
}

impl std::cmp::PartialEq for Func {
    fn eq(&self, _other: &Func) -> bool { false }
}

impl Func {
    pub fn new(arity: usize, inner: fn(&mut interp::Interpreter, Vec<value::Value>) -> value::Value) -> Self {
        Func {
            inner,
            arity,
        }
    }

    pub fn call(&self, mut i: &mut interp::Interpreter, args: Vec<value::Value>) -> value::Value {
        let inner = self.inner;
        inner(&mut i, args)
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}

