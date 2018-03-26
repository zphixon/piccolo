use super::*;
use env::Scope;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use foreign::Foreign;

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum Arity {
    None,
    Multi,
    Some(usize),
}

impl Arity {
    pub fn compatible(&self, other: Arity) -> bool {
        match *self {
            Arity::None => other == Arity::None,
            Arity::Multi => true,
            Arity::Some(own) => match other {
                Arity::Some(n) => n == own,
                Arity::None => false,
                Arity::Multi => panic!("parsed a call with varargs"),
            },
        }
    }

    pub fn to_number(&self) -> usize {
        match *self {
            Arity::None => 0,
            Arity::Multi => panic!("infinity is not a number"),
            Arity::Some(n) => n,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Func {
    pub decl: stmt::Func,
    pub method: bool,
    pub scope: Rc<RefCell<Scope>>,
    pub arity: Arity,
}

impl Func {
    pub fn new(arity: Arity, decl: stmt::Func) -> Self {
        Func {
            arity,
            decl,
            method: false,
            scope: Rc::new(RefCell::new(Scope::new())),
        }
    }

    pub fn new_method(arity: Arity, decl: stmt::Func) -> Self {
        Func {
            arity,
            decl,
            method: true,
            scope: Rc::new(RefCell::new(Scope::new())),
        }
    }

    pub fn new_with_scope(arity: Arity, decl: stmt::Func, scope: Rc<RefCell<Scope>>) -> Self {
        Func {
            arity,
            decl,
            method: false,
            scope,
        }
    }

    pub fn bind(self, inst: data::Instance) -> Func {
        self.scope
            .borrow_mut()
            .set("me", value::Value::Instance(inst));
        self
    }

    pub fn call(
        &mut self,
        interp: &mut interp::Interpreter,
        args: &[value::Value],
    ) -> Result<value::Value, err::PiccoloError> {
        interp.env.push();

        for (i, arg) in args.iter().enumerate() {
            interp.env.set_local(&self.decl.args[i].lexeme, arg.clone());
        }

        let result = interp.interpret_with(&self.decl.body, &mut self.scope.borrow_mut());

        interp.env.pop();

        result.map(|opt| match opt {
            Some(v) => v,
            None => value::Value::Nil,
        })
    }
}

pub type NativeFuncType =
    fn(&mut interp::Interpreter, &[value::Value]) -> Result<value::Value, err::PiccoloError>;

pub struct ForeignFunc {
    pub inner: NativeFuncType,
}

impl Foreign for ForeignFunc {
    fn get_name(&self) -> &'static str {
        "fn"
    }

    fn call(
        &self,
        interp: &mut interp::Interpreter,
        args: &[value::Value],
    ) -> Result<value::Value, err::PiccoloError> {
        (self.inner)(interp, args)
    }
}

impl fmt::Debug for ForeignFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn")
    }
}

impl fmt::Display for ForeignFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn")
    }
}

