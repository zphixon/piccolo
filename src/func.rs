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

//#[derive(Clone, PartialEq, Debug)]
//pub struct Func {
//    pub kind: FuncKind,
//    pub arity: Arity,
//}
//
//#[derive(Clone, PartialEq, Debug)]
//pub enum FuncKind {
//    Native(NativeFunc),
//    Normal(NormalFunc),
//}
//
//impl Func {
//    pub fn new(arity: Arity, decl: stmt::Func) -> Self {
//        Func {
//            arity,
//            kind: FuncKind::Normal(NormalFunc {
//                decl,
//                method: false,
//                scope: Rc::new(RefCell::new(Scope::new())),
//            }),
//        }
//    }
//
//    pub fn new_method(arity: Arity, decl: stmt::Func) -> Self {
//        Func {
//            arity,
//            kind: FuncKind::Normal(NormalFunc {
//                decl,
//                method: true,
//                scope: Rc::new(RefCell::new(Scope::new())),
//            }),
//        }
//    }
//
//    pub fn new_with_scope(arity: Arity, decl: stmt::Func, scope: Rc<RefCell<Scope>>) -> Self {
//        Func {
//            arity,
//            kind: FuncKind::Normal(NormalFunc {
//                decl,
//                method: false,
//                scope,
//            }),
//        }
//    }
//
//    pub fn new_native(arity: Arity, native: NativeFunc) -> Self {
//        Func {
//            arity,
//            kind: FuncKind::Native(native),
//        }
//    }
//
//    pub fn call(
//        &mut self,
//        interp: &mut interp::Interpreter,
//        args: &[value::Value],
//    ) -> Result<value::Value, err::PiccoloError> {
//        match self.kind {
//            FuncKind::Normal(ref mut f) => f.call(interp, args),
//            FuncKind::Native(ref mut f) => f.call(interp, args),
//        }
//    }
//
//    pub fn is_native(&self) -> bool {
//        match self.kind {
//            FuncKind::Native(_) => true,
//            _ => false,
//        }
//    }
//
//    pub fn is_method(&self) -> bool {
//        match self.kind {
//            FuncKind::Normal(ref n) => n.method,
//            FuncKind::Native(ref n) => n.method,
//        }
//    }
//
//    pub fn bind(self, inst: data::Instance) -> Func {
//        match self.kind {
//            FuncKind::Normal(n) => Func {
//                kind: FuncKind::Normal(n.bind(inst)),
//                ..self
//            },
//            _ => panic!("bind on native func"),
//        }
//    }
//}

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
        //let mut scope = Rc::new(RefCell::new(env::Scope::new()));
        self.scope
            .borrow_mut()
            .set("me", value::Value::Instance(inst));
        self
        //NormalFunc { scope, ..self }
    }

    pub fn call(
        &mut self,
        interp: &mut interp::Interpreter,
        args: &[value::Value],
    ) -> Result<value::Value, err::PiccoloError> {
        //println!("what?");
        interp.env.push();

        for (i, arg) in args.iter().enumerate() {
            interp.env.set_local(&self.decl.args[i].lexeme, arg.clone());
        }

        let result = interp.interpret_with(&self.decl.body, &mut self.scope.borrow_mut());

        //println!("{}", self.scope);
        //let result = interp.interpret_with(&self.decl.body, &mut self.scope.borrow_mut());
        //println!("{}", self.scope);
        //let mut e = interp.env.append(&self.scope);
        //let result = interp.interpret(&self.decl.body);
        //let result = interp.interpret_with(&self.decl.body, &mut e);
        //println!("{}\n{}", self.scope, e);
        //self.scope = e;

        interp.env.pop();
        //println!("{}", self.scope);

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


//
//#[derive(Clone)]
//pub struct NativeFunc {
//    pub inner: NativeFuncType,
//    //pub method: bool,
//}
//
//impl NativeFunc {
//    pub fn new(inner: NativeFuncType) -> Self {
//        NativeFunc {
//            inner,
//            method: false,
//        }
//    }
//
//    //pub fn method(self) -> Self {
//    //    NativeFunc {
//    //        method: true,
//    //        ..self
//    //    }
//    //}
//
//    fn call(
//        &self,
//        mut interp: &mut interp::Interpreter,
//        args: &[value::Value],
//    ) -> Result<value::Value, err::PiccoloError> {
//        let inner = self.inner;
//        inner(&mut interp, args)
//    }
//}
//
//impl std::fmt::Debug for NativeFunc {
//    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        write!(f, "native func")
//    }
//}
//
//impl std::cmp::PartialEq for NativeFunc {
//    fn eq(&self, _other: &NativeFunc) -> bool {
//        false
//    }
//}
