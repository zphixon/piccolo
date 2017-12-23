
use ::*;

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
                Arity::Multi => panic!("parsed a call with varargs")
            }
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
    pub kind: FuncKind,
    pub arity: Arity,
}

#[derive(Clone, PartialEq, Debug)]
pub enum FuncKind {
    Native(NativeFunc),
    Normal(NormalFunc),
}

impl Func {
    pub fn new(arity: Arity, decl: stmt::Func) -> Self {
        Func {
            arity,
            kind: FuncKind::Normal(NormalFunc { decl, method: false }),
        }
    }

    pub fn new_method(arity: Arity, decl: stmt::Func) -> Self {
        Func {
            arity,
            kind: FuncKind::Normal(NormalFunc { decl, method: true }),
        }
    }

    pub fn new_native(arity: Arity, native: NativeFunc) -> Self {
        Func {
            arity,
            kind: FuncKind::Native(native),
        }
    }

    pub fn call(&mut self, interp: &mut interp::Interpreter, args: &[value::Value]) -> Result<value::Value, err::PiccoloError> {
        match self.kind {
            FuncKind::Normal(ref mut f) => f.call(interp, args),
            FuncKind::Native(ref mut f) => f.call(interp, args),
        }
    }

    pub fn is_native(&self) -> bool {
        match self.kind {
            FuncKind::Native(_) => true,
            _ => false,
        }
    }

    pub fn is_method(&self) -> bool {
        match self.kind {
            FuncKind::Normal(ref n) => n.method,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NormalFunc {
    pub decl: stmt::Func,
    pub method: bool,
}

impl NormalFunc {
    pub fn call(&mut self, interp: &mut interp::Interpreter, args: &[value::Value]) -> Result<value::Value, err::PiccoloError> {
        interp.env.push();

        let inst = interp.env.latest_me();
        if self.method {
            let inst = inst.as_ref().unwrap();
            inst.all_public();
            interp.env.set_local("me", value::Value::Instance(inst.clone()));
        }

        for (i, arg) in args.iter().enumerate() {
            interp.env.set_local(&self.decl.args[i].lexeme, arg.clone());
        }

        let result = interp.interpret(&self.decl.body);

        if self.method {
            let inst = inst.as_ref().unwrap();
            inst.reset();
        }

        interp.env.pop();

        result.map(|opt| match opt {
            Some(v) => v,
            None => value::Value::Nil,
        })
    }
}

pub type NativeFuncType = fn(&mut interp::Interpreter, &[value::Value]) -> Result<value::Value, err::PiccoloError>;

#[derive(Clone)]
pub struct NativeFunc {
    pub inner: NativeFuncType,
}

impl NativeFunc {
    pub fn new(inner: NativeFuncType) -> Self {
        NativeFunc {
            inner,
        }
    }

    fn call(&self, mut interp: &mut interp::Interpreter, args: &[value::Value]) -> Result<value::Value, err::PiccoloError> {
        let inner = self.inner;
        inner(&mut interp, args)
    }
}

impl std::fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "native func")
    }
}

impl std::cmp::PartialEq for NativeFunc {
    fn eq(&self, _other: &NativeFunc) -> bool { false }
}

