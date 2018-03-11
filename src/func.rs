
use ::*;
use env::Scope;

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
            kind: FuncKind::Normal(NormalFunc { decl, method: false, scope: Scope::new() }),
        }
    }

    pub fn new_method(arity: Arity, decl: stmt::Func) -> Self {
        Func {
            arity,
            kind: FuncKind::Normal(NormalFunc { decl, method: true, scope: Scope::new() }),
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
            FuncKind::Native(ref n) => n.method,
        }
    }

    pub fn bind(self, inst: data::Instance) -> Func {
        match self.kind {
            FuncKind::Normal(n) => Func { kind: FuncKind::Normal(n.bind(inst)), ..self },
            _ => panic!("bind on native func")
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NormalFunc {
    pub decl: stmt::Func,
    pub method: bool,
    pub scope: Scope,
}

impl NormalFunc {
    pub fn bind(self, inst: data::Instance) -> NormalFunc {
        let mut scope = env::Scope::new();
        //inst.all_public(); // panic
        scope.set("me", value::Value::Instance(inst));
        NormalFunc {
            scope, ..self
        }
    }

    pub fn call(&mut self, interp: &mut interp::Interpreter, args: &[value::Value]) -> Result<value::Value, err::PiccoloError> {
        //interp.env.push();

        ////let inst = interp.env.latest_me();
        //let inst = interp.env.get("me");
        //if self.method {
        //    let inst = inst.as_ref().unwrap();
        //    inst.all_public();
        //    interp.env.set_local("me", value::Value::new(value::ValueKind::Instance(inst.clone())));
        //}

        //for (i, arg) in args.iter().enumerate() {
        //    interp.env.set_local(&self.decl.args[i].lexeme, arg.clone());
        //}

        //let result = interp.interpret(&self.decl.body);

        //if self.method {
        //    let inst = inst.as_ref().unwrap();
        //    inst.reset();
        //    //interp.env.pop_me();
        //    interp.env.pop();
        //}

        //interp.env.pop();

        //interp.env.push();
        //let result = if let Some(value::Value::Instance(inst)) = interp.env.get("me") {
        //    inst.all_public();
        //    interp.env.delete("me").unwrap();
        //    interp.env.set_local("me", value::Value::Instance(inst.clone()));
        //    for (i, arg) in args.iter().enumerate() {
        //        interp.env.set_local(&self.decl.args[i].lexeme, arg.clone());
        //    }
        //    let result = interp.interpret(&self.decl.body);
        //    inst.reset();
        //    result
        //} else {

        // TODO - figure out if we're in a method and allow access to
        //        private variables if we are
        interp.env.push();
        for (i, arg) in args.iter().enumerate() {
            interp.env.set_local(&self.decl.args[i].lexeme, arg.clone());
        }
        let mut e = interp.env.append(&self.scope);
        let result = interp.interpret_with(&self.decl.body, &mut e);
        interp.env.pop();
        //};
        //interp.env.pop();

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
    pub method: bool,
}

impl NativeFunc {
    pub fn new(inner: NativeFuncType) -> Self {
        NativeFunc {
            inner,
            method: false,
        }
    }

    pub fn method(self) -> Self {
        NativeFunc {
            method: true,
            ..self
        }
    }

    fn call(&self, mut interp: &mut interp::Interpreter, args: &[value::Value]) -> Result<value::Value, err::PiccoloError> {
        //interp.env.push();
        //let result = if let Some(value::Value::Instance(inst)) = interp.env.get("me") {
        //    inst.all_public();
        //    interp.env.set_local("me", value::Value::Instance(inst.clone()));
            let inner = self.inner;
            let result = inner(&mut interp, args);
        //    inst.reset();
        //    result
        //} else {
        //    let inner = self.inner;
        //    inner(&mut interp, args)
        //};
        //interp.env.pop();
        result
        //interp.env.push();

        ////let inst = interp.env.latest_me();
        //let inst = interp.env.get("me");
        //if self.method {
        //    let inst = inst.as_ref().unwrap().kind;
        //    inst.all_public();
        //    interp.env.set_local("me", value::ValueKind::Instance(inst.clone()));
        //}

        //let inner = self.inner;
        //let result = inner(&mut interp, args);

        //if self.method {
        //    let inst = inst.as_ref().unwrap();
        //    inst.reset();
        //    //interp.env.pop_me();
        //}

        //interp.env.pop();

        //result
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

