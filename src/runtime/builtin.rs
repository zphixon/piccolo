use crate::{
    error::{ErrorKind, PiccoloError},
    runtime::{memory2::Heap, Arity, Ptr, Value},
};
use std::fmt::{Debug, Write};

pub fn print(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    let mut s = String::new();

    for (i, value) in values.iter().enumerate() {
        match value {
            Value::Bool(b) => write!(s, "{b}").unwrap(),
            Value::Integer(i) => write!(s, "{i}").unwrap(),
            Value::Double(d) => write!(s, "{d}").unwrap(),
            Value::String(_) => s.push_str("string todo"),
            Value::Function(f) => write!(s, "<func {}>", heap.get_string(f.name).unwrap()).unwrap(),
            Value::NativeFunction(f) => {
                write!(s, "<native func {}>", heap.get_string(f.name).unwrap()).unwrap()
            }
            Value::Object(ptr) => write!(s, "{}", heap.get(*ptr).unwrap().format(heap)).unwrap(),
            Value::Nil => s.push_str("nil"),
        }

        if i != values.len() {
            s.push('\t');
        }
    }

    println!("{s}");

    Ok(Value::Nil)
}

pub fn rand(_: &mut Heap, _: &[Value]) -> Result<Value, PiccoloError> {
    Ok(Value::Double(rand::random()))
}

pub type PiccoloFunction = fn(&mut Heap, &[Value]) -> Result<Value, PiccoloError>;

#[derive(Clone, Copy)]
pub struct NativeFunction {
    name: Ptr,
    arity: Arity,
    ptr: PiccoloFunction,
}

impl NativeFunction {
    pub fn new(name: Ptr, arity: Arity, ptr: PiccoloFunction) -> Self {
        NativeFunction { name, arity, ptr }
    }

    pub fn call(&self, heap: &mut Heap, args: &[Value]) -> Result<Value, PiccoloError> {
        if let Arity::Exact(arity) = self.arity {
            if arity != args.len() {
                return Err(PiccoloError::new(ErrorKind::IncorrectArity {
                    name: heap.get_string(self.name).unwrap().to_string(),
                    exp: arity,
                    got: args.len(),
                }));
            }
        }

        (self.ptr)(heap, args)
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("arity", &self.arity)
            .field("ptr", &(self.ptr as *const fn() as usize))
            .finish()
    }
}
