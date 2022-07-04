use crate::{
    error::{ErrorKind, PiccoloError},
    runtime::{interner::StringPtr, memory::Heap, Arity, Object, Value},
};
use std::fmt::{Debug, Write};

pub fn to_string(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    let mut s = String::new();

    for (i, value) in values.iter().enumerate() {
        write!(s, "{}", value.format(heap))?;

        if i + 1 != values.len() {
            s.push('\t');
        }
    }

    let ptr = heap.alloc_string(s);

    Ok(Value::String(ptr))
}

pub fn print(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    match to_string(heap, values)? {
        Value::String(ptr) => {
            println!("{}", heap.get_string(ptr).unwrap());
        }
        _ => unreachable!(),
    }

    Ok(Value::Nil)
}

pub fn rand(_: &mut Heap, _: &[Value]) -> Result<Value, PiccoloError> {
    Ok(Value::Double(rand::random()))
}

pub type PiccoloFunction = fn(&mut Heap, &[Value]) -> Result<Value, PiccoloError>;

#[derive(Clone, Copy)]
pub struct NativeFunction {
    pub name: StringPtr,
    pub arity: Arity,
    pub ptr: PiccoloFunction,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl NativeFunction {
    pub fn new(name: StringPtr, arity: Arity, ptr: PiccoloFunction) -> Self {
        NativeFunction { name, arity, ptr }
    }

    pub fn name(&self) -> StringPtr {
        self.name
    }

    pub fn call(&self, heap: &mut Heap, args: &[Value]) -> Result<Value, PiccoloError> {
        if !self.arity.is_compatible(args.len()) {
            return Err(PiccoloError::new(ErrorKind::IncorrectArity {
                name: heap.get_string(self.name).unwrap().to_string(),
                exp: self.arity,
                got: args.len(),
            }));
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
