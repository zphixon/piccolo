use crate::{
    error::{ErrorKind, PiccoloError},
    runtime::{
        interner::StringPtr,
        memory::{Heap, Ptr},
        Arity, Object, Value,
    },
};
use once_cell::sync::Lazy;
use std::{
    fmt::{Debug, Write},
    time::{Duration, Instant},
};

pub fn to_string(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    let mut s = String::new();

    for (i, value) in values.iter().enumerate() {
        write!(s, "{}", value.format(heap))?;

        if i + 1 != values.len() {
            s.push('\t');
        }
    }

    let ptr = heap.interner_mut().allocate_string(s);

    Ok(Value::String(ptr))
}

pub fn print(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    match to_string(heap, values)? {
        Value::String(ptr) => {
            println!("{}", heap.interner().get_string(ptr));
        }
        _ => unreachable!(),
    }

    Ok(Value::Nil)
}

pub fn rand(_: &mut Heap, _: &[Value]) -> Result<Value, PiccoloError> {
    Ok(Value::Double(rand::random()))
}

pub fn clone(heap: &mut Heap, args: &[Value]) -> Result<Value, PiccoloError> {
    let arg = args[0];
    match arg {
        // TODO when we have closures
        // Value::Function(f) => {}
        Value::Object(ptr) => return Ok(Value::Object(heap.clone(ptr))),
        _ => {}
    }
    Ok(arg)
}

pub fn type_(heap: &mut Heap, args: &[Value]) -> Result<Value, PiccoloError> {
    let arg = args[0];
    let name = arg.type_name(heap);
    Ok(Value::String(heap.interner_mut().allocate_str(name)))
}

static START: Lazy<Instant> = Lazy::new(Instant::now);
pub fn clock(_: &mut Heap, _: &[Value]) -> Result<Value, PiccoloError> {
    let duration = Instant::now() - *START;
    Ok(Value::Double(duration.as_secs_f64()))
}

pub fn sleep(heap: &mut Heap, args: &[Value]) -> Result<Value, PiccoloError> {
    if let Value::Integer(seconds) = args[0] {
        if let Ok(seconds) = seconds.try_into() {
            std::thread::sleep(Duration::from_secs(seconds));
            return Ok(Value::Nil);
        }

        return Err(PiccoloError::new(ErrorKind::InvalidArgument {
            exp: "non-negative integer".to_string(),
            got: args[0].format(heap),
        }));
    }

    Err(PiccoloError::new(ErrorKind::IncorrectType {
        exp: "integer".to_string(),
        got: args[0].type_name(heap).to_string(),
        op: super::op::Opcode::Call(0),
    }))
}

pub type PiccoloFunction = fn(&mut Heap, &[Value]) -> Result<Value, PiccoloError>;

#[derive(Clone, Copy)]
pub struct BuiltinFunction {
    pub name: StringPtr,
    pub arity: Arity,
    pub ptr: PiccoloFunction,
    pub this: Option<Ptr>,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl BuiltinFunction {
    pub fn new(name: StringPtr, arity: Arity, ptr: PiccoloFunction) -> Self {
        BuiltinFunction {
            name,
            arity,
            ptr,
            this: None,
        }
    }

    pub fn name(&self) -> StringPtr {
        self.name
    }

    pub fn call(&self, heap: &mut Heap, args: &[Value]) -> Result<Value, PiccoloError> {
        if !self.arity.is_compatible(args.len()) {
            return Err(PiccoloError::new(ErrorKind::IncorrectArity {
                name: heap.interner().get_string(self.name).to_string(),
                exp: self.arity,
                got: args.len(),
            }));
        }

        (self.ptr)(heap, args)
    }
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction")
            .field("arity", &self.arity)
            .field("ptr", &(self.ptr as *const fn() as usize))
            .finish()
    }
}
