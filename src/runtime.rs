//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod builtin;
pub mod chunk;
pub mod memory;
pub mod object;
pub mod op;
pub mod value;
pub mod vm;

pub mod interner;
pub mod memory2;
pub mod vm2;

use interner::StringPtr;
use memory2::{Heap, Object, Ptr};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    Any,
    Exact(usize),
}

impl Arity {
    pub fn is_compatible(&self, with: usize) -> bool {
        if let Arity::Exact(arity) = self {
            *arity == with
        } else {
            true
        }
    }

    pub fn number(&self) -> usize {
        if let Arity::Exact(arity) = self {
            *arity
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Function {
    arity: Arity,
    chunk: usize,
    name: StringPtr,
}

#[derive(Clone, Copy)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(StringPtr),
    Function(Function),
    NativeFunction(builtin::NativeFunction),
    Object(Ptr),
    Nil,
}

impl Value {
    pub fn as_ptr(&self) -> Ptr {
        if let Value::Object(ptr) = self {
            *ptr
        } else {
            panic!("called as_ptr on non-object");
        }
    }

    pub fn from_constant(c: value::Constant, heap: &mut Heap) -> Value {
        use value::Constant;
        match c {
            Constant::Bool(b) => Value::Bool(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Double(d) => Value::Double(d),
            Constant::String(s) => Value::String(heap.alloc_string(s)),
            Constant::Function(f) => Value::Function(Function {
                arity: Arity::Exact(f.arity()),
                chunk: f.chunk(),
                name: heap.alloc_string(f.name().to_string()),
            }),
            //Constant::NativeFunction(f) => Value::NativeFunction(f),
            //Constant::Object(Box<dyn Object>) => {} // TODO?
            Constant::Nil => Value::Nil,
            _ => todo!(),
        }
    }

    pub fn to_string(&self, heap: &Heap) -> String {
        self.format(heap)
    }
}

impl Object for Value {
    fn trace(&self, heap: &Heap) {
        match self {
            Value::Object(ptr) => {
                heap.trace(*ptr);
            }
            _ => {}
        }
    }

    fn type_name(&self, heap: &Heap) -> &'static str {
        "value"
    }

    fn format(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(b) => format!("{b}"),
            Value::Integer(i) => format!("{i}"),
            Value::Double(d) => format!("{d}"),
            Value::String(p) => heap.get_string(*p).unwrap().to_string(),
            Value::Function(f) => heap.get_string(f.name).unwrap().to_string(),
            Value::NativeFunction(f) => heap.get_string(f.name()).unwrap().to_string(),
            Value::Object(p) => heap.get(*p).unwrap().format(heap),
            Value::Nil => String::from("nil"),
        }
    }

    fn debug_format(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(v) => format!("Bool({v})"),
            Value::Integer(v) => format!("Integer({v})"),
            Value::Double(v) => format!("Double({v})"),
            Value::String(v) => format!("String({v:?})"),
            Value::Function(f) => format!("Function({:?})", heap.get_string(f.name).unwrap()),
            Value::NativeFunction(f) => {
                format!("NativeFunction({:?})", heap.get_string(f.name()).unwrap())
            }
            Value::Object(p) => format!("Object({})", heap.get(*p).unwrap().debug_format(heap)),
            Value::Nil => String::from("nil"),
        }
    }
}

#[derive(Default)]
pub struct Stack {
    values: Vec<Value>,
}

impl Stack {
    pub fn as_collectable(&self) -> impl Iterator<Item = Option<&Ptr>> {
        self.values.iter().map(|value| {
            if let Value::Object(ptr) = value {
                Some(ptr)
            } else {
                None
            }
        })
    }

    pub fn push(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn peek(&self, index: usize) -> Option<Value> {
        self.values.get(self.values.len() - index - 1).cloned()
    }
}

impl std::ops::Index<usize> for Stack {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}
