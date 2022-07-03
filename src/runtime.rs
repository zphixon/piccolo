//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod builtin;
pub mod chunk;
pub mod memory;
pub mod object;
pub mod op;
pub mod value;
pub mod vm;

pub mod memory2;
pub mod object2;

use memory2::{Heap, Object, Ptr};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    Any,
    Exact(usize),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Function {
    arity: Arity,
    chunk: usize,
    //name: InternedString,
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Nil,
    String(()),
    Bool(bool),
    Double(f64),
    Function(Function),
    Object(Ptr),
}

impl Value {
    pub fn as_ptr(&self) -> Ptr {
        if let Value::Object(ptr) = self {
            *ptr
        } else {
            panic!("called as_ptr on non-object {self:?}");
        }
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
