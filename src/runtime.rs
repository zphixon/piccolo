//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod builtin;
pub mod chunk;
pub mod interner;
pub mod memory;
pub mod op;
pub mod value;
pub mod vm;

use crate::{
    error::{ErrorKind, PiccoloError},
    runtime::{
        interner::{Interner, StringPtr},
        memory::{Heap, Ptr},
        value::Value,
    },
};

#[derive(Clone, Copy)]
pub enum This {
    None,
    Ptr(Ptr),
    String(StringPtr),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    Any,
    Exact(usize),
    AtLeast(usize),
}

impl Default for Arity {
    fn default() -> Self {
        Arity::Exact(0)
    }
}

impl Arity {
    pub fn is_compatible(&self, with: usize) -> bool {
        if let Arity::Exact(arity) = self {
            *arity == with
        } else if let Arity::AtLeast(arity) = self {
            *arity <= with
        } else {
            true
        }
    }
}

impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arity::Any => write!(f, "any"),
            Arity::Exact(n) => write!(f, "{}", n),
            Arity::AtLeast(n) => write!(f, "at least {}", n),
        }
    }
}

// https://stackoverflow.com/a/30353928/18270160
pub trait ObjectClone {
    fn clone_object(&self) -> Box<dyn Object>;
}

impl<T> ObjectClone for T
where
    T: 'static + Object + Clone,
{
    fn clone_object(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

pub trait Object: downcast_rs::Downcast + ObjectClone {
    fn trace(&self, heap: &Heap);

    fn type_name(&self, heap: &Heap) -> &'static str {
        let _ = heap;
        "object"
    }

    fn format(&self, heap: &Heap, interner: &Interner) -> String {
        let _ = interner;
        self.type_name(heap).to_string()
    }

    fn debug_format(&self, heap: &Heap, interner: &Interner) -> String {
        let _ = interner;
        format!("{}(?)", self.type_name(heap))
    }

    fn call(
        &self,
        heap: &Heap,
        interner: &Interner,
        values: &[Value],
    ) -> Result<Value, PiccoloError> {
        let _ = values;
        Err(PiccoloError::new(ErrorKind::CannotCall {
            callee: self.format(heap, interner),
        }))
    }

    fn get(
        &self,
        this: This,
        heap: &Heap,
        interner: &Interner,
        index_value: Value,
    ) -> Result<Value, PiccoloError> {
        let _ = this;
        Err(PiccoloError::new(ErrorKind::CannotGet {
            object: self.type_name(heap).to_string(),
            index: index_value.format(heap, interner),
        }))
    }

    fn set(
        &mut self,
        heap: &Heap,
        interner: &Interner,
        index_value: Value,
        value: Value,
    ) -> Result<(), PiccoloError> {
        let _ = value;
        Err(PiccoloError::new(ErrorKind::CannotGet {
            object: self.type_name(heap).to_string(),
            index: index_value.format(heap, interner),
        }))
    }

    fn eq(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        let _ = other;
        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }

    fn lt(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        let _ = other;
        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }

    fn gt(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        let _ = other;
        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }
}

downcast_rs::impl_downcast!(Object);
