//! Objects defined in Rust that may exist at runtime.

use crate::runtime::value::Value;

use downcast_rs::Downcast;

use core::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ObjectKind {
    Function,
    Other,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ObjectPtr {
    pub idx: usize,
    pub kind: ObjectKind,
}

/// Trait for Piccolo objects that will be stored in a [`Heap`].
///
/// [`Heap`]: ../memory/struct.Heap.html
pub trait Object: Downcast + fmt::Debug + fmt::Display {
    /// Return the name of the type of the object. Used for runtime type comparison inside
    /// Piccolo via the `type()` builtin function.
    fn type_name(&self) -> &'static str {
        "object"
    }

    fn kind(&self) -> ObjectKind {
        ObjectKind::Other
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn gt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn lt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn eq(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Get the named property from the object. Returns `None` if it doesn't exist.
    fn get(&self, _property: &str) -> Option<Value> {
        None
    }

    /// Sets the named property on the object. Returns `None` if it doesn't exist.
    fn set(&mut self, _property: &str, _value: Value) -> Option<()> {
        None
    }

    /// Attempts to clone the object. Returns `None` if it is not possible.
    fn try_clone(&self) -> Option<Box<dyn Object>> {
        None
    }
}

downcast_rs::impl_downcast!(Object);

#[derive(Debug, Default)]
pub(crate) struct Function {
    arity: usize,
    name: String,
    chunk: super::chunk::Chunk,
}

impl Object for Function {
    fn type_name(&self) -> &'static str {
        "function"
    }

    fn kind(&self) -> ObjectKind {
        ObjectKind::Function
    }

    fn eq(&self, other: &dyn Object) -> Option<bool> {
        if let Some(other) = other.downcast_ref::<Function>() {
            Some(other.name == self.name)
        } else {
            None
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl Function {
    pub(crate) fn arity(&self) -> &usize {
        &self.arity
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn chunk(&self) -> &super::chunk::Chunk {
        &self.chunk
    }

    pub(crate) fn chunk_mut(&mut self) -> &mut super::chunk::Chunk {
        &mut self.chunk
    }

    pub(crate) fn into_chunk(self) -> super::chunk::Chunk {
        self.chunk
    }
}
