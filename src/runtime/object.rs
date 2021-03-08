//! Objects defined in Rust that may exist at runtime.

use crate::runtime::memory::Object;

use core::fmt;

#[derive(Debug, Default)]
pub(crate) struct Function {
    arity: usize,
    name: String,
    chunk: super::chunk::Chunk,
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

impl Object for Function {
    fn trace(&self) {
        // TODO: constants?
    }

    fn type_name(&self) -> &'static str {
        "func"
    }
}

impl<T: Object> Object for std::cell::RefCell<T> {
    fn trace(&self) {
        self.borrow().trace();
    }
}

impl<T: Object> Object for Vec<T> {
    fn trace(&self) {
        for item in self {
            item.trace();
        }
    }
}

impl<T: Object> Object for &Vec<T> {
    fn trace(&self) {
        for item in *self {
            item.trace();
        }
    }
}

impl<K: Eq + std::hash::Hash, T: Object> Object for std::collections::HashMap<K, T> {
    fn trace(&self) {
        for value in self.values() {
            value.trace();
        }
    }
}

impl Object for String {
    fn trace(&self) {}
}
