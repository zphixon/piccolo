//! Objects defined in Rust that may exist at runtime.
use super::chunk::Chunk;

use crate::runtime::memory::Object;
use crate::runtime::ChunkIndex;

use core::fmt;

#[derive(Default)]
pub struct Function {
    arity: usize,
    name: String,
    chunk: ChunkIndex,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        trace!("ufveowsi");
        write!(f, "<fn {}>", self.name)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        trace!("hLLEOO");
        write!(f, "<fn {}>", self.name)
    }
}

impl Function {
    pub fn new(arity: usize, name: String, chunk: ChunkIndex) -> Self {
        Self { arity, name, chunk }
    }

    pub fn arity(&self) -> &usize {
        &self.arity
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn chunk(&self) -> ChunkIndex {
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

    fn type_name(&self) -> &'static str {
        "list"
    }
}

impl<T: Object> Object for &Vec<T> {
    fn trace(&self) {
        for item in *self {
            item.trace();
        }
    }

    fn type_name(&self) -> &'static str {
        "list"
    }
}

impl<K: Eq + std::hash::Hash, T: Object> Object for fnv::FnvHashMap<K, T> {
    fn trace(&self) {
        for value in self.values() {
            value.trace();
        }
    }

    fn type_name(&self) -> &'static str {
        "map"
    }
}

impl Object for String {
    fn trace(&self) {}

    fn type_name(&self) -> &'static str {
        "string"
    }
}
