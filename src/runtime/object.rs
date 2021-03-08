//! Objects defined in Rust that may exist at runtime.

use crate::runtime::{value::Value, HeapPtr};

use downcast_rs::Downcast;

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
