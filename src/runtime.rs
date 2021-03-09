//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod chunk;
pub mod memory;
pub mod object;
pub mod op;
pub mod value;
pub mod vm;

pub type ConstantIdx = u16;
pub type LocalSlotIdx = u16;
pub type LocalScopeDepth = u16;
pub type Line = usize;
pub type ChunkOffset = usize;
