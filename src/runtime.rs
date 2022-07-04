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
	runtime::{memory::Heap, value::Value},
};

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

	fn type_name(&self) -> &'static str {
		"object"
	}

	fn format(&self, heap: &Heap) -> String {
		let _ = heap;
		self.type_name().to_string()
	}

	fn debug_format(&self, heap: &Heap) -> String {
		let _ = heap;
		format!("{}(?)", self.type_name())
	}

	fn call(&self, heap: &Heap, values: &[Value]) -> Result<Value, PiccoloError> {
		let _ = values;
		Err(PiccoloError::new(ErrorKind::CannotCall {
			callee: self.format(heap),
		}))
	}

	fn index(&self, heap: &Heap, value: Value) -> Result<Value, PiccoloError> {
		let _ = value;
		Err(PiccoloError::new(ErrorKind::CannotIndex {
			object: self.format(heap),
			with: value.format(heap),
		}))
	}

	fn index_assign(
		&mut self,
		heap: &Heap,
		index: Value,
		value: Value,
	) -> Result<(), PiccoloError> {
		let _ = index;
		let _ = value;
		Err(PiccoloError::new(ErrorKind::CannotIndex {
			object: self.format(heap),
			with: value.format(heap),
		}))
	}
}

downcast_rs::impl_downcast!(Object);
