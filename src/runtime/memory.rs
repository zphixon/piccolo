use crate::{
	error::PiccoloError,
	runtime::{
		builtin::{self, NativeFunction},
		interner::{Interner, StringPtr},
		value::Value,
		Arity, Object,
	},
};
use fnv::FnvHashMap;
use slotmap::{DefaultKey, SlotMap};
use std::{
	fmt::Debug,
	hash::BuildHasherDefault,
	sync::atomic::{AtomicBool, Ordering},
};

pub struct ObjectHeader {
	inner: Box<dyn Object>,
	marked: AtomicBool,
}

#[derive(Copy, Clone, Debug)]
pub struct Ptr(DefaultKey);

impl Ptr {
	pub fn null() -> Ptr {
		use slotmap::Key;
		Ptr(DefaultKey::null())
	}
}

pub struct Heap {
	objects: SlotMap<DefaultKey, ObjectHeader>,
	native_functions: FnvHashMap<&'static str, NativeFunction>,
	interner: Interner,
}

impl Default for Heap {
	fn default() -> Self {
		Self::new()
	}
}

impl Heap {
	pub fn new() -> Heap {
		let mut heap = Heap {
			objects: Default::default(),
			native_functions: FnvHashMap::with_capacity_and_hasher(
				0,
				BuildHasherDefault::default(),
			),
			interner: Default::default(),
		};
		let mut native_functions = FnvHashMap::default();

		native_functions.insert(
			"print",
			NativeFunction::new(
				heap.alloc_string(String::from("print")),
				Arity::Any,
				builtin::print,
			),
		);

		native_functions.insert(
			"rand",
			NativeFunction::new(
				heap.alloc_string(String::from("rand")),
				Arity::Exact(0),
				builtin::rand,
			),
		);

		native_functions.insert(
			"toString",
			NativeFunction::new(
				heap.alloc_string(String::from("toString")),
				Arity::Any,
				builtin::to_string,
			),
		);

		heap.native_functions = native_functions;
		heap
	}

	pub fn null_ptr() -> Ptr {
		Ptr::null()
	}

	pub fn allocate(&mut self, object: impl Object) -> Ptr {
		Ptr(self.objects.insert(ObjectHeader {
			inner: Box::new(object),
			marked: AtomicBool::new(false),
		}))
	}

	pub fn get(&self, ptr: Ptr) -> Option<&dyn Object> {
		self.get_header(ptr).map(|header| header.inner.as_ref())
	}

	pub fn get_mut(&mut self, ptr: Ptr) -> Option<&mut dyn Object> {
		self.get_header_mut(ptr).map(|header| header.inner.as_mut())
	}

	fn get_header(&self, ptr: Ptr) -> Option<&ObjectHeader> {
		self.objects.get(ptr.0)
	}

	fn get_header_mut(&mut self, ptr: Ptr) -> Option<&mut ObjectHeader> {
		self.objects.get_mut(ptr.0)
	}

	pub fn take(&mut self, ptr: Ptr) -> Option<Box<dyn Object>> {
		self.objects.remove(ptr.0).map(|header| header.inner)
	}

	pub fn collect<'iter>(&mut self, roots: impl Iterator<Item = Option<&'iter Ptr>>) {
		self.objects
			.iter_mut()
			.for_each(|(_, header)| header.marked.store(false, Ordering::SeqCst));

		for root in roots.flatten() {
			self.trace(*root);
		}

		self.objects
			.retain(|_, header| header.marked.load(Ordering::SeqCst));
	}

	pub fn call_native(&mut self, name: &str, args: &[Value]) -> Result<Value, PiccoloError> {
		let f = self.native_functions[name];
		f.call(self, args)
	}

	pub fn trace(&self, ptr: Ptr) {
		if let Some(header) = self.get_header(ptr) {
			header.marked.store(true, Ordering::SeqCst);
			header.inner.trace(self);
		}
	}

	pub fn object_as<T>(&self, ptr: Ptr) -> Option<&T>
	where
		T: Object,
	{
		self.get(ptr)?.downcast_ref::<T>()
	}

	pub fn alloc_string(&mut self, string: String) -> StringPtr {
		self.interner.alloc_string(string)
	}

	pub fn get_string(&self, ptr: StringPtr) -> Option<&str> {
		self.interner.get_string(ptr)
	}

	pub fn get_native(&self, name: &str) -> Option<NativeFunction> {
		self.native_functions.get(name).copied()
	}

	pub fn clone(&mut self, ptr: Ptr) -> Ptr {
		Ptr(self.objects.insert(ObjectHeader {
			inner: self.get(ptr).unwrap().clone_object(),
			marked: AtomicBool::new(false),
		}))
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn simple_collect() {
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
		}
		impl std::ops::Index<usize> for Stack {
			type Output = Value;
			fn index(&self, index: usize) -> &Self::Output {
				&self.values[index]
			}
		}

		#[derive(Debug, Default)]
		struct O(AtomicBool);
		impl Object for O {
			fn trace(&self, _: &Heap) {
				self.0.store(true, Ordering::SeqCst);
			}
		}
		impl crate::runtime::ObjectClone for O {
			fn clone_object(&self) -> Box<dyn Object> {
				Box::new(O(AtomicBool::new(self.0.load(Ordering::SeqCst))))
			}
		}

		let mut heap = Heap::new();
		let mut stack = Stack::default();

		let unrooted = heap.allocate(O::default());
		stack.push(Value::Object(heap.allocate(O::default())));

		heap.collect(stack.as_collectable());

		assert!(heap.get(stack[0].as_ptr()).is_some());
		assert!(heap
			.get_header(stack[0].as_ptr())
			.unwrap()
			.marked
			.load(Ordering::SeqCst));
		assert!(heap.get_header(unrooted).is_none());

		assert!(heap
			.object_as::<O>(stack[0].as_ptr())
			.unwrap()
			.0
			.load(Ordering::SeqCst));
	}

	#[test]
	fn more_complicated() {
		#[derive(Debug, Clone)]
		struct Cons {
			car: i32,
			cdr: Ptr,
		}
		impl Object for Cons {
			fn trace(&self, heap: &Heap) {
				heap.trace(self.cdr);
			}

			fn type_name(&self) -> &'static str {
				"cons"
			}

			fn format(&self, heap: &Heap) -> String {
				format!(
					"(cons {} {})",
					self.car,
					heap.get(self.cdr)
						.map(|object| object.format(heap))
						.unwrap_or_else(|| String::from("nil"))
				)
			}
		}

		let mut heap = Heap::new();
		let inner1 = heap.allocate(Cons {
			car: 1,
			cdr: Heap::null_ptr(),
		});
		let inner2 = heap.allocate(Cons {
			car: 2,
			cdr: inner1,
		});
		let root = heap.allocate(Cons {
			car: 3,
			cdr: inner2,
		});

		assert_eq!(
			"(cons 3 (cons 2 (cons 1 nil)))",
			heap.get(root).unwrap().format(&heap)
		);

		heap.collect([Some(&inner2)].into_iter());
		assert_eq!(
			"(cons 2 (cons 1 nil))",
			heap.get(inner2).unwrap().format(&heap)
		);

		heap.collect([Some(&inner1)].into_iter());
		assert_eq!("(cons 1 nil)", heap.get(inner1).unwrap().format(&heap));

		heap.collect([].into_iter());
		assert!(heap.get(inner1).is_none());
	}

	#[test]
	fn arity() {
		let mut heap = Heap::new();
		assert!(heap.call_native("print", &[]).is_ok());
		assert!(heap.call_native("print", &[Value::Nil]).is_ok());

		assert!(heap.call_native("rand", &[]).is_ok());
		assert!(heap.call_native("rand", &[Value::Nil]).is_err());
	}
}
