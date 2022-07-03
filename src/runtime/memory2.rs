use crate::{
    error::{ErrorKind, PiccoloError},
    runtime::{
        builtin::{self, NativeFunction},
        interner::Interner,
        Arity, Value,
    },
};
use fnv::FnvHashMap;
use slotmap::SlotMap;
use std::{
    fmt::Debug,
    sync::atomic::{AtomicBool, Ordering},
};

pub trait Object: Debug + downcast_rs::Downcast {
    fn trace(&self, heap: &Heap);

    fn type_name(&self) -> &'static str {
        "object"
    }

    fn format(&self, heap: &Heap) -> String {
        let _ = heap;
        format!("{self:?}")
    }

    fn call(&self, heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
        let _ = values;
        Err(PiccoloError::new(ErrorKind::CannotCall {
            callee: self.format(heap),
        }))
    }
}

downcast_rs::impl_downcast!(Object);

#[derive(Debug)]
pub struct ObjectHeader {
    inner: Box<dyn Object>,
    marked: AtomicBool,
}

pub type Ptr = slotmap::DefaultKey;

#[derive(Default)]
pub struct Heap {
    objects: SlotMap<Ptr, ObjectHeader>,
    native_functions: FnvHashMap<&'static str, NativeFunction>,
    interner: Interner,
}

impl Heap {
    pub fn new() -> Heap {
        let mut heap = Heap::default();
        let mut native_functions = FnvHashMap::default();

        native_functions.insert(
            "print",
            NativeFunction::new(
                heap.alloc_string(String::from("print")),
                Arity::Any,
                builtin::rand,
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

        heap.native_functions = native_functions;
        heap
    }

    pub fn null_ptr() -> Ptr {
        use slotmap::Key;
        Ptr::null()
    }

    pub fn allocate(&mut self, object: impl Object) -> Ptr {
        self.objects.insert(ObjectHeader {
            inner: Box::new(object),
            marked: AtomicBool::new(false),
        })
    }

    pub fn get(&self, ptr: Ptr) -> Option<&dyn Object> {
        self.get_header(ptr).map(|header| header.inner.as_ref())
    }

    fn get_header(&self, ptr: Ptr) -> Option<&ObjectHeader> {
        self.objects.get(ptr)
    }

    pub fn take(&mut self, ptr: Ptr) -> Option<Box<dyn Object>> {
        self.objects.remove(ptr).map(|header| header.inner)
    }

    pub fn collect<'iter>(&mut self, roots: impl Iterator<Item = Option<&'iter Ptr>>) {
        self.objects
            .iter_mut()
            .for_each(|(_, header)| header.marked.store(false, Ordering::SeqCst));

        for root in roots {
            if let Some(&root) = root {
                self.trace(root);
            }
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

    pub fn alloc_string(&mut self, string: String) -> Ptr {
        self.interner.alloc_string(string)
    }

    pub fn get_string(&self, ptr: Ptr) -> Option<&str> {
        self.interner.get_string(ptr)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_collect() {
        #[derive(Debug, Default)]
        struct O(AtomicBool);
        impl Object for O {
            fn trace(&self, _: &Heap) {
                self.0.store(true, Ordering::SeqCst);
            }
        }

        let mut heap = Heap::new();
        let mut stack = crate::runtime::Stack::default();

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
        #[derive(Debug)]
        struct Cons {
            car: i32,
            cdr: Ptr,
        }
        impl Object for Cons {
            fn trace(&self, heap: &Heap) {
                heap.trace(self.cdr);
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
