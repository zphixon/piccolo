use crate::runtime::{
    interner::{Interner, StringPtr},
    Object,
};
use slotmap::{DefaultKey, SlotMap};
use std::{
    cell::UnsafeCell,
    fmt::Debug,
    sync::atomic::{AtomicBool, Ordering},
};

pub struct ObjectHeader {
    inner: UnsafeCell<Box<dyn Object>>,
    marked: AtomicBool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Ptr(DefaultKey);

impl Ptr {
    pub fn null() -> Ptr {
        use slotmap::Key;
        Ptr(DefaultKey::null())
    }
}

#[derive(Default)]
pub struct Heap {
    objects: SlotMap<DefaultKey, ObjectHeader>,
    interner: Interner,
}

impl Heap {
    pub fn new() -> Heap {
        Self::default()
    }

    pub fn null_ptr() -> Ptr {
        Ptr::null()
    }

    pub fn allocate(&mut self, object: impl Object) -> Ptr {
        Ptr(self.objects.insert(ObjectHeader {
            inner: UnsafeCell::new(Box::new(object)),
            marked: AtomicBool::new(false),
        }))
    }

    pub fn get(&self, ptr: Ptr) -> &dyn Object {
        self.get_header(ptr)
            .and_then(|header| unsafe { header.inner.get().as_ref() })
            .map(|inner| inner.as_ref())
            .expect("invalid pointer")
    }

    pub unsafe fn get_mut<'this>(&'this self, ptr: Ptr) -> &'this mut dyn Object {
        if self.objects.contains_key(ptr.0) {
            // this is still 100% wrong, since we can use this method to mutably alias Objects
            let header = self.objects.get_unchecked(ptr.0);
            header
                .inner
                .get()
                .as_mut()
                .expect("invalid pointer")
                .as_mut()
        } else {
            panic!("invalid pointer");
        }
    }

    fn get_header(&self, ptr: Ptr) -> Option<&ObjectHeader> {
        self.objects.get(ptr.0)
    }

    pub fn take(&mut self, ptr: Ptr) -> Option<Box<dyn Object>> {
        self.objects
            .remove(ptr.0)
            .map(|header| header.inner.into_inner())
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

    pub fn trace(&self, ptr: Ptr) {
        if let Some(header) = self.get_header(ptr) {
            header.marked.store(true, Ordering::SeqCst);
            unsafe {
                header.inner.get().as_ref().unwrap().trace(self);
            }
        }
    }

    pub fn object_as<T>(&self, ptr: Ptr) -> Option<&T>
    where
        T: Object,
    {
        self.get(ptr).downcast_ref::<T>()
    }

    pub fn allocate_string(&mut self, string: String) -> StringPtr {
        self.interner.allocate_string(string)
    }

    pub fn get_string(&self, ptr: StringPtr) -> &str {
        self.interner.get_string(ptr)
    }

    pub fn get_string_ptr(&self, string: &str) -> Option<StringPtr> {
        self.interner.get_string_ptr(string)
    }

    pub fn clone(&mut self, ptr: Ptr) -> Ptr {
        Ptr(self.objects.insert(ObjectHeader {
            inner: UnsafeCell::new(self.get(ptr).clone_object()),
            marked: AtomicBool::new(false),
        }))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::runtime::Value;

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

        assert!(heap.get_header(stack[0].as_ptr()).is_some());
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
                if self.cdr != Heap::null_ptr() {
                    format!("(cons {} {})", self.car, heap.get(self.cdr).format(heap))
                } else {
                    format!("(cons {} nil)", self.car)
                }
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
            heap.get(root).format(&heap)
        );

        heap.collect([Some(&inner2)].into_iter());
        assert_eq!("(cons 2 (cons 1 nil))", heap.get(inner2).format(&heap));

        heap.collect([Some(&inner1)].into_iter());
        assert_eq!("(cons 1 nil)", heap.get(inner1).format(&heap));

        heap.collect([].into_iter());
        assert!(heap.get_header(inner1).is_none());
    }
}
