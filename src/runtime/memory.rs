use super::value::Object;

// objects need to be able to live for any period of time during the interpreter's
// execution. they don't need to exist at creation, since constants are baked into the
// chunk. I'm probably going to break up Value into standard Value and Constant which is
// what a Chunk will hold, and when needed at runtime a Value can be an index to a chunk's
// Constant table. I think this is the easiest method of implementing "dynamic" references
// without taking a deep dive into actual unsafe rust land.

pub struct Heap {
    memory: Vec<Option<Box<dyn Object>>>,
    alloc_after: usize,
}

impl Heap {
    pub fn new(capacity: usize) -> Heap {
        let mut memory = Vec::with_capacity(capacity);
        memory.resize_with(capacity, || None);
        Heap {
            memory,
            alloc_after: 0,
        }
    }

    // needs to see the vm stack
    pub fn gc(&mut self) {
        // for item in memory
        // if inaccessible by the VM, Option::take() it
        self.alloc_after = 0;
    }

    pub fn try_copy(&mut self, ptr: usize) -> Option<usize> {
        let cloned = self.deref(ptr).try_clone()?;
        Some(self.alloc(cloned))
    }

    pub fn alloc(&mut self, value: Box<dyn Object>) -> usize {
        while self.memory[self.alloc_after].is_some() {
            self.alloc_after += 1;
        }

        if self.alloc_after + 1 == self.memory.len() {
            self.memory
                .resize_with(self.memory.len() + (self.memory.len() / 2 + 1), || None);
        }

        self.memory[self.alloc_after] = Some(value);
        self.alloc_after
    }

    #[inline]
    pub fn deref(&self, ptr: usize) -> &dyn Object {
        self.memory[ptr].as_deref().expect("deref invalid ptr")
    }

    #[inline]
    pub fn deref_mut(&mut self, ptr: usize) -> &mut dyn Object {
        self.memory[ptr]
            .as_deref_mut()
            .expect("deref_mut invalid ptr")
    }

    #[inline]
    pub fn take(&mut self, ptr: usize) -> Box<dyn Object> {
        self.memory[ptr].take().expect("free invalid ptr")
    }
}

#[cfg(test)]
mod test {
    use super::Heap;
    use crate::runtime::value::Object;

    #[test]
    fn heap_alloc() {
        let mut h = Heap::new(30);
        let _ = h.alloc(Box::new(String::from("something")));
    }

    #[test]
    fn heap_deref() {
        let mut h = Heap::new(30);
        let s = h.alloc(Box::new(String::from("hello")));
        assert!(h.deref(s).eq(&String::from("hello")).unwrap());
    }

    #[test]
    fn heap_free() {
        let mut h = Heap::new(30);
        let s = h.alloc(Box::new(String::from("world")));
        assert!(h.take(s).eq(&String::from("world")).unwrap());
    }

    #[test]
    #[should_panic]
    fn use_after_free() {
        let mut h = Heap::new(30);
        let s = h.alloc(Box::new(String::from("world")));
        assert!(h.take(s).eq(&String::from("world")).unwrap());
        h.deref(s);
    }

    #[test]
    fn mutable() {
        use crate::Value;
        #[derive(Debug, PartialEq)]
        struct S(i64);
        impl Object for S {
            fn type_name(&self) -> &'static str {
                "S"
            }
            fn eq(&self, rhs: &dyn Object) -> Option<bool> {
                Some(rhs.downcast_ref::<S>()?.0 == self.0)
            }
            fn set(&mut self, _property: &str, value: Value) -> Option<()> {
                match value {
                    Value::Integer(v) => self.0 = v,
                    _ => panic!(),
                }
                Some(())
            }
        }
        impl core::fmt::Display for S {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        let mut heap = Heap::new(8);
        let ptr = heap.alloc(Box::new(S(32)));
        heap.deref_mut(ptr).set("", Value::Integer(78));
        assert_eq!(heap.deref(ptr).downcast_ref::<S>().unwrap(), &S(78));
    }

    #[test]
    fn bunches() {
        #[derive(Debug)]
        struct S(usize);
        impl Object for S {
            fn type_name(&self) -> &'static str {
                "S"
            }
            fn eq(&self, rhs: &dyn Object) -> Option<bool> {
                Some(rhs.downcast_ref::<S>()?.0 == self.0)
            }
        }
        impl core::fmt::Display for S {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        let mut pointers = vec![];
        let mut h = Heap::new(32);

        for i in 0..10 {
            pointers.push(h.alloc(Box::new(S(i))));
        }
        for i in 0..10 {
            assert!(h.deref(pointers[i]).eq(&S(i)).unwrap());
        }
        for ptr in pointers {
            h.take(ptr);
        }
    }

    #[test]
    fn non_contiguous2() {
        #[derive(Debug, PartialEq)]
        struct S(usize);
        impl Object for S {
            fn type_name(&self) -> &'static str {
                "S"
            }
            fn eq(&self, rhs: &dyn Object) -> Option<bool> {
                Some(rhs.downcast_ref::<S>()?.0 == self.0)
            }
            fn try_clone(&self) -> Option<Box<dyn Object>> {
                Some(Box::new(S(self.0)))
            }
        }
        impl core::fmt::Display for S {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        let mut pointers = vec![];
        let mut heap = Heap::new(32);

        heap.gc();
        for i in 0..16 {
            pointers.push(heap.alloc(Box::new(S(i))));
        }

        heap.gc();
        for _ in 4..12 {
            let _ = heap.take(pointers.remove(4));
        }

        heap.gc();
        for i in 0..16 {
            pointers.push(heap.alloc(Box::new(S(i))));
        }

        heap.gc();

        for i in 50..58 {
            pointers.push(heap.alloc(Box::new(S(i))));
        }
        heap.gc();

        for _ in 4..12 {
            let _ = heap.take(pointers.remove(16));
        }
        heap.gc();

        for _ in 0..32 {
            pointers.push(heap.alloc(Box::new(S(777))));
        }
    }

    #[test]
    fn non_contiguous() {
        #[derive(Debug, PartialEq)]
        struct S(usize);
        impl Object for S {
            fn type_name(&self) -> &'static str {
                "S"
            }
            fn eq(&self, rhs: &dyn Object) -> Option<bool> {
                Some(rhs.downcast_ref::<S>()?.0 == self.0)
            }
            fn try_clone(&self) -> Option<Box<dyn Object>> {
                Some(Box::new(S(self.0)))
            }
        }
        impl core::fmt::Display for S {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        let mut pointers = vec![];
        let mut heap = Heap::new(32);

        for i in 0..16 {
            pointers.push(heap.alloc(Box::new(S(i))));
        }

        for _ in 4..12 {
            let _ = heap.take(pointers.remove(4));
        }

        for i in 0..16 {
            pointers.push(heap.alloc(Box::new(S(i))));
        }

        // just to set the allocated size back to zero
        heap.gc();
        assert_eq!(heap.memory.len(), 49);
        assert_eq!(pointers.len(), 24);

        for i in 50..58 {
            pointers.push(heap.alloc(Box::new(S(i))));
        }

        assert_eq!(heap.memory.len(), 49);
        assert_eq!(pointers.len(), 32);

        let v: Vec<Option<S>> = heap
            .memory
            .into_iter()
            .map(|item: Option<Box<dyn Object>>| {
                item.and_then(|item: Box<dyn Object>| {
                    item.downcast::<S>()
                        .ok()
                        .take()
                        .and_then(|item: Box<S>| Some(*item))
                })
            })
            .collect();

        assert_eq!(
            v,
            vec![
                Some(S(0)),
                Some(S(1)),
                Some(S(2)),
                Some(S(3)),
                Some(S(50)),
                Some(S(51)),
                Some(S(52)),
                Some(S(53)),
                Some(S(54)),
                Some(S(55)),
                Some(S(56)),
                Some(S(57)),
                Some(S(12)),
                Some(S(13)),
                Some(S(14)),
                Some(S(15)),
                Some(S(0)),
                Some(S(1)),
                Some(S(2)),
                Some(S(3)),
                Some(S(4)),
                Some(S(5)),
                Some(S(6)),
                Some(S(7)),
                Some(S(8)),
                Some(S(9)),
                Some(S(10)),
                Some(S(11)),
                Some(S(12)),
                Some(S(13)),
                Some(S(14)),
                Some(S(15)),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
            ]
        );
    }
}
