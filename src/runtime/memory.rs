//! Contains items for the manipulation of memory at runtime.

use crate::{Constant, Object, Value};

/// A `Heap` is the main method of runtime variable reference semantics.
///
/// The only way of implementing the dynamic lifetimes of values that need to exist in
/// a scripting language like Piccolo is to circumvent Rust's system of borrows and lifetimes
/// entirely. This struct is how this is implemented in Piccolo.
///
/// A `Heap` is implemented using a `Vec<Option<Box<dyn `[`Object`]`>>>`, and at runtime, pointers
/// to objects in the heap are simply `usize` indices into the heap's vector. Although there
/// are two levels of indirection, the negative impact on performance comes with static memory
/// safety guarantees.
///
/// [`Object`]: ../value/trait.Object.html
pub struct Heap {
    memory: Vec<Option<Box<dyn Object>>>,
    alloc_after: usize,
}

impl Heap {
    /// Create a new `Heap` with a capacity. See also [`Vec::with_capacity`].
    ///
    /// [`Vec::with_capacity`]: https://doc.rust-lang.org/stable/std/vec/struct.Vec.html#method.with_capacity
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
        debug!("start GC");
        // for item in memory
        // if inaccessible by the VM, Option::take() it
        self.alloc_after = 0;
    }

    /// Allocate space for a new value, and return its pointer.
    pub fn alloc(&mut self, value: Box<dyn Object>) -> usize {
        while self.memory[self.alloc_after].is_some() {
            self.alloc_after += 1;
        }

        if self.alloc_after + 1 == self.memory.len() {
            self.memory
                .resize_with(self.memory.len() + (self.memory.len() / 2 + 1), || None);
        }

        debug!("insert {:x} = {:?}", self.alloc_after, value);

        self.memory[self.alloc_after] = Some(value);
        self.alloc_after
    }

    /// De-reference a pointer.
    ///
    /// # Panics:
    ///
    /// This method panics if the pointer points to memory outside the range of the heap,
    /// or if the object pointed at is `None`.
    #[inline]
    pub fn deref(&self, ptr: usize) -> &dyn Object {
        trace!("deref {:x}", ptr);
        self.memory[ptr].as_deref().expect("deref invalid ptr")
    }

    /// De-reference a pointer, and get a mutable reference.
    ///
    /// # Panics:
    ///
    /// This method panics if the pointer points to memory outside the range of the heap,
    /// or if the object pointed at is `None`.
    #[inline]
    pub fn deref_mut(&mut self, ptr: usize) -> &mut dyn Object {
        trace!("deref mut {:x}", ptr);
        self.memory[ptr]
            .as_deref_mut()
            .expect("deref_mut invalid ptr")
    }

    /// Move a value out of the heap, de-allocating it.
    ///
    /// # Panics:
    ///
    /// This method panics if the pointer points to memory outside the range of the heap,
    /// or if the object pointed at is `None`.
    #[inline]
    pub fn take(&mut self, ptr: usize) -> Box<dyn Object> {
        debug!("take {:x}", ptr);
        self.memory[ptr].take().expect("free invalid ptr")
    }

    pub fn deep_copy(&mut self, value: &Value) -> Option<Value> {
        Some(match value {
            Value::Object(ptr) => {
                trace!("try copy {:x}", ptr);
                let cloned = self.deref(*ptr).try_clone()?;
                Value::Object(self.alloc(cloned))
            },
            Value::Bool(v) => Value::Bool(*v),
            Value::Integer(v) => Value::Integer(*v),
            Value::Double(v) => Value::Double(*v),
            Value::Nil => Value::Nil,
        })
    }

    pub(crate) fn value_into_constant(&mut self, v: Value) -> Constant {
        match v {
            Value::Object(ptr) => {
                let obj = self.take(ptr);
                if let Ok(string) = obj.downcast::<String>() {
                    Constant::String(string.to_string())
                } else {
                    panic!("non-string constant");
                }
            }
            Value::Bool(v) => Constant::Bool(v),
            Value::Integer(v) => Constant::Integer(v),
            Value::Double(v) => Constant::Double(v),
            Value::Nil => Constant::Nil,
        }
    }

    // TODO: check VM's string table - maybe even own the string table instead of the VM?
    pub(crate) fn constant_into_value(&mut self, constant: Constant) -> Value {
        trace!("into_value");
        match constant {
            Constant::String(v) => {
                let ptr = self.alloc(Box::new(v));
                Value::Object(ptr)
            }
            Constant::Integer(v) => Value::Integer(v),
            Constant::Bool(v) => Value::Bool(v),
            Constant::Double(v) => Value::Double(v),
            Constant::Nil => Value::Nil,
        }
    }

    /// Returns the type name of a value.
    pub fn type_name(&self, v: &Value) -> &'static str {
        match v {
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Object(v) => self.deref(*v).type_name(),
            Value::Nil => "nil",
        }
    }

    /// Formats the value.
    pub fn fmt(&self, v: &Value) -> String {
        match v {
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(v) => format!("{}", self.deref(*v)),
            Value::Nil => "nil".into(),
        }
    }

    /// Formats the value.
    pub fn dbg(&self, v: &Value) -> String {
        match v {
            Value::Bool(v) => format!("bool({})", v),
            Value::Integer(v) => format!("integer({})", v),
            Value::Double(v) => format!("double({})", v),
            Value::Object(v) => format!("*{}({:?})", self.deref(*v).type_name(), self.deref(*v)),
            Value::Nil => "Nil".into(),
        }
    }

    /// Tests a value for equality. Returns `None` if incomparable.
    pub fn eq(&self, lhs: &Value, rhs: &Value) -> Option<bool> {
        match lhs {
            Value::Bool(l) => match rhs {
                Value::Bool(r) => Some(l == r),
                _ => None,
            },
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Some(l == r),
                Value::Double(r) => Some(*l as f64 == *r),
                _ => None,
            },
            Value::Double(l) => match rhs {
                Value::Integer(r) => Some(*l == *r as f64),
                Value::Double(r) => Some(l == r),
                _ => None,
            },
            Value::Object(l) => match rhs {
                Value::Object(r) => {
                    if l == r {
                        Some(true)
                    } else {
                        let lhs = self.deref(*l);
                        let rhs = self.deref(*r);
                        lhs.eq(rhs)
                    }
                }
                _ => None,
            },
            Value::Nil => match rhs {
                Value::Nil => Some(true),
                _ => Some(false),
            },
        }
    }

    pub fn lt(&self, lhs: &Value, rhs: &Value) -> Option<bool> {
        match lhs {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Some(l < r),
                Value::Double(r) => Some((*l as f64) < *r),
                _ => None,
            },
            Value::Double(l) => match rhs {
                Value::Integer(r) => Some(*l < *r as f64),
                Value::Double(r) => Some(l < r),
                _ => None,
            },
            Value::Object(l) => match rhs {
                Value::Object(r) => {
                    let lhs = self.deref(*l);
                    let rhs = self.deref(*r);
                    lhs.lt(rhs)
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn gt(&self, lhs: &Value, rhs: &Value) -> Option<bool> {
        match lhs {
            Value::Integer(l) => match rhs {
                Value::Integer(r) => Some(l > r),
                Value::Double(r) => Some(*l as f64 > *r),
                _ => None,
            },
            Value::Double(l) => match rhs {
                Value::Integer(r) => Some(*l > *r as f64),
                Value::Double(r) => Some(l > r),
                _ => None,
            },
            Value::Object(l) => match rhs {
                Value::Object(r) => {
                    let lhs = self.deref(*l);
                    let rhs = self.deref(*r);
                    lhs.gt(rhs)
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_string(&self, v: &Value) -> bool {
        match v {
            Value::Object(ptr) => self.deref(*ptr).is::<String>(),
            _ => false,
        }
    }
}

pub(crate) fn dbg_list(l: &[Value], heap: &Heap) -> String {
    trace!("dbg_list");
    if l.is_empty() {
        "[]".into()
    } else {
        let mut s = String::from("[");
        for item in l {
            s.push_str(&format!("{}, ", heap.dbg(item)));
        }
        format!("{}]", &s[..s.len() - 2])
    }
}

#[cfg(test)]
mod test {
    use super::super::object::Object;
    use super::Heap;

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
