//! Contains items for the manipulation of memory at runtime.

use crate::{Constant, ErrorKind, Object, PiccoloError, Value};

use std::mem;

use crate::fnv::FnvHashMap;

/// Simple string interner.
///
/// Discussed on [matklad's blog post.] Further discussion on reddit [here.]
/// Uses FNV hashing provided by [`FnvHashMap`].
///
/// [matklad's blog post.]: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
/// [here.]: https://www.reddit.com/r/rust/comments/fn1jxf/blog_post_fast_and_simple_rust_interner/
/// [`FnvHashMap`]: https://doc.servo.org/fnv/
pub struct Interner {
    map: FnvHashMap<&'static str, usize>,
    strings: Vec<&'static str>,
    current_buffer: String,
    previous_buffers: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            map: FnvHashMap::default(),
            strings: Vec::new(),
            current_buffer: String::with_capacity(cap),
            previous_buffers: Vec::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> usize {
        if let Some(&id) = self.map.get(s) {
            trace!("str exists {:x}", id);
            return id;
        }

        let s = unsafe { self.alloc(s) };
        let id = self.map.len();
        self.map.insert(s, id);
        self.strings.push(s);

        trace!("str does not exist {:x}", id);
        debug_assert!(self.lookup(id) == s);
        debug_assert!(self.intern(s) == id);

        id
    }

    pub fn lookup(&self, id: usize) -> &str {
        self.strings
            .get(id)
            .unwrap_or_else(|| panic!("str does not exist: {:x}", id))
    }

    // this is OK because:
    //  1. we are the only user of this function
    //  2. we don't drop or deallocate the old buffers when we reallocate the current one,
    //     so any &str that are floating around will always point to valid data
    //  3. the &str from Intern::lookup is bounded by the lifetime of self anyway, so we can't
    //     hold on to a &str at all if we want to add new ones
    unsafe fn alloc(&mut self, s: &str) -> &'static str {
        let current_capacity = self.current_buffer.capacity();

        // if adding a new string will cause reallocation of the buffer
        if current_capacity < self.current_buffer.len() + s.len() {
            // create the next buffer
            let new_capacity = (current_capacity.max(s.len()) + 1).next_power_of_two();
            let next_buffer = String::with_capacity(new_capacity);
            trace!(
                "reallocate interner: {} -> {}",
                self.current_buffer.len(),
                new_capacity
            );

            // swap them out
            let previous_buffer = mem::replace(&mut self.current_buffer, next_buffer);

            // retain ownership of the old buffer so &str references don't become invalid
            self.previous_buffers.push(previous_buffer);
        }

        // add s to the current buffer
        let interned = {
            let start = self.current_buffer.len();
            self.current_buffer.push_str(s);
            &self.current_buffer[start..]
        };

        &*(interned as *const str)
    }
}

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
/// [`Object`]: ../object/trait.Object.html
pub struct Heap {
    memory: Vec<Option<Box<dyn Object>>>,
    interner: Interner,
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
            interner: Interner::with_capacity(32),
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

    pub fn alloc_string(&mut self, s: &str) -> Value {
        Value::String(self.interner.intern(s))
    }

    /// Allocate space for a new value, and return its pointer.
    pub fn alloc(&mut self, value: Box<dyn Object>) -> Value {
        while self.memory[self.alloc_after].is_some() {
            self.alloc_after += 1;
        }

        if self.alloc_after + 1 == self.memory.len() {
            self.memory
                .resize_with(self.memory.len() + (self.memory.len() / 2 + 1), || None);
        }

        debug!("insert {:x} = {:?}", self.alloc_after, value);

        self.memory[self.alloc_after] = Some(value);
        Value::Object(self.alloc_after)
    }

    /// De-reference a pointer.
    ///
    /// # Panics:
    ///
    /// This method panics if the pointer points to memory outside the range of the heap,
    /// or if the object pointed at is `None`.
    #[inline]
    pub fn deref(&self, ptr: Value) -> &dyn Object {
        match ptr {
            Value::Object(ptr) => {
                trace!("deref {:x}", ptr);
                self.memory[ptr].as_deref().expect("deref invalid ptr")
            }
            _ => panic!("deref with non-ptr {:?}", ptr),
        }
    }

    /// De-reference a pointer, and get a mutable reference.
    ///
    /// # Panics:
    ///
    /// This method panics if the pointer points to memory outside the range of the heap,
    /// or if the object pointed at is `None`.
    #[inline]
    pub fn deref_mut(&mut self, ptr: Value) -> &mut dyn Object {
        match ptr {
            Value::Object(ptr) => {
                trace!("deref mut {:x}", ptr);
                self.memory[ptr]
                    .as_deref_mut()
                    .expect("deref_mut invalid ptr")
            }
            _ => panic!("deref_mut with non-ptr {:?}", ptr),
        }
    }

    /// Move a value out of the heap, de-allocating it.
    ///
    /// # Panics:
    ///
    /// This method panics if the pointer points to memory outside the range of the heap,
    /// or if the object pointed at is `None`.
    #[inline]
    pub fn take(&mut self, ptr: Value) -> Box<dyn Object> {
        match ptr {
            Value::Object(ptr) => {
                debug!("take {:x}", ptr);
                self.memory[ptr].take().expect("free invalid ptr")
            }
            _ => panic!("take with non-ptr {:?}", ptr),
        }
    }

    pub fn try_deep_copy(&mut self, value: &Value) -> Result<Value, PiccoloError> {
        Ok(match value {
            Value::Object(ptr) => {
                trace!("try copy {:x}", ptr);
                let cloned = self.deref(*value).try_clone().ok_or_else(|| {
                    PiccoloError::new(ErrorKind::CannotClone {
                        ty: self.deref(*value).type_name().to_owned(),
                    })
                })?;
                self.alloc(cloned)
            }
            Value::String(v) => Value::String(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::Integer(v) => Value::Integer(*v),
            Value::Double(v) => Value::Double(*v),
            Value::Nil => Value::Nil,
        })
    }

    pub(crate) fn value_into_constant(&mut self, v: Value) -> Constant {
        match v {
            Value::String(ptr) => Constant::String(self.interner.lookup(ptr).to_owned()),
            Value::Bool(v) => Constant::Bool(v),
            Value::Integer(v) => Constant::Integer(v),
            Value::Double(v) => Constant::Double(v),
            Value::Nil => Constant::Nil,
            _ => panic!("cannot make constant from object"),
        }
    }

    pub(crate) fn constant_into_value(&mut self, constant: &Constant) -> Value {
        trace!("into_value");
        match constant {
            Constant::String(v) => {
                let ptr = self.interner.intern(v);
                trace!("string ptr {:x}", ptr);
                Value::String(ptr)
            }
            Constant::Integer(v) => Value::Integer(*v),
            Constant::Bool(v) => Value::Bool(*v),
            Constant::Double(v) => Value::Double(*v),
            Constant::Nil => Value::Nil,
        }
    }

    /// Returns the type name of a value.
    pub fn type_name(&self, v: &Value) -> &'static str {
        match v {
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Object(_) => self.deref(*v).type_name(),
            Value::String(_) => "string",
            Value::Nil => "nil",
        }
    }

    /// Formats the value.
    pub fn fmt(&self, v: &Value) -> String {
        match v {
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(_) => format!("{}", self.deref(*v)),
            Value::String(v) => format!("{}", self.interner.lookup(*v)),
            Value::Nil => "nil".into(),
        }
    }

    /// Formats the value.
    pub fn dbg(&self, v: &Value) -> String {
        match v {
            Value::Bool(v) => format!("bool({})", v),
            Value::Integer(v) => format!("integer({})", v),
            Value::Double(v) => format!("double({})", v),
            Value::Object(_) => format!("*{}({:?})", self.deref(*v).type_name(), self.deref(*v)),
            Value::String(v) => format!("string({:?})", self.interner.lookup(*v)),
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
                    if *l == *r {
                        Some(true)
                    } else {
                        let lhs = self.deref(*lhs);
                        let rhs = self.deref(*rhs);
                        lhs.eq(rhs)
                    }
                }
                _ => None,
            },
            Value::String(l) => match rhs {
                Value::String(r) => Some(*l == *r),
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
            Value::Object(_) => match rhs {
                Value::Object(_) => {
                    let lhs = self.deref(*lhs);
                    let rhs = self.deref(*rhs);
                    lhs.lt(rhs)
                }
                _ => None,
            },
            Value::String(l) => match rhs {
                Value::String(r) => {
                    let lhs = self.interner.lookup(*l);
                    let rhs = self.interner.lookup(*r);
                    Some(lhs < rhs)
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
            Value::Object(_) => match rhs {
                Value::Object(_) => {
                    let lhs = self.deref(*lhs);
                    let rhs = self.deref(*rhs);
                    lhs.gt(rhs)
                }
                _ => None,
            },
            Value::String(l) => match rhs {
                Value::String(r) => {
                    let lhs = self.interner.lookup(*l);
                    let rhs = self.interner.lookup(*r);
                    Some(lhs > rhs)
                }
                _ => None,
            },
            _ => None,
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
    use crate::Value;

    use super::super::object::Object;
    use super::Heap;

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

    #[test]
    fn heap_alloc() {
        let mut h = Heap::new(30);
        let _ = h.alloc(Box::new(S(32)));
    }

    #[test]
    fn heap_deref() {
        let mut h = Heap::new(30);
        let s = h.alloc(Box::new(S(342)));
        assert!(h.deref(s).eq(&S(342)).unwrap());
    }

    #[test]
    fn heap_free() {
        let mut h = Heap::new(30);
        let s = h.alloc(Box::new(S(32)));
        assert!(h.take(s).eq(&S(32)).unwrap());
    }

    #[test]
    #[should_panic]
    fn use_after_free() {
        let mut h = Heap::new(30);
        let s = h.alloc(Box::new(S(34232)));
        assert!(h.take(s).eq(&S(34232)).unwrap());
        h.deref(s);
    }

    #[test]
    fn mutable() {
        let mut heap = Heap::new(8);
        let ptr = heap.alloc(Box::new(S(32)));
        heap.deref_mut(ptr).set("", Value::Integer(78));
        assert_eq!(heap.deref(ptr).downcast_ref::<S>().unwrap(), &S(78));
    }

    #[test]
    fn bunches() {
        let mut pointers = vec![];
        let mut h = Heap::new(32);

        for i in 0..10 {
            pointers.push(h.alloc(Box::new(S(i))));
        }
        for i in 0..10 {
            assert!(h.deref(pointers[i]).eq(&S(i as i64)).unwrap());
        }
        for ptr in pointers {
            h.take(ptr);
        }
    }

    #[test]
    fn non_contiguous2() {
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
