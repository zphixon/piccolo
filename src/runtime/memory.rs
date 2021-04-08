//! Contains items for the manipulation of memory at runtime.
//!
//! Shamelessly copied from https://github.com/Darksecond/lox

use crate::Object;

use std::{
    cell::Cell,
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicUsize, Ordering},
};

/// Allocation metadata for a GC object.
#[derive(Debug, Default)]
struct Header {
    roots: AtomicUsize,
    marked: Cell<bool>,
}

/// A GC allocation.
///
/// Contains some metadata about the allocation, for example how many roots there are, and whether
/// or not the object is marked.
#[derive(Debug)]
struct Allocation<'data, T: Object + ?Sized> {
    header: Header,
    _phantom: PhantomData<&'data ()>,
    data: T,
}

/// Heap type.
///
/// A Vec<Box<Allocation<dyn Object>>> contains all heap objects. Objects allocated by the GC will
/// be owned by this struct.
#[derive(Debug, Default)]
pub struct Heap<'data> {
    objects: Vec<Box<Allocation<'data, dyn Object + 'data>>>,
}

/// GC smart pointer. Copyable.
///
/// Contains non-null pointer to an allocation stored on the heap.
pub struct Gc<'data, T: Object + ?Sized> {
    ptr: *mut Allocation<'data, T>,
}

/// Represents a rooted value. Cloneable.
///
/// Contains non-null pointer to an allocation stored on the heap.
pub struct Root<'data, T: Object + ?Sized> {
    ptr: *mut Allocation<'data, T>,
}

/// A value that is uniquely rooted. Uncloneable.
///
/// Contains non-null pointer to an allocation stored on the heap. Semantically uncopyable.
pub struct UniqueRoot<'data, T: Object + ?Sized> {
    ptr: *mut Allocation<'data, T>,
}

impl<T: Object + ?Sized> Allocation<'_, T> {
    // mark the gc object
    fn unmark(&self) {
        self.header.marked.set(false);
    }

    // increase the ref count for an object. not actually a ref count but it's similar.
    fn root(&self) {
        self.header.roots.fetch_add(1, Ordering::Relaxed);
    }

    fn unroot(&self) {
        self.header.roots.fetch_sub(1, Ordering::Relaxed);
    }
}

// this is where the magic happens
impl<T: Object + ?Sized> Object for Allocation<'_, T> {
    fn trace(&self) {
        // mark the allocation as having been traced and then trace its data
        if !self.header.marked.replace(true) {
            self.data.trace();
        }
    }

    fn type_name(&self) -> &'static str {
        T::type_name(&self.data)
    }
}

impl<'data> Heap<'data> {
    // allocate some data on the heap. return type points to allocation owned by Heap.objects
    unsafe fn allocate<'this, T: 'data + Object>(
        &'this mut self,
        data: T,
    ) -> *mut Allocation<'data, T> {
        let mut alloc = Box::new(Allocation {
            header: Header::default(),
            data,
            _phantom: Default::default(),
        });

        // TODO: maybe box unnecessary?
        // TODO: maybe NonNull::new?
        //let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
        let ptr = &mut *alloc as *mut Allocation<T>;
        self.objects.push(alloc);
        ptr
    }

    /// Root a GCd object.
    pub fn root<'this, T: Object + ?Sized>(&'this mut self, obj: Gc<'data, T>) -> Root<'data, T> {
        obj.as_allocation().root();
        Root { ptr: obj.ptr }
    }

    /// Manage an object on the heap.
    ///
    /// Data will be GCable.
    pub fn manage<'this, T: 'data + Object>(&'this mut self, data: T) -> Root<'data, T> {
        let root = Root {
            ptr: unsafe { self.allocate(data) },
        };
        root.as_allocation().root();
        root
    }

    /// Manage an uncopyable object on the heap.
    ///
    /// As in manage, data will be GCable.
    pub fn manage_unique<'this, T: 'data + Object>(
        &'this mut self,
        data: T,
    ) -> UniqueRoot<'data, T> {
        let root = UniqueRoot {
            ptr: unsafe { self.allocate(data) },
        };
        root.as_allocation().root();
        root
    }

    /// Take out the trash.
    ///
    /// Return number of bytes cleared.
    pub fn collect(&mut self) -> usize {
        self.mark();
        let bytes = self.bytes_unmarked();
        self.sweep();
        bytes
    }

    // Mark rooted objects
    fn mark(&mut self) {
        for object in &self.objects {
            object.unmark();
        }

        self.objects
            .iter()
            .filter(|obj| obj.header.roots.load(Ordering::Relaxed) > 0)
            .for_each(|obj| obj.trace());
    }

    // Delete all unmarked objects. If there are any roots to an object,
    fn sweep(&mut self) {
        self.objects.retain(|obj| obj.header.marked.get());
    }

    // number of unmarked bytes
    fn bytes_unmarked(&self) -> usize {
        self.objects.iter().fold(0, |bytes, obj| {
            if !obj.header.marked.get() {
                bytes + std::mem::size_of_val(&obj.data)
            } else {
                bytes
            }
        })
    }

    /// Number of objects in the heap.
    pub fn objects(&self) -> usize {
        self.objects.len()
    }
}

// Gc<T> impl, Clone, Copy, Deref, Debug, Display, Object {{{
impl<'data, T: Object + ?Sized> Gc<'data, T> {
    // safety: the lifetime of the returned allocation is bound to self
    fn as_allocation(&self) -> &Allocation<'data, T> {
        unsafe { self.ptr.as_ref().unwrap() }
    }
}

impl<T: Object + Sized + Clone> Gc<'_, T> {
    pub fn deep_copy(&self) -> T {
        self.as_allocation().data.clone()
    }
}

impl<'data, T: Object + ?Sized> Clone for Gc<'data, T> {
    fn clone<'this>(&'this self) -> Gc<'data, T> {
        *self
    }
}

impl<T: Object + ?Sized> Copy for Gc<'_, T> {}

impl<T: Object + ?Sized> Deref for Gc<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.as_allocation().data
    }
}

impl<T: fmt::Debug + Object + ?Sized> fmt::Debug for Gc<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        write!(f, "{:?}", inner)
    }
}

impl<T: fmt::Display + Object + ?Sized> fmt::Display for Gc<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        std::fmt::Display::fmt(inner, f)
    }
}

impl<T: Object + ?Sized> Object for Gc<'_, T> {
    fn trace(&self) {
        self.as_allocation().trace()
    }

    fn type_name(&self) -> &'static str {
        T::type_name(self)
    }
}
// }}}

// Root<T> impl, Clone, Deref, Debug, Display, Object, Drop {{{
impl<'data, T: Object + ?Sized> Root<'data, T> {
    // safety: the lifetime of the returned allocation is bound to self
    fn as_allocation<'this>(&'this self) -> &'this Allocation<'data, T> {
        unsafe { self.ptr.as_ref().unwrap() }
    }

    pub(crate) fn as_gc<'this>(&'this self) -> Gc<'data, T> {
        Gc { ptr: self.ptr }
    }
}

impl<'data, T: Object + ?Sized> Clone for Root<'data, T> {
    fn clone<'this>(&'this self) -> Root<'data, T> {
        self.as_allocation().root();
        Root { ptr: self.ptr }
    }
}

impl<T: Object + ?Sized> Deref for Root<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.as_allocation().data
    }
}

impl<T: fmt::Debug + Object + ?Sized> fmt::Debug for Root<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        write!(f, "Root({:?})", inner)
    }
}

impl<T: fmt::Display + Object + ?Sized> fmt::Display for Root<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        std::fmt::Display::fmt(inner, f)
    }
}

impl<T: Object + ?Sized> Object for Root<'_, T> {
    fn trace(&self) {
        self.as_allocation().trace()
    }

    fn type_name(&self) -> &'static str {
        T::type_name(self)
    }
}

impl<T: Object + ?Sized> Drop for Root<'_, T> {
    fn drop(&mut self) {
        self.as_allocation().unroot();
    }
}
// }}}

// UniqueRoot<T> impl, Deref, DerefMut, Debug, Display, Object, Drop {{{
impl<'data, T: Object + ?Sized> UniqueRoot<'data, T> {
    // safety: the lifetime of the returned allocation is bound to self
    fn as_allocation<'this>(&'this self) -> &'this Allocation<'data, T> {
        unsafe { self.ptr.as_ref().unwrap() }
    }

    fn as_allocation_mut<'this>(&'this mut self) -> &'this mut Allocation<'data, T> {
        unsafe { self.ptr.as_mut().unwrap() }
    }
}

impl<T: Object + ?Sized> Deref for UniqueRoot<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.as_allocation().data
    }
}

impl<T: Object + ?Sized> DerefMut for UniqueRoot<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.as_allocation_mut().data
    }
}

impl<T: fmt::Debug + Object + ?Sized> fmt::Debug for UniqueRoot<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        write!(f, "UniqueRoot({:?})", inner)
    }
}

impl<T: fmt::Display + Object + ?Sized> fmt::Display for UniqueRoot<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        std::fmt::Display::fmt(inner, f)
    }
}

impl<T: Object + ?Sized> Object for UniqueRoot<'_, T> {
    fn trace(&self) {
        self.as_allocation().trace()
    }

    fn type_name(&self) -> &'static str {
        T::type_name(self)
    }
}

impl<T: Object + ?Sized> Drop for UniqueRoot<'_, T> {
    fn drop(&mut self) {
        self.as_allocation().unroot();
    }
}
// }}}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn clean_up_roots() {
        let mut heap = Heap::default();

        {
            let root = heap.manage(String::from("h"));
            assert_eq!(*root, "h");
        }

        assert_eq!(1, heap.objects());
        assert_eq!(std::mem::size_of::<String>(), heap.collect());
        assert_eq!(0, heap.objects());
    }

    #[test]
    fn clean_up_unique_roots() {
        let mut heap = Heap::default();

        {
            let _root = heap.manage_unique(String::new());
        }

        assert_eq!(1, heap.objects());
        assert_eq!(std::mem::size_of::<String>(), heap.collect());
        assert_eq!(0, heap.objects());
    }

    #[test]
    fn use_after_free() {
        use crate::Object;

        #[derive(Debug)]
        struct S(u64);
        impl Drop for S {
            fn drop(&mut self) {
                self.0 = 0xbeeeb000;
            }
        }
        impl Object for S {
            fn trace(&self) {}
        }

        let gc;

        {
            let mut heap = Heap::default();
            gc = heap.manage(S(0)).as_gc();
            println!("{:#?}", heap);
        }

        assert_ne!(gc.0, 0xbeeeb000);
    }

    #[test]
    fn deep_copy() {
        use std::cell::RefCell;

        let mut heap = Heap::default();
        let root = heap.manage(RefCell::new(String::from("din")));
        let copy = root.as_gc().deep_copy();

        root.borrow_mut().push_str("gus");

        assert_eq!(root.borrow().as_str(), "dingus");
        assert_eq!(copy.borrow().as_str(), "din");
    }
}
