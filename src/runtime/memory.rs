//! Contains items for the manipulation of memory at runtime.
//!
//! Shamelessly copied from https://github.com/Darksecond/lox

use std::cell::Cell;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Trait to trace objects for marking and sweeping.
pub trait Object {
    fn trace(&self);

    fn type_name(&self) -> &'static str {
        "object"
    }
}

impl fmt::Debug for dyn Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<trace>")
    }
}

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
struct Allocation<T: 'static + Object + ?Sized> {
    header: Header,
    data: T,
}

/// Heap type.
///
/// A Vec<Box<Allocation<dyn Object>>> contains all heap objects. Objects allocated by the GC will
/// be owend by this struct.
#[derive(Debug, Default)]
pub struct Heap {
    objects: Vec<Box<Allocation<dyn Object>>>,
}

/// GC smart pointer. Copyable.
///
/// Contains non-null pointer to an allocation stored on the heap.
pub struct Gc<T: 'static + Object + ?Sized> {
    ptr: NonNull<Allocation<T>>,
}

/// Represents a rooted value. Cloneable.
///
/// Contains non-null pointer to an allocation stored on the heap.
pub struct Root<T: 'static + Object + ?Sized> {
    ptr: NonNull<Allocation<T>>,
}

/// A value that is uniquely rooted. Uncloneable.
///
/// Contains non-null pointer to an allocation stored on the heap. Semantically uncopyable.
pub struct UniqueRoot<T: 'static + Object + ?Sized> {
    ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Object + ?Sized> Allocation<T> {
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
impl<T: 'static + Object + ?Sized> Object for Allocation<T> {
    fn trace(&self) {
        // mark the allocation as having been traced and then trace its data
        if !self.header.marked.replace(true) {
            self.data.trace();
        }
    }
}

impl Heap {
    // allocate some data on the heap. return type points to allocation owned by Heap.objects
    fn allocate<T: 'static + Object>(&mut self, data: T) -> NonNull<Allocation<T>> {
        let mut alloc = Box::new(Allocation {
            header: Header::default(),
            data,
        });

        // TODO: maybe box unnecessary?
        // TODO: maybe NonNull::new?
        let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
        self.objects.push(alloc);
        ptr
    }

    /// Root a GCd object.
    pub fn root<T: 'static + Object + ?Sized>(&mut self, obj: Gc<T>) -> Root<T> {
        obj.as_allocation().root();
        Root { ptr: obj.ptr }
    }

    /// Manage an object on the heap.
    ///
    /// Data will be GCable.
    pub fn manage<T: 'static + Object>(&mut self, data: T) -> Root<T> {
        let root = Root {
            ptr: self.allocate(data),
        };
        root.as_allocation().root();
        root
    }

    /// Manage an uncopyable object on the heap.
    ///
    /// As in manage, data will be GCable.
    pub fn manage_unique<T: 'static + Object>(&mut self, data: T) -> UniqueRoot<T> {
        let root = UniqueRoot {
            ptr: self.allocate(data),
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
impl<T: 'static + Object + ?Sized> Gc<T> {
    // safety: the lifetime of the returned allocation is bound to self
    fn as_allocation(&self) -> &Allocation<T> {
        unsafe { &self.ptr.as_ref() }
    }
}

impl<T: 'static + Object + ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        *self
    }
}

impl<T: 'static + Object + ?Sized> Copy for Gc<T> {}

impl<T: 'static + Object + ?Sized> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.as_allocation().data
    }
}

impl<T: fmt::Debug + 'static + Object + ?Sized> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        write!(f, "Gc({:?})", inner)
    }
}

impl<T: fmt::Display + 'static + Object + ?Sized> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        inner.fmt(f)
    }
}

impl<T: 'static + Object + ?Sized> Object for Gc<T> {
    fn trace(&self) {
        self.as_allocation().trace()
    }
}
// }}}

// Root<T> impl, Clone, Deref, Debug, Display, Object, Drop {{{
impl<T: 'static + Object + ?Sized> Root<T> {
    // safety: the lifetime of the returned allocation is bound to self
    fn as_allocation(&self) -> &Allocation<T> {
        unsafe { &self.ptr.as_ref() }
    }

    // TODO: this might allow accidental copying of stuff we didn't want to copy...
    pub(crate) fn as_gc(&self) -> Gc<T> {
        self.as_allocation().root();
        Gc { ptr: self.ptr }
    }

    // TODO: remove? allows some weird stuff combined with as_gc, necessitating a call to root
    /// Upgrade to a unique root.
    ///
    /// Panics if there is more than one root around.
    pub fn upgrade(self) -> UniqueRoot<T> {
        assert_eq!(
            1,
            self.as_allocation().header.roots.load(Ordering::Relaxed),
            "cannot upgrade if there is more than one root"
        );
        self.as_allocation().root();
        UniqueRoot { ptr: self.ptr }
    }
}

impl<T: 'static + Object + ?Sized> Clone for Root<T> {
    fn clone(&self) -> Root<T> {
        self.as_allocation().root();
        Root { ptr: self.ptr }
    }
}

impl<T: 'static + Object + ?Sized> Deref for Root<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.as_allocation().data
    }
}

impl<T: fmt::Debug + 'static + Object + ?Sized> fmt::Debug for Root<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        write!(f, "Root({:?})", inner)
    }
}

impl<T: fmt::Display + 'static + Object + ?Sized> fmt::Display for Root<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        inner.fmt(f)
    }
}

impl<T: 'static + Object + ?Sized> Object for Root<T> {
    fn trace(&self) {
        self.as_allocation().trace()
    }
}

impl<T: 'static + Object + ?Sized> Drop for Root<T> {
    fn drop(&mut self) {
        self.as_allocation().unroot();
    }
}
// }}}

// UniqueRoot<T> impl, Deref, DerefMut, Debug, Display, Object, Drop {{{
impl<T: 'static + Object + ?Sized> UniqueRoot<T> {
    // safety: the lifetime of the returned allocation is bound to self
    fn as_allocation(&self) -> &Allocation<T> {
        unsafe { self.ptr.as_ref() }
    }

    fn as_allocation_mut(&mut self) -> &mut Allocation<T> {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: 'static + Object + ?Sized> Deref for UniqueRoot<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.as_allocation().data
    }
}

impl<T: 'static + Object + ?Sized> DerefMut for UniqueRoot<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.as_allocation_mut().data
    }
}

impl<T: fmt::Debug + 'static + Object + ?Sized> fmt::Debug for UniqueRoot<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        write!(f, "UniqueRoot({:?})", inner)
    }
}

impl<T: fmt::Display + 'static + Object + ?Sized> fmt::Display for UniqueRoot<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner: &T = &*self;
        inner.fmt(f)
    }
}

impl<T: 'static + Object + ?Sized> Object for UniqueRoot<T> {
    fn trace(&self) {
        self.as_allocation().trace()
    }

    fn type_name(&self) -> &'static str {
        "jiof3jio8w3jio"
    }
}

impl<T: 'static + Object + ?Sized> Drop for UniqueRoot<T> {
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
            let root = heap.manage(String::new());
            let mut _root = root.upgrade();
        }

        assert_eq!(1, heap.objects());
        assert_eq!(std::mem::size_of::<String>(), heap.collect());
        assert_eq!(0, heap.objects());
    }

    #[test]
    #[should_panic]
    fn no_aliasing() {
        use std::ptr;

        let mut heap = Heap::default();
        let root = heap.manage(String::new());

        let gc = root.as_gc();
        let mut unique = root.upgrade(); // panic

        let borrow: &str = gc.as_ref();
        let mut_borrow: &mut str = unique.as_mut();

        // !!!
        assert!(ptr::eq(borrow.as_ptr(), mut_borrow.as_ptr()));
    }
}
