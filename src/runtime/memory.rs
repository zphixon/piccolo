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

    fn gt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn lt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn eq(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn get(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn set(&self, _other: &dyn Object) -> Option<bool> {
        None
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
    pub fn unique<T: 'static + Object>(&mut self, data: T) -> UniqueRoot<T> {
        let root = UniqueRoot {
            ptr: self.allocate(data),
        };
        root.as_allocation().root();
        root
    }

    /// Take out the trash.
    pub fn collect(&mut self) {
        self.mark();
        self.sweep();
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
        write!(f, "Gc({:?})", &*self)
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

    /// Upgrade to a unique root.
    ///
    /// Panics if there is more than one root around.
    pub fn upgrade(self) -> UniqueRoot<T> {
        assert!(self.as_allocation().header.roots.load(Ordering::Relaxed) == 1);
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
        write!(f, "Root({:?})", &*self)
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
        write!(f, "UniqueRoot({:?})", &*self)
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
    fn heap_stuff() {
        let mut heap = Heap::default();
        let root = heap.manage(String::new());
        let mut s = root.upgrade();
        s.push_str("hi");
        println!("{}", s);

        let root2 = heap.manage(String::new());
        let mut s2 = root2.upgrade();
        s2.push_str(&s);
        s2.push_str("jfo3iawjoeio");
        println!("{}", s2);

        assert_eq!("hijfo3iawjoeio", *s2);
    }
}
