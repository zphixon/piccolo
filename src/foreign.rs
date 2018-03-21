use std::any::{Any, TypeId};
use std::cmp;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub struct ForeignOuter {
    pub inner: Rc<RefCell<Foreign>>,
}

impl ForeignOuter {
    pub fn new<T: Foreign>(inner: T) -> Self {
        ForeignOuter {
            inner: Rc::new(RefCell::new(inner)),
        }
    }
    pub fn get_name(&self) -> &'static str {
        self.inner.borrow().get_name()
    }
    pub fn compare(&self, rhs: &ForeignOuter) -> Option<cmp::Ordering> {
        self.inner.borrow().compare(rhs)
    }
    pub fn get(&self, name: &str) -> Option<::value::Value> {
        self.inner.borrow().get(name)
    }
    pub fn set(&mut self, name: &str, value: ::value::Value) -> Result<(), ()> {
        self.inner.borrow_mut().set(name, value)
    }
}

// cheating
#[repr(C)]
pub struct TraitObject {
    pub data: *mut (),
    pub vtable: *mut (),
}

pub trait Foreign: Any + ::std::fmt::Debug {
    fn get_name(&self) -> &'static str;
    fn compare(&self, rhs: &ForeignOuter) -> Option<cmp::Ordering>;
    fn get(&self, name: &str) -> Option<::value::Value>;
    fn set(&mut self, name: &str, value: ::value::Value) -> Result<(), ()>;
}

// put it in a separate block because reasons
impl Foreign {
    pub fn is<T: Foreign>(&self) -> bool {
        self.get_type_id() == TypeId::of::<T>()
    }

    pub fn downcast_ref<T: Foreign>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe {
                let obj: TraitObject = ::std::mem::transmute(self);
                Some(&*(obj.data as *const T))
            }
        } else {
            None
        }
    }

    pub fn downcast_mut<T: Foreign>(&self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe {
                let obj: TraitObject = ::std::mem::transmute(self);
                Some(&mut *(obj.data as *mut T))
            }
        } else {
            None
        }
    }
}

#[derive(Foreign, Debug)]
pub struct Test {
    pub inner: String,
}
