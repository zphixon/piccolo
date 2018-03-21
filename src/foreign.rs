use std::any::{Any, TypeId};
use std::cmp;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug)]
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
    pub fn is<T: Foreign>(&self) -> bool {
        self.inner.borrow().is::<T>()
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

    fn compare(&self, _rhs: &ForeignOuter) -> Option<cmp::Ordering> {
        None
    }

    fn get(&self, _name: &str) -> Option<::value::Value> {
        None
    }

    fn set(&mut self, _name: &str, _value: ::value::Value) -> Result<(), ()> {
        Err(())
    }
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

#[derive(Debug)]
pub struct Array {
    pub inner: Vec<::value::Value>,
}

impl Foreign for Array {
    fn get_name(&self) -> &'static str {
        "array"
    }

    fn compare(&self, rhs: &ForeignOuter) -> Option<cmp::Ordering> {
        let rhs = rhs.inner.borrow();
        if rhs.is::<Array>() {
            let rhs = rhs.downcast_ref::<Array>().unwrap();
            if self.inner == rhs.inner {
                return Some(::std::cmp::Ordering::Equal)
            }
        }
        None
    }

    fn get(&self, i: &str) -> Option<::value::Value> {
        if let Ok(i) = i.parse::<usize>() {
            if i < self.inner.len() {
                Some(self.inner[i].clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn set(&mut self, i: &str, value: ::value::Value) -> Result<(), ()> {
        i.parse::<usize>()
            .map(|i| {
                if i >= self.inner.len() {
                    self.inner.resize(i + 1, ::value::Value::Nil);
                }
                self.inner[i] = value;
            })
            .map_err(|_| ())
    }
}

