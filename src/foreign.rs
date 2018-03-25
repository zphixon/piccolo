use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::rc::Rc;

use super::*;

#[derive(Clone)]
pub struct ForeignOuter {
    pub inner: Rc<RefCell<Foreign>>,
}

impl fmt::Debug for ForeignOuter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(foreign {:?})", self.inner.borrow())
    }
}

impl fmt::Display for ForeignOuter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner.borrow())
    }
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
    pub fn set(
        &mut self,
        name: &str,
        value: ::value::Value,
    ) -> Result<value::Value, err::PiccoloError> {
        self.inner.borrow_mut().set(name, value)
    }
    pub fn is<T: Foreign>(&self) -> bool {
        self.inner.borrow().is::<T>()
    }
    pub fn call(
        &mut self,
        interp: &mut interp::Interpreter,
        args: &[value::Value],
    ) -> Result<value::Value, err::PiccoloError> {
        self.inner.borrow_mut().call(interp, args)
    }
}

// cheating
#[repr(C)]
pub struct TraitObject {
    pub data: *mut (),
    pub vtable: *mut (),
}

pub trait Foreign: Any + fmt::Display + fmt::Debug {
    fn get_name(&self) -> &'static str;

    fn compare(&self, _rhs: &ForeignOuter) -> Option<cmp::Ordering> {
        None
    }

    fn get(&self, _name: &str) -> Option<::value::Value> {
        None
    }

    fn set(
        &mut self,
        name: &str,
        _value: ::value::Value,
    ) -> Result<value::Value, err::PiccoloError> {
        Err(err::PiccoloError::new(
            err::ErrorKind::NoSuchField,
            &format!("No field named {} on {}", name, self.get_name()),
            0,
        ))
    }

    fn call(
        &mut self,
        _interp: &mut interp::Interpreter,
        _args: &[value::Value],
    ) -> Result<value::Value, err::PiccoloError> {
        Err(err::PiccoloError::new(
            err::ErrorKind::NonFunction,
            "Cannot call non-function",
            0,
        ))
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

pub struct ForeignFunc {
    pub inner:
        fn(&mut interp::Interpreter, &[value::Value]) -> Result<value::Value, err::PiccoloError>,
}

impl Foreign for ForeignFunc {
    fn get_name(&self) -> &'static str {
        "fn"
    }

    fn call(
        &mut self,
        interp: &mut interp::Interpreter,
        args: &[value::Value],
    ) -> Result<value::Value, err::PiccoloError> {
        (self.inner)(interp, args)
    }
}

impl fmt::Debug for ForeignFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn")
    }
}

impl fmt::Display for ForeignFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn")
    }
}

#[derive(Foreign, Debug)]
pub struct Test {
    pub inner: String,
}

impl fmt::Display for Test {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

pub struct Array {
    pub inner: Vec<::value::Value>,
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::from("[");
        if self.inner.len() > 0 {
            for i in 0..self.inner.len() - 1 {
                s.push_str(&format!("{}, ", self.inner[i].clone()));
            }
            s.push_str(&format!("{}", self.inner[self.inner.len() - 1].clone()));
        }
        s.push_str("]");
        write!(f, "{}", s)
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.inner)
    }
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
                return Some(::std::cmp::Ordering::Equal);
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

    fn set(&mut self, i: &str, value: ::value::Value) -> Result<value::Value, err::PiccoloError> {
        if let Ok(i) = i.parse::<usize>() {
            if i == self.inner.len() {
                self.inner.resize(i + 1, value::Value::Nil);
            }
            if i < self.inner.len() {
                self.inner[i] = value.clone();
                Ok(value)
            } else {
                Err(err::PiccoloError::new(
                    err::ErrorKind::IndexError,
                    &format!("Index was {} but length was {}", i, self.inner.len()),
                    0,
                ))
            }
        } else {
            Err(err::PiccoloError::new(
                err::ErrorKind::IndexError,
                &format!("Could not index with non-integer {}", i),
                0,
            ))
        }
    }
}
