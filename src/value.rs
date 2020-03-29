#![allow(non_snake_case)]

use core::fmt;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};

use crate::PiccoloError;
use downcast_rs::Downcast;
use slotmap::DefaultKey;
use slotmap::DenseSlotMap;
use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::cmp::Ordering::Equal;
use std::rc::Rc;

pub trait Object: Downcast + Debug + Display {
    fn type_name(&self) -> &'static str;
    fn gt(&self, other: &dyn Object) -> bool;
    fn eq(&self, other: &dyn Object) -> bool;
}
downcast_rs::impl_downcast!(Object);

impl Object for String {
    fn type_name(&self) -> &'static str {
        "string"
    }

    fn gt(&self, other: &dyn Object) -> bool {
        self > other.downcast_ref::<String>().unwrap()
    }

    fn eq(&self, other: &dyn Object) -> bool {
        self == other.downcast_ref::<String>().unwrap()
    }
}

#[derive(Debug)]
pub struct Idklol(pub f32);
impl Object for Idklol {
    fn type_name(&self) -> &'static str {
        "IDK lol"
    }

    fn gt(&self, other: &dyn Object) -> bool {
        self.0 > other.downcast_ref::<Idklol>().unwrap().0
    }

    fn eq(&self, other: &dyn Object) -> bool {
        println!("{:?} == {:?}", self, other);
        self.0 == other.downcast_ref::<Idklol>().unwrap().0
    }
}
impl Display for Idklol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "idklol({})", self.0)
    }
}

/// Wrapper type for piccolo values.
#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Object(DefaultKey),
    Nil,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn into<T>(self) -> T
    where
        Value: Into<T>,
    {
        std::convert::Into::<T>::into(self)
    }

    pub fn ref_string(&self) -> &String {
        match self {
            Value::String(s) => s,
            _ => panic!("tried to take reference to inner string - file a bug report!"),
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Value::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_double(&self) -> bool {
        match self {
            Value::Double(_) => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Value::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn type_name(&self, map: &DenseSlotMap<DefaultKey, Box<dyn Object>>) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Object(v) => map.get(*v).unwrap().type_name(),
            Value::Nil => "nil",
        }
    }

    pub fn fmt(&self, map: &DenseSlotMap<DefaultKey, Box<dyn Object>>) -> String {
        match self {
            Value::String(v) => format!("{}", v),
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(v) => format!("{}", map.get(*v).unwrap()),
            Value::Nil => String::new(),
        }
    }

    pub fn eq(&self, other: &Value, map: &DenseSlotMap<DefaultKey, Box<dyn Object>>) -> bool {
        match self {
            Value::String(l) => match other {
                Value::String(r) => l == r,
                _ => false,
            },
            Value::Bool(l) => match other {
                Value::Bool(r) => l == r,
                _ => false,
            },
            Value::Integer(l) => match other {
                Value::Integer(r) => l == r,
                _ => false,
            },
            Value::Double(l) => match other {
                Value::Double(r) => l == r,
                _ => false,
            },
            Value::Object(l) => match other {
                Value::Object(r) => map.get(*l).unwrap().eq(map.get(*r).unwrap().as_ref()),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn gt(&self, other: &Value, map: &DenseSlotMap<DefaultKey, Box<dyn Object>>) -> bool {
        match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => l > r,
                Value::Double(r) => *l as f64 > *r,
                _ => false,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l > *r as f64,
                Value::Double(r) => l > r,
                _ => false,
            },
            Value::String(l) => match other {
                Value::String(r) => l > r,
                _ => false,
            },
            Value::Bool(l) => match other {
                Value::Bool(r) => l > r,
                _ => false,
            },
            Value::Object(l) => match other {
                Value::Object(r) => map.get(*l).unwrap().gt(map.get(*r).unwrap().as_ref()),
                _ => false,
            },
            _ => false,
        }
    }
}

impl Into<String> for Value {
    fn into(self) -> String {
        match self {
            Value::String(t) => t,
            _ => panic!("could not cast {:?} to string", self),
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Bool(t) => t,
            _ => panic!("could not cast {:?} to bool", self),
        }
    }
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::Integer(t) => t,
            _ => panic!("could not cast {:?} to i64", self),
        }
    }
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::Double(t) => t,
            _ => panic!("could not cast {:?} to f64", self),
        }
    }
}

impl From<String> for Value {
    fn from(String: String) -> Self {
        Value::String(String)
    }
}

impl From<bool> for Value {
    fn from(Bool: bool) -> Self {
        Value::Bool(Bool)
    }
}

impl From<i64> for Value {
    fn from(Integer: i64) -> Self {
        Value::Integer(Integer)
    }
}

impl From<f64> for Value {
    fn from(Double: f64) -> Self {
        Value::Double(Double)
    }
}
