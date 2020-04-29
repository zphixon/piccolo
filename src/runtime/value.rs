//! Contains types for working with Piccolo values at runtime.

use super::memory::Heap;
use super::object::Object;

/// Wrapper type for runtime Piccolo values.
///
/// `Value::Object` is a pointer into a [`Heap`].
///
/// [`Heap`]: ../memory/struct.Heap.html
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Double(f64),
    Object(usize),
    Nil,
}

impl Value {
    /// A value is only false-y if it is of type bool and false, or of type nil.
    /// All other values are truth-y.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    /// Converts the value into a type T for which Value implements Into<T>.
    pub fn into_object<T: Object>(self, heap: &mut Heap) -> Option<Box<T>> {
        match self {
            Value::Object(ptr) => heap.take(ptr).downcast::<T>().ok(),
            _ => None,
        }
    }

    pub fn into<T>(self) -> T
    where
        Value: Into<T>,
    {
        std::convert::Into::into(self)
    }

    /// Attempts to clone a value. Panics if it doesn't succeed.
    pub fn try_clone(&self, heap: &mut Heap) -> Option<Value> {
        Some(match self {
            Value::Object(v) => Value::Object(heap.try_copy(*v)?),
            Value::Bool(v) => Value::Bool(*v),
            Value::Integer(v) => Value::Integer(*v),
            Value::Double(v) => Value::Double(*v),
            Value::Nil => Value::Nil,
        })
    }

    pub fn is_string(&self, heap: &Heap) -> bool {
        match self {
            Value::Object(ptr) => heap.deref(*ptr).is::<String>(),
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

    // TODO: probably move anything that derefs a Value::Object into Heap

    /// Returns the type name of a value.
    pub fn type_name(&self, heap: &Heap) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Object(v) => heap.deref(*v).type_name(),
            Value::Nil => "nil",
        }
    }

    /// Formats the value.
    pub fn fmt(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(v) => format!("{}", heap.deref(*v)),
            Value::Nil => "nil".into(),
        }
    }

    /// Formats the value.
    pub fn dbg(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(v) => format!("bool({})", v),
            Value::Integer(v) => format!("integer({})", v),
            Value::Double(v) => format!("double({})", v),
            Value::Object(v) => format!("*{}({:?})", heap.deref(*v).type_name(), heap.deref(*v)),
            Value::Nil => "Nil".into(),
        }
    }

    /// Tests a value for equality. Returns `None` if incomparable.
    pub fn eq(&self, other: &Value, heap: &Heap) -> Option<bool> {
        match self {
            Value::Bool(l) => match other {
                Value::Bool(r) => Some(l == r),
                _ => None,
            },
            Value::Integer(l) => match other {
                Value::Integer(r) => Some(l == r),
                Value::Double(r) => Some(*l as f64 == *r),
                _ => None,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => Some(*l == *r as f64),
                Value::Double(r) => Some(l == r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => {
                    if l == r {
                        Some(true)
                    } else {
                        let lhs = heap.deref(*l);
                        let rhs = heap.deref(*r);
                        lhs.eq(rhs)
                    }
                }
                _ => None,
            },
            Value::Nil => match other {
                Value::Nil => Some(true),
                _ => Some(false),
            },
        }
    }

    pub fn lt(&self, other: &Value, heap: &Heap) -> Option<bool> {
        match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => Some(l < r),
                Value::Double(r) => Some((*l as f64) < *r),
                _ => None,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => Some(*l < *r as f64),
                Value::Double(r) => Some(l < r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => {
                    let lhs = heap.deref(*l);
                    let rhs = heap.deref(*r);
                    lhs.lt(rhs)
                }
                _ => None,
            },
            _ => None,
        }
    }

    pub fn gt(&self, other: &Value, heap: &Heap) -> Option<bool> {
        match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => Some(l > r),
                Value::Double(r) => Some(*l as f64 > *r),
                _ => None,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => Some(*l > *r as f64),
                Value::Double(r) => Some(l > r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => {
                    let lhs = heap.deref(*l);
                    let rhs = heap.deref(*r);
                    lhs.gt(rhs)
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
            s.push_str(&format!("{}, ", item.dbg(heap)));
        }
        format!("{}]", &s[..s.len() - 2])
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Bool(v) => v,
            _ => panic!("could not cast to bool"),
        }
    }
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::Integer(v) => v,
            _ => panic!("could not cast to i64"),
        }
    }
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::Double(v) => v,
            _ => panic!("could not cast to f64"),
        }
    }
}
