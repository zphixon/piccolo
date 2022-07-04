//! Contains types for working with Piccolo values.

use crate::{
    compiler::{Token, TokenKind},
    error::PiccoloError,
    runtime::{
        builtin,
        builtin::NativeFunction,
        interner::StringPtr,
        memory::{Heap, Ptr},
        Arity, Object,
    },
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Function {
    pub arity: Arity,
    pub chunk: usize,
    pub name: StringPtr,
}

#[derive(Clone, Copy)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(StringPtr),
    Function(Function),
    NativeFunction(builtin::NativeFunction),
    Object(Ptr),
    Nil,
}

impl Value {
    pub fn as_ptr(&self) -> Ptr {
        if let Value::Object(ptr) = self {
            *ptr
        } else {
            panic!("called as_ptr on non-object");
        }
    }

    pub fn from_constant(c: Constant, heap: &mut Heap) -> Value {
        match c {
            Constant::Bool(b) => Value::Bool(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Double(d) => Value::Double(d),
            Constant::String(s) => Value::String(heap.alloc_string(s)),
            Constant::Function(f) => Value::Function(Function {
                arity: Arity::Exact(f.arity),
                chunk: f.chunk,
                name: heap.alloc_string(f.name),
            }),
            //Constant::NativeFunction(f) => Value::NativeFunction(f),
            //Constant::Object(Box<dyn Object>) => {} // TODO?
            Constant::Nil => Value::Nil,
        }
    }

    pub fn into_constant(self, heap: &Heap) -> Constant {
        match self {
            Value::Bool(b) => Constant::Bool(b),
            Value::Integer(i) => Constant::Integer(i),
            Value::Double(d) => Constant::Double(d),
            Value::String(ptr) => Constant::String(heap.get_string(ptr).unwrap().to_string()),
            Value::Nil => Constant::Nil,
            Value::Function(f) => Constant::Function(ConstantFunction {
                arity: f.arity.number(),
                name: heap.get_string(f.name).unwrap().to_string(),
                chunk: f.chunk,
            }),
            _ => todo!("{self:?}"),
        }
    }

    pub fn to_string(&self, heap: &Heap) -> String {
        self.format(heap)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Integer(_))
    }

    pub fn is_double(&self) -> bool {
        matches!(self, Value::Double(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function(_))
    }

    pub fn as_function(&self) -> Function {
        assert!(self.is_function());
        match self {
            Value::Function(f) => *f,
            _ => panic!(),
        }
    }

    pub fn is_native_function(&self) -> bool {
        matches!(self, Value::NativeFunction(_))
    }

    pub fn as_native_function(&self) -> NativeFunction {
        assert!(self.is_native_function());
        match self {
            Value::NativeFunction(f) => *f,
            _ => panic!(),
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn eq(&self, other: &Value) -> Option<bool> {
        Some(match self {
            Value::Bool(l) => match other {
                Value::Bool(r) => l == r,
                _ => None?,
            },
            Value::Integer(l) => match other {
                Value::Integer(r) => l == r,
                Value::Double(r) => *l as f64 == *r,
                _ => None?,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l == *r as f64,
                Value::Double(r) => l == r,
                _ => None?,
            },
            Value::String(l) => match other {
                Value::String(r) => l == r,
                _ => None?,
            },
            Value::Function(l) => match other {
                Value::Function(r) => l == r,
                _ => None?,
            },
            Value::NativeFunction(l) => match other {
                Value::NativeFunction(r) => *l == *r,
                _ => None?,
            },
            Value::Nil => match other {
                Value::Nil => true,
                _ => None?,
            },
            _ => None?,
        })
    }

    pub fn gt(&self, other: &Value) -> Option<bool> {
        Some(match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => l > r,
                Value::Double(r) => *l as f64 > *r,
                _ => None?,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l > *r as f64,
                Value::Double(r) => l > r,
                _ => None?,
            },
            _ => None?,
        })
    }

    pub fn lt(&self, other: &Value) -> Option<bool> {
        Some(match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => l < r,
                Value::Double(r) => (*l as f64) < (*r),
                _ => None?,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l < *r as f64,
                Value::Double(r) => l < r,
                _ => None?,
            },
            _ => None?,
        })
    }

    pub fn into<T>(self) -> T
    where
        T: From<Value>,
    {
        T::from(self)
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "Bool({v})"),
            Value::Integer(v) => write!(f, "Integer({v})"),
            Value::Double(v) => write!(f, "Double({v})"),
            Value::String(_) => write!(f, "String(?)"),
            Value::Function(_) => write!(f, "Function(?)"),
            Value::NativeFunction(_) => write!(f, "NativeFunction(?)"),
            Value::Object(_) => write!(f, "Object(?)"),
            Value::Nil => write!(f, "Nil"),
        }
    }
}

impl From<Value> for i64 {
    fn from(v: Value) -> Self {
        match v {
            Value::Integer(i) => i,
            _ => panic!("not an integer: {v:?}"),
        }
    }
}

impl From<Value> for f64 {
    fn from(v: Value) -> Self {
        match v {
            Value::Double(f) => f,
            _ => panic!("not a double: {v:?}"),
        }
    }
}

impl Object for Value {
    fn trace(&self, heap: &Heap) {
        match self {
            Value::Object(ptr) => {
                heap.trace(*ptr);
            }
            _ => {}
        }
    }

    fn type_name(&self) -> &'static str {
        "value"
    }

    fn format(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(b) => format!("{b}"),
            Value::Integer(i) => format!("{i}"),
            Value::Double(d) => format!("{d}"),
            Value::String(p) => heap.get_string(*p).unwrap().to_string(),
            Value::Function(f) => heap.get_string(f.name).unwrap().to_string(),
            Value::NativeFunction(f) => heap.get_string(f.name()).unwrap().to_string(),
            Value::Object(p) => heap.get(*p).unwrap().format(heap),
            Value::Nil => String::from("nil"),
        }
    }

    fn debug_format(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(v) => format!("Bool({v})"),
            Value::Integer(v) => format!("Integer({v})"),
            Value::Double(v) => format!("Double({v})"),
            Value::String(v) => format!("String({:?})", heap.get_string(*v).unwrap()),
            Value::Function(f) => format!("Function({:?})", heap.get_string(f.name).unwrap()),
            Value::NativeFunction(f) => {
                format!("NativeFunction({:?})", heap.get_string(f.name()).unwrap())
            }
            Value::Object(p) => format!("Object({})", heap.get(*p).unwrap().debug_format(heap)),
            Value::Nil => String::from("nil"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ConstantFunction {
    pub arity: usize,
    pub name: String,
    pub chunk: usize,
}

/// Compile-time constant Piccolo values.
///
/// Similar to [`Value`]. `Constant` is also used to return from Piccolo execution.
///
/// [`Value`]: ../value/enum.Value.html
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Function(ConstantFunction),
    Nil,
}

impl Constant {
    pub(crate) fn ref_string(&self) -> &str {
        match self {
            Constant::String(v) => v,
            _ => panic!("ref string on non-string"),
        }
    }

    pub(crate) fn try_from(token: Token) -> Result<Constant, PiccoloError> {
        Ok(match token.kind {
            TokenKind::String => Constant::String(crate::compiler::escape_string(token)?),
            TokenKind::Integer(v) => Constant::Integer(v),
            TokenKind::True => Constant::Bool(true),
            TokenKind::False => Constant::Bool(false),
            TokenKind::Double(v) => Constant::Double(v),
            TokenKind::Nil => Constant::Nil,
            _ => panic!("cannot create value from token {:?}", token),
        })
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Constant::String(v) => write!(f, "{}", v),
            Constant::Bool(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
            Constant::Function(v) => write!(f, "{}", v.name),
            Constant::Nil => write!(f, "nil"),
        }
    }
}
