//! Contains types for working with Piccolo values.

use crate::{
    compiler::{Token, TokenKind},
    error::{ErrorKind, PiccoloError},
    runtime::{
        builtin,
        builtin::BuiltinFunction,
        interner::StringPtr,
        memory::{Heap, Ptr},
        Arity, Object,
    },
};
use std::fmt::{Debug, Display, Formatter};

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
    BuiltinFunction(builtin::BuiltinFunction),
    Object(Ptr),
    Nil,
}

impl Value {
    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

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
            Constant::String(s) => Value::String(heap.interner_mut().allocate_string(s)),
            Constant::Function(f) => Value::Function(Function {
                arity: f.arity,
                chunk: f.chunk,
                name: heap.interner_mut().allocate_string(f.name),
            }),
            //Constant::BuiltinFunction(f) => Value::BuiltinFunction(f),
            Constant::Object(v) => Value::Object(heap.allocate_boxed(v)),
            Constant::Array(v) => {
                let values = v
                    .into_iter()
                    .map(|constant| Value::from_constant(constant, heap))
                    .collect();
                Value::Object(heap.allocate(Array { values }))
            }
            Constant::Nil => Value::Nil,
        }
    }

    pub fn into_constant(self, heap: &Heap) -> Constant {
        match self {
            Value::Bool(b) => Constant::Bool(b),
            Value::Integer(i) => Constant::Integer(i),
            Value::Double(d) => Constant::Double(d),
            Value::String(ptr) => Constant::String(heap.interner().get_string(ptr).to_string()),
            Value::Nil => Constant::Nil,
            Value::Function(f) => Constant::Function(ConstantFunction {
                arity: f.arity,
                name: heap.interner().get_string(f.name).to_string(),
                chunk: f.chunk,
            }),
            Value::BuiltinFunction(f) => Constant::Function(ConstantFunction {
                arity: f.arity,
                name: heap.interner().get_string(f.name).to_string(),
                // TODO this may be a problem if we:
                // 1. move a Value::BuiltinFunction out of the vm, making it a Constant::Function
                // 2. make that Constant::Function into a Value::Function
                // 3. attempt to execute that function on the vm
                // we should probably have a Constant::BuiltinFunction but meehhhhh
                chunk: 0,
            }),
            Value::Object(ptr) => {
                let object = heap.get(ptr).clone_object();
                if let Ok(array) = object.downcast::<Array>() {
                    Constant::Array(
                        array
                            .values
                            .into_iter()
                            .map(|value| value.into_constant(heap))
                            .collect(),
                    )
                } else {
                    panic!(
                        "cannot convert object {} to constant",
                        heap.get(ptr).debug_format(heap)
                    );
                }
            }
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
        matches!(self, Value::BuiltinFunction(_))
    }

    pub fn as_native_function(&self) -> BuiltinFunction {
        assert!(self.is_native_function());
        match self {
            Value::BuiltinFunction(f) => *f,
            _ => panic!(),
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn into<T>(self) -> T
    where
        T: From<Value>,
    {
        T::from(self)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "Bool({v})"),
            Value::Integer(v) => write!(f, "Integer({v})"),
            Value::Double(v) => write!(f, "Double({v})"),
            Value::String(_) => write!(f, "String(?)"),
            Value::Function(_) => write!(f, "Function(?)"),
            Value::BuiltinFunction(_) => write!(f, "BuiltinFunction(?)"),
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
        if let Value::Object(ptr) = self {
            heap.trace(*ptr);
        }
    }

    fn type_name(&self, heap: &Heap) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::Integer(_) => "int",
            Value::Double(_) => "float",
            Value::String(_) => "string",
            Value::Function(_) => "function",
            Value::BuiltinFunction(_) => "function",
            Value::Object(ptr) => heap.get(*ptr).type_name(heap),
            Value::Nil => "nil",
        }
    }

    fn format(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(b) => format!("{b}"),
            Value::Integer(i) => format!("{i}"),
            Value::Double(d) => format!("{d}"),
            Value::String(p) => heap.interner().get_string(*p).to_string(),
            Value::Function(f) => heap.interner().get_string(f.name).to_string(),
            Value::BuiltinFunction(f) => heap.interner().get_string(f.name()).to_string(),
            Value::Object(p) => heap.get(*p).format(heap),
            Value::Nil => String::from("nil"),
        }
    }

    fn debug_format(&self, heap: &Heap) -> String {
        match self {
            Value::Bool(v) => format!("Bool({v})"),
            Value::Integer(v) => format!("Integer({v})"),
            Value::Double(v) => format!("Double({v})"),
            Value::String(v) => format!("String({:?})", heap.interner().get_string(*v)),
            Value::Function(f) => format!("Function({:?})", heap.interner().get_string(f.name)),
            Value::BuiltinFunction(f) => {
                format!(
                    "BuiltinFunction({:?})",
                    heap.interner().get_string(f.name())
                )
            }
            Value::Object(p) => heap.get(*p).debug_format(heap),
            Value::Nil => String::from("nil"),
        }
    }

    fn eq(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        if let Value::Bool(l) = *self {
            if let Value::Bool(r) = other {
                return Ok(l == r);
            }
        } else if let Value::Integer(l) = *self {
            if let Value::Integer(r) = other {
                return Ok(l == r);
            } else if let Value::Double(r) = other {
                return Ok(l as f64 == r);
            }
        } else if let Value::Double(l) = *self {
            if let Value::Integer(r) = other {
                return Ok(l == r as f64);
            } else if let Value::Double(r) = other {
                return Ok(l == r);
            }
        } else if let Value::String(l) = *self {
            if let Value::String(r) = other {
                return Ok(l == r);
            }
        } else if let Value::Function(l) = *self {
            if let Value::Function(r) = other {
                return Ok(l == r);
            }
        } else if let Value::BuiltinFunction(l) = *self {
            if let Value::BuiltinFunction(r) = other {
                return Ok(l == r);
            }
        } else if let Value::Object(l) = *self {
            return heap.get(l).eq(heap, other);
        } else if let Value::Nil = *self {
            if let Value::Nil = other {
                return Ok(true);
            }
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }

    fn gt(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        match *self {
            Value::Integer(l) => match other {
                Value::Integer(r) => return Ok(l > r),
                Value::Double(r) => return Ok(l as f64 > r),
                _ => {}
            },
            Value::Double(l) => match other {
                Value::Integer(r) => return Ok(l > r as f64),
                Value::Double(r) => return Ok(l > r),
                _ => {}
            },
            Value::Object(ptr) => return heap.get(ptr).gt(heap, other),
            _ => {}
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }

    fn lt(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        match *self {
            Value::Integer(l) => match other {
                Value::Integer(r) => return Ok(l < r),
                Value::Double(r) => return Ok((l as f64) < r),
                _ => {}
            },
            Value::Double(l) => match other {
                Value::Integer(r) => return Ok(l < r as f64),
                Value::Double(r) => return Ok(l < r),
                _ => {}
            },
            Value::Object(l) => return heap.get(l).lt(heap, other),
            _ => {}
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }

    fn get(&self, _: Ptr, heap: &Heap, index_value: Value) -> Result<Value, PiccoloError> {
        match self {
            Value::Object(ptr) => heap.get(*ptr).get(*ptr, heap, index_value),
            _ => Err(PiccoloError::new(ErrorKind::CannotGet {
                object: self.type_name(heap).to_string(),
                index: index_value.format(heap),
            })),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ConstantFunction {
    pub arity: Arity,
    pub name: String,
    pub chunk: usize,
}

/// Compile-time constant Piccolo values.
///
/// Similar to [`Value`]. `Constant` is also used to return from Piccolo execution.
///
/// [`Value`]: ../value/enum.Value.html
pub enum Constant {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Function(ConstantFunction),
    Array(Vec<Constant>),
    Object(Box<dyn Object>),
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

impl Debug for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::String(v) => write!(f, "{}", v),
            Constant::Bool(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
            Constant::Function(v) => write!(f, "{}", v.name),
            Constant::Array(v) => {
                let mut s = String::from("[");
                for (i, value) in v.iter().enumerate() {
                    s.push_str(&value.to_string());
                    if i + 1 != v.len() {
                        s.push_str(", ");
                    }
                }
                s.push(']');
                write!(f, "{s}")
            }
            Constant::Object(object) => write!(f, "Object{:p}", object.as_ref()),
            Constant::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Constant::String(v) => write!(f, "{}", v),
            Constant::Bool(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
            Constant::Function(v) => write!(f, "{}", v.name),
            Constant::Array(v) => {
                let mut s = String::from("[");
                for (i, value) in v.iter().enumerate() {
                    s.push_str(&value.to_string());
                    if i + 1 != v.len() {
                        s.push_str(", ");
                    }
                }
                s.push(']');
                write!(f, "{s}")
            }
            Constant::Object(_) => write!(f, "object"),
            Constant::Nil => write!(f, "nil"),
        }
    }
}

impl Clone for Constant {
    fn clone(&self) -> Self {
        match self {
            Constant::String(v) => Constant::String(v.clone()),
            Constant::Bool(v) => Constant::Bool(*v),
            Constant::Integer(v) => Constant::Integer(*v),
            Constant::Double(v) => Constant::Double(*v),
            Constant::Function(v) => Constant::Function(v.clone()),
            Constant::Array(v) => Constant::Array(v.clone()),
            Constant::Object(v) => Constant::Object(v.clone_object()),
            Constant::Nil => Constant::Nil,
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        use Constant::*;
        match (self, other) {
            (String(l), String(r)) => l == r,
            (Bool(l), Bool(r)) => l == r,
            (Integer(l), Integer(r)) => l == r,
            (Double(l), Double(r)) => l == r,
            (Function(l), Function(r)) => l == r,
            (Array(l), Array(r)) => l == r,
            (Object(l), Object(r)) => std::ptr::eq(l, r),
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct Array {
    values: Vec<Value>,
}

impl Array {
    pub fn new(len: usize) -> Self {
        Array {
            values: Vec::with_capacity(len),
        }
    }

    pub fn new_with(values: Vec<Value>) -> Self {
        Array { values }
    }
}

fn array_push(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    let this = values[0].as_ptr();

    if let Some(array) = unsafe { heap.get_mut(this) }.downcast_mut::<Array>() {
        array.values.push(values[1]);
    } else {
        panic!("called array_push with non-Array");
    }

    Ok(Value::Nil)
}

impl Object for Array {
    fn trace(&self, heap: &Heap) {
        for value in self.values.iter() {
            value.trace(heap);
        }
    }

    fn type_name(&self, _: &Heap) -> &'static str {
        "array"
    }

    fn format(&self, heap: &Heap) -> String {
        let mut s = String::from("[");
        for (i, value) in self.values.iter().enumerate() {
            s.push_str(&value.format(heap));
            if i + 1 != self.values.len() {
                s.push_str(", ");
            }
        }
        s.push(']');
        s
    }

    fn debug_format(&self, heap: &Heap) -> String {
        let mut s = String::from("[");
        for (i, value) in self.values.iter().enumerate() {
            s.push_str(&value.debug_format(heap));
            if i + 1 != self.values.len() {
                s.push_str(", ");
            }
        }
        s.push(']');
        s
    }

    fn get(&self, this: Ptr, heap: &Heap, index_value: Value) -> Result<Value, PiccoloError> {
        if let Value::Integer(index) = index_value {
            let index: usize = index.try_into().map_err(|_| {
                PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: index_value.format(heap),
                })
                .msg_string(format!("Cannot index with {}", index_value.format(heap)))
            })?;

            if index >= self.values.len() {
                return Err(PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: format!("{index}"),
                })
                .msg_string(format!("Out of bounds of length {}", self.values.len())));
            }

            return Ok(self.values[index]);
        } else if let Value::String(string) = index_value {
            // slow?
            // if heap.interner().get_string(string) == "len" {}

            let len = heap.interner().get_string_ptr("len").unwrap();
            if string == len {
                return Ok(Value::Integer(self.values.len() as i64));
            } else if heap.interner().get_string(string) == "push" {
                return Ok(Value::BuiltinFunction(BuiltinFunction {
                    name: heap.interner().get_string_ptr("push").unwrap(),
                    arity: Arity::Exact(2),
                    ptr: array_push,
                    this: Some(this),
                }));
            } else {
                return Err(PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: index_value.format(heap),
                }));
            }
        }

        Err(PiccoloError::new(ErrorKind::CannotGet {
            object: String::from("array"),
            index: index_value.format(heap),
        })
        .msg_string(format!(
            "Cannot index with type {}",
            index_value.type_name(heap)
        )))
    }

    fn set(&mut self, heap: &Heap, index_value: Value, value: Value) -> Result<(), PiccoloError> {
        if let Value::Integer(index) = index_value {
            let index: usize = index.try_into().map_err(|_| {
                PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: value.format(heap),
                })
                .msg_string(format!("Cannot index with {}", index_value.format(heap)))
            })?;

            if index >= self.values.len() {
                return Err(PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: format!("{index}"),
                })
                .msg_string(format!("Out of bounds of length {}", self.values.len())));
            }

            self.values[index] = value;
            return Ok(());
        }

        Err(PiccoloError::new(ErrorKind::CannotSet {
            object: String::from("array"),
            property: index_value.format(heap),
            value: value.format(heap),
        })
        .msg("Only integer properties may be modified"))
    }

    fn eq(&self, heap: &Heap, other: Value) -> Result<bool, PiccoloError> {
        if let Value::Object(ptr) = other {
            if let Some(other) = heap.get(ptr).downcast_ref::<Array>() {
                if self.values.len() != other.values.len() {
                    return Ok(false);
                }

                for (l, r) in self.values.iter().zip(other.values.iter()) {
                    if !l.eq(heap, *r)? {
                        return Ok(false);
                    }
                }

                return Ok(true);
            }
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(heap).to_string(),
            exp: self.type_name(heap).to_string(),
        }))
    }
}
