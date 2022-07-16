//! Contains types for working with Piccolo values.

use crate::{
    compiler::{Token, TokenKind},
    error::{ErrorKind, PiccoloError},
    runtime::{
        builtin,
        builtin::BuiltinFunction,
        interner::{Interner, StringPtr},
        memory::{Heap, Ptr},
        Arity, Context, ContextMut, Object, This,
    },
};
use std::fmt::{Debug, Formatter};

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

    pub(crate) fn from_constant(c: &Constant) -> Value {
        match c {
            Constant::Bool(b) => Value::Bool(*b),
            Constant::Integer(i) => Value::Integer(*i),
            Constant::Double(d) => Value::Double(*d),
            Constant::StringPtr(s) => Value::String(*s),
            Constant::Function(f) => Value::Function(Function {
                arity: f.arity,
                chunk: f.chunk,
                name: f.name,
            }),
            //Constant::BuiltinFunction(f) => Value::BuiltinFunction(f),
            Constant::Nil => Value::Nil,
        }
    }

    pub fn to_string(&self, ctx: Context) -> String {
        self.format(ctx)
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

    pub fn is_builtin_function(&self) -> bool {
        matches!(self, Value::BuiltinFunction(_))
    }

    pub fn as_builtin_function(&self) -> BuiltinFunction {
        assert!(self.is_builtin_function());
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

    fn type_name(&self, ctx: Context) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::Integer(_) => "int",
            Value::Double(_) => "float",
            Value::String(_) => "string",
            Value::Function(_) => "function",
            Value::BuiltinFunction(_) => "function",
            Value::Object(ptr) => ctx.heap.get(*ptr).type_name(ctx),
            Value::Nil => "nil",
        }
    }

    fn format(&self, ctx: Context) -> String {
        match self {
            Value::Bool(b) => format!("{b}"),
            Value::Integer(i) => format!("{i}"),
            Value::Double(d) => format!("{d}"),
            Value::String(p) => ctx.interner.get_string(*p).to_string(),
            Value::Function(f) => ctx.interner.get_string(f.name).to_string(),
            Value::BuiltinFunction(f) => ctx.interner.get_string(f.name()).to_string(),
            Value::Object(p) => ctx.format(*p),
            Value::Nil => String::from("nil"),
        }
    }

    fn debug_format(&self, ctx: Context) -> String {
        match self {
            Value::Bool(v) => format!("Bool({v})"),
            Value::Integer(v) => format!("Integer({v})"),
            Value::Double(v) => format!("Double({v})"),
            Value::String(v) => format!("String({:?})", ctx.interner.get_string(*v)),
            Value::Function(f) => format!("Function({:?})", ctx.interner.get_string(f.name)),
            Value::BuiltinFunction(f) => {
                format!("BuiltinFunction({:?})", ctx.interner.get_string(f.name()))
            }
            Value::Object(p) => ctx.heap.get(*p).debug_format(ctx),
            Value::Nil => String::from("nil"),
        }
    }

    fn eq(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
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
            return ctx.heap.get(l).eq(ctx, other);
        } else if let Value::Nil = *self {
            if let Value::Nil = other {
                return Ok(true);
            }
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }

    fn gt(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
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
            Value::Object(ptr) => return ctx.heap.get(ptr).gt(ctx, other),
            _ => {}
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }

    fn lt(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
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
            Value::Object(l) => return ctx.heap.get(l).lt(ctx, other),
            _ => {}
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }

    fn get(&self, ctx: Context, _: This, index_value: Value) -> Result<Value, PiccoloError> {
        match (self, index_value) {
            (Value::String(this), Value::String(property))
                if ctx.interner.get_string(property) == "len" =>
            {
                Ok(Value::Integer(this.len as i64))
            }

            (Value::String(this), Value::String(property))
                if ctx.interner.get_string(property) == "trim" =>
            {
                Ok(Value::BuiltinFunction(BuiltinFunction {
                    arity: Arity::Exact(1),
                    name: ctx.interner.get_string_ptr("trim").unwrap(),
                    ptr: string_trim,
                    this: This::String(*this),
                }))
            }

            (Value::Object(ptr), index) => ctx.heap.get(*ptr).get(ctx, This::Ptr(*ptr), index),

            _ => Err(PiccoloError::new(ErrorKind::CannotGet {
                object: self.type_name(ctx).to_string(),
                index: index_value.format(ctx),
            })),
        }
    }
}

fn string_trim(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    let this = args[0];
    if let Value::String(string) = this {
        // TODO investigate adding a byte length field to StringPtr
        let string = ctx.interner.get_string(string).trim().to_string();
        Ok(Value::String(ctx.interner.allocate_string(string)))
    } else {
        Err(PiccoloError::new(ErrorKind::InvalidArgument {
            exp: "string".to_string(),
            got: this.type_name(ctx.as_ref()).to_string(),
        }))
    }
}

/// Compile-time constant Piccolo values.
///
/// Similar to [`Value`]. `Constant` is also used to return from Piccolo execution.
///
/// [`Value`]: ../value/enum.Value.html
pub(crate) enum Constant {
    StringPtr(StringPtr),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Function(Function),
    Nil,
}

impl Constant {
    pub(crate) fn string_ptr(&self) -> StringPtr {
        if let Constant::StringPtr(ptr) = self {
            *ptr
        } else {
            panic!("string_ptr on non-string")
        }
    }

    pub(crate) fn try_from(
        interner: &mut Interner,
        token: Token,
    ) -> Result<Constant, PiccoloError> {
        Ok(match token.kind {
            TokenKind::String => {
                let string = crate::compiler::escape_string(token.lexeme)?;
                Constant::StringPtr(interner.allocate_string(string))
            }
            TokenKind::Integer(v) => Constant::Integer(v),
            TokenKind::True => Constant::Bool(true),
            TokenKind::False => Constant::Bool(false),
            TokenKind::Double(v) => Constant::Double(v),
            TokenKind::Nil => Constant::Nil,
            _ => panic!("cannot create value from token {:?}", token),
        })
    }

    pub(crate) fn debug(&self, interner: &Interner) -> String {
        match self {
            Constant::StringPtr(v) => format!("String({:?})", interner.get_string(*v)),
            Constant::Bool(v) => format!("Bool({})", v),
            Constant::Integer(v) => format!("Integer({})", v),
            Constant::Double(v) => format!("Double({})", v),
            Constant::Function(v) => format!("Function({})", interner.get_string(v.name)),
            Constant::Nil => format!("Nil"),
        }
    }
}

impl Clone for Constant {
    fn clone(&self) -> Self {
        match self {
            Constant::StringPtr(v) => Constant::StringPtr(v.clone()),
            Constant::Bool(v) => Constant::Bool(*v),
            Constant::Integer(v) => Constant::Integer(*v),
            Constant::Double(v) => Constant::Double(*v),
            Constant::Function(v) => Constant::Function(v.clone()),
            Constant::Nil => Constant::Nil,
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        use Constant::*;
        match (self, other) {
            (StringPtr(l), StringPtr(r)) => l == r,
            (Bool(l), Bool(r)) => l == r,
            (Integer(l), Integer(r)) => l == r,
            (Double(l), Double(r)) => l == r,
            (Function(l), Function(r)) => l == r,
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

fn array_push(ctx: &mut ContextMut, values: &[Value]) -> Result<Value, PiccoloError> {
    let this = values[0].as_ptr();

    if let Some(array) = unsafe { ctx.heap.get_mut(this) }.downcast_mut::<Array>() {
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

    fn type_name(&self, _: Context) -> &'static str {
        "array"
    }

    fn format(&self, ctx: Context) -> String {
        let mut s = String::from("[");
        for (i, value) in self.values.iter().enumerate() {
            s.push_str(&value.format(ctx));
            if i + 1 != self.values.len() {
                s.push_str(", ");
            }
        }
        s.push(']');
        s
    }

    fn debug_format(&self, ctx: Context) -> String {
        let mut s = String::from("[");
        for (i, value) in self.values.iter().enumerate() {
            s.push_str(&value.debug_format(ctx));
            if i + 1 != self.values.len() {
                s.push_str(", ");
            }
        }
        s.push(']');
        s
    }

    fn get(&self, ctx: Context, this: This, index_value: Value) -> Result<Value, PiccoloError> {
        if let Value::Integer(index) = index_value {
            let index: usize = index.try_into().map_err(|_| {
                PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: index_value.format(ctx),
                })
                .msg_string(format!("Cannot index with {}", index_value.format(ctx)))
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
            if ctx.interner.get_string(string) == "len" {
                return Ok(Value::Integer(self.values.len() as i64));
            } else if ctx.interner.get_string(string) == "push" {
                return Ok(Value::BuiltinFunction(BuiltinFunction {
                    name: ctx.interner.get_string_ptr("push").unwrap(),
                    arity: Arity::Exact(2),
                    ptr: array_push,
                    this,
                }));
            } else {
                return Err(PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: index_value.format(ctx),
                }));
            }
        }

        Err(PiccoloError::new(ErrorKind::CannotGet {
            object: String::from("array"),
            index: index_value.format(ctx),
        })
        .msg_string(format!(
            "Cannot index with type {}",
            index_value.type_name(ctx)
        )))
    }

    fn set(&mut self, ctx: Context, index_value: Value, value: Value) -> Result<(), PiccoloError> {
        if let Value::Integer(index) = index_value {
            let index: usize = index.try_into().map_err(|_| {
                PiccoloError::new(ErrorKind::CannotGet {
                    object: String::from("array"),
                    index: value.format(ctx),
                })
                .msg_string(format!("Cannot index with {}", index_value.format(ctx)))
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
            property: index_value.format(ctx),
            value: value.format(ctx),
        })
        .msg("Only integer properties may be modified"))
    }

    fn eq(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
        if let Value::Object(ptr) = other {
            if let Some(other) = ctx.heap.get(ptr).downcast_ref::<Array>() {
                if self.values.len() != other.values.len() {
                    return Ok(false);
                }

                for (l, r) in self.values.iter().zip(other.values.iter()) {
                    if !l.eq(ctx, *r)? {
                        return Ok(false);
                    }
                }

                return Ok(true);
            }
        }

        Err(PiccoloError::new(ErrorKind::CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }
}
