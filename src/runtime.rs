//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod builtin;
pub mod chunk;
pub mod interner;
pub mod memory;
pub mod op;
pub mod value;
pub mod vm;

use crate::{
    error::PiccoloError,
    make_error,
    runtime::{
        interner::{Interner, StringPtr},
        memory::{Heap, Ptr},
        value::Value,
    },
};

#[derive(Clone, Copy)]
pub struct Context<'a, 'b> {
    pub heap: &'a Heap,
    pub interner: &'b Interner,
}

impl Context<'_, '_> {
    pub fn debug(&self, ptr: Ptr) -> String {
        self.heap.get(ptr).debug_format(*self)
    }

    fn format(&self, ptr: Ptr) -> String {
        self.heap.get(ptr).format(*self)
    }

    #[cfg(feature = "color")]
    fn color_format(&self, ptr: Ptr) -> tcolor::ColorString {
        self.heap.get(ptr).color_format(*self)
    }
}

pub struct ContextMut<'a, 'b> {
    pub heap: &'a mut Heap,
    pub interner: &'b mut Interner,
}

impl ContextMut<'_, '_> {
    pub fn as_ref(&self) -> Context<'_, '_> {
        Context {
            interner: self.interner,
            heap: self.heap,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum This {
    None,
    Ptr(Ptr),
    String(StringPtr),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    Any,
    Exact(usize),
    AtLeast(usize),
}

impl Default for Arity {
    fn default() -> Self {
        Arity::Exact(0)
    }
}

impl Arity {
    pub fn is_compatible(&self, with: usize) -> bool {
        if let Arity::Exact(arity) = self {
            *arity == with
        } else if let Arity::AtLeast(arity) = self {
            *arity <= with
        } else {
            true
        }
    }
}

impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arity::Any => write!(f, "any"),
            Arity::Exact(n) => write!(f, "{}", n),
            Arity::AtLeast(n) => write!(f, "at least {}", n),
        }
    }
}

// https://stackoverflow.com/a/30353928/18270160
pub trait ObjectClone {
    fn clone_object(&self) -> Box<dyn Object>;
}

impl<T> ObjectClone for T
where
    T: 'static + Object + Clone,
{
    fn clone_object(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

pub trait Object: downcast_rs::Downcast + ObjectClone {
    fn trace(&self, heap: &Heap);

    fn type_name(&self, ctx: Context) -> &'static str {
        let _ = ctx;
        "object"
    }

    fn format(&self, ctx: Context) -> String {
        self.type_name(ctx).to_string()
    }

    #[cfg(feature = "color")]
    fn color_format(&self, ctx: Context) -> tcolor::ColorString {
        tcolor::ColorString::new_fg(self.format(ctx), tcolor::Color::Red)
    }

    fn debug_format(&self, ctx: Context) -> String {
        format!("{}(?)", self.type_name(ctx))
    }

    fn call(&self, ctx: Context, values: &[Value]) -> Result<Value, PiccoloError> {
        let _ = values;
        Err(make_error!(CannotCall {
            callee: self.format(ctx),
        }))
    }

    fn get(&self, ctx: Context, this: This, index_value: Value) -> Result<Value, PiccoloError> {
        let _ = this;
        Err(make_error!(CannotGet {
            object: self.type_name(ctx).to_string(),
            index: index_value.format(ctx),
        }))
    }

    fn set(&mut self, ctx: Context, index_value: Value, value: Value) -> Result<(), PiccoloError> {
        let _ = value;
        Err(make_error!(CannotGet {
            object: self.type_name(ctx).to_string(),
            index: index_value.format(ctx),
        }))
    }

    fn eq(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
        let _ = other;
        Err(make_error!(CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }

    fn lt(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
        let _ = other;
        Err(make_error!(CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }

    fn gt(&self, ctx: Context, other: Value) -> Result<bool, PiccoloError> {
        let _ = other;
        Err(make_error!(CannotCompare {
            got: other.type_name(ctx).to_string(),
            exp: self.type_name(ctx).to_string(),
        }))
    }
}

downcast_rs::impl_downcast!(Object);
