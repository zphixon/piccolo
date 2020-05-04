//! Contains `Object` and its implementors.

use crate::runtime::value::Value;

use downcast_rs::Downcast;

use core::fmt;

/// Trait for Piccolo objects that will be stored in a [`Heap`].
///
/// [`Heap`]: ../memory/struct.Heap.html
pub trait Object: Downcast + fmt::Debug + fmt::Display {
    /// Return the name of the type of the object. Used for runtime type comparison inside
    /// Piccolo via the `type()` builtin function.
    fn type_name(&self) -> &'static str {
        "object"
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn gt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn lt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn eq(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Get the named property from the object. Returns `None` if it doesn't exist.
    fn get(&self, _property: &str) -> Option<Value> {
        None
    }

    /// Sets the named property on the object. Returns `None` if it doesn't exist.
    fn set(&mut self, _property: &str, _value: Value) -> Option<()> {
        None
    }

    /// Attempts to clone the object. Returns `None` if it is not possible.
    fn try_clone(&self) -> Option<Box<dyn Object>> {
        None
    }
}

downcast_rs::impl_downcast!(Object);
