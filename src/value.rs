use std::fmt::{Display, Formatter};
use core::fmt;

// TODO: PartialOrd
macro_rules! values {
    ($name:ident => $({$typename:ident, $is_ty:ident, $variant:ident($ty:ty)},)*) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum $name {
            $($variant($ty),)*
        }

        impl $name {
            pub fn into<T>(self) -> T where $name: Into<T> {
                std::convert::Into::<T>::into(self)
            }

            $(pub fn $is_ty(&self) -> bool {
                match self {
                    $name::$variant(_) => true,
                    _ => false
                }
            })*

            pub fn type_name(&self) -> &'static str {
                match self {
                    $($name::$variant(_) => stringify!($typename),)*
                }
            }
        }

        $(impl Into<$ty> for $name {
            fn into(self) -> $ty {
                match self {
                    $name::$variant(t) => t,
                    _ => panic!("could not cast {:?} to {}", self, stringify!($ty)),
                }
            }
        })*

        $(impl From<$ty> for $name {
            fn from($variant: $ty) -> Self {
                $name::$variant($variant)
            }
        })*

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $($name::$variant($variant) => write!(f, "{}", $variant)),*,
                }
            }
        }
    };
}

values!(Value =>
    {string,  is_string,  String(String)},
    {bool,    is_bool,    Bool(bool)},
    {integer, is_integer, Integer(i64)},
    {double,  is_double,  Double(f64)},
    {nil,     is_nil,     Nil(Nil)},
);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Nil;
impl Display for Nil {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "nil")
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => true,
        }
    }
}

