use std::fmt::{Display, Formatter};

macro_rules! values {
    ($name:ident => $($variant:ident($kind:ty),)*) => {
        #[derive(Debug, Clone)]
        pub enum $name {
            $($variant($kind),)*
        }

        impl $name {
            pub fn into<T>(self) -> T where $name: Into<T> {
                std::convert::Into::<T>::into(self)
            }
        }

        $(impl Into<$kind> for $name {
            fn into(self) -> $kind {
                match self {
                    $name::$variant(t) => t,
                    _ => panic!("could not cast {:?} to {}", self, stringify!($kind)),
                }
            }
        })*

        $(impl From<$kind> for $name {
            fn from($variant: $kind) -> Self {
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
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
);

