
macro_rules! opcodes {
    ($name:ident => $($op:ident = $num:expr),*,) => {
        #[derive(Debug, PartialEq, Clone, Copy)]
        #[repr(u8)]
        pub enum $name {
            $($op = $num,)*
        }

        impl Into<u8> for $name {
            fn into(self) -> u8 {
                match self {
                    $($name::$op => $num,)*
                }
            }
        }

        impl From<u8> for $name {
            fn from(u: u8) -> $name {
                match u {
                    $($num => $name::$op,)*
                    n => panic!("{} does not correspond to any opcode in {}", n, stringify!($name))
                }
            }
        }
    };
}

opcodes!(Opcode =>
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Subtract = 4,
    Multiply = 5,
    Divide = 6,
);
