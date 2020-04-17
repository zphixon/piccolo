macro_rules! opcodes {
    ($name:ident => $($op:ident = $num:expr,)*) => {
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
    Nil = 2,
    True = 3,
    False = 4,
    Negate = 5,
    Add = 6,
    Subtract = 7,
    Multiply = 8,
    Divide = 9,
    Not = 10,
    Equal = 11,
    Greater = 12,
    Less = 13,
    GreaterEqual = 14,
    LessEqual = 15,
    Pop = 16,
    DeclareGlobal = 17,
    GetGlobal = 18,
    AssignGlobal = 19,
    Assert = 20,
);
