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
    Return          = 0x00,
    Constant        = 0x01,
    Nil             = 0x02,
    True            = 0x03,
    False           = 0x04,
    Negate          = 0x05,
    Add             = 0x06,
    Subtract        = 0x07,
    Multiply        = 0x08,
    Divide          = 0x09,
    Not             = 0x0a,
    Equal           = 0x0b,
    Greater         = 0x0c,
    Less            = 0x0d,
    GreaterEqual    = 0x0e,
    LessEqual       = 0x0f,
    Pop             = 0x10,
    DeclareGlobal   = 0x11,
    GetGlobal       = 0x12,
    AssignGlobal    = 0x13,
    Assert          = 0x14,
);
