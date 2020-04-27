//! Contains types for working with compiled Piccolo bytecode.

use crate::runtime::memory::Heap;
use crate::{PiccoloError, Token, TokenKind, Value};

use core::fmt;

/// Compile-time constant Piccolo values.
///
/// Similar to [`Value`]. `Constant` is also used to return from Piccolo execution.
///
/// [`Value`]: ../value/enum.Value.html
#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub enum Constant {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Nil,
}

impl Constant {
    pub(crate) fn ref_string(&self) -> &str {
        match self {
            Constant::String(v) => v,
            _ => panic!("ref string on non-string"),
        }
    }

    pub(crate) fn from_value(v: Value, heap: &mut Heap) -> Constant {
        match v {
            Value::Object(ptr) => {
                let obj = heap.take(ptr);
                if let Ok(string) = obj.downcast::<String>() {
                    Constant::String(string.to_string())
                } else {
                    panic!("non-string constant");
                }
            }
            Value::Bool(v) => Constant::Bool(v),
            Value::Integer(v) => Constant::Integer(v),
            Value::Double(v) => Constant::Double(v),
            Value::Nil => Constant::Nil,
        }
    }

    pub(crate) fn try_from(token: Token) -> Result<Constant, PiccoloError> {
        Ok(match token.kind {
            TokenKind::String => Constant::String(crate::compiler::escape_string(&token)?),
            TokenKind::Integer(v) => Constant::Integer(v),
            TokenKind::True => Constant::Bool(true),
            TokenKind::False => Constant::Bool(false),
            TokenKind::Double(v) => Constant::Double(v),
            TokenKind::Nil => Constant::Nil,
            _ => panic!("cannot create value from token {:?}", token),
        })
    }

    // TODO: check VM's string table
    pub(crate) fn into_value(self, heap: &mut Heap) -> Value {
        trace!("into_value");
        match self {
            Constant::String(v) => {
                let ptr = heap.alloc(Box::new(v));
                Value::Object(ptr)
            }
            Constant::Integer(v) => Value::Integer(v),
            Constant::Bool(v) => Value::Bool(v),
            Constant::Double(v) => Value::Double(v),
            Constant::Nil => Value::Nil,
        }
    }

    /// Convert a `Constant` into its base type.
    pub fn into<T>(self) -> T
    where
        Constant: Into<T>,
    {
        core::convert::Into::<T>::into(self)
    }

    pub fn is_string(&self) -> bool {
        match self {
            Constant::String(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::String(v) => write!(f, "{}", v),
            Constant::Bool(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
            Constant::Nil => write!(f, "nil"),
        }
    }
}

impl Into<bool> for Constant {
    fn into(self) -> bool {
        match self {
            Constant::Bool(v) => v,
            _ => panic!("could not cast to bool"),
        }
    }
}

impl Into<i64> for Constant {
    fn into(self) -> i64 {
        match self {
            Constant::Integer(v) => v,
            _ => panic!("could not cast to i64"),
        }
    }
}

impl Into<f64> for Constant {
    fn into(self) -> f64 {
        match self {
            Constant::Double(v) => v,
            _ => panic!("could not cast to f64"),
        }
    }
}

// TODO: don't let chunk be cloned
/// Stores a piece of compiled Piccolo bytecode.
#[derive(Default, Debug, Clone)]
pub struct Chunk {
    pub(crate) data: Vec<u8>,
    pub(crate) lines: Vec<usize>,
    pub(crate) constants: Vec<Constant>,
}

impl Chunk {
    pub(crate) fn write_u8<T: Into<u8>>(&mut self, byte: T, line: usize) {
        let byte = byte.into();
        trace!("write u8 {}", byte);

        self.data.push(byte);
        self.add_to_line(line);
    }

    pub(crate) fn write_u16<T: Into<u16>>(&mut self, bytes: T, line: usize) {
        let (low, high) = crate::decode_bytes(bytes.into());
        self.write_u8(low, line);
        self.write_u8(high, line);
    }

    pub(crate) fn write_arg_u16<T: Into<u8>>(&mut self, op: T, arg: u16, line: usize) {
        self.write_u8(op, line);
        self.write_u16(arg, line);
    }

    // allows for duplicate constants, non-duplicates are checked in the compiler
    pub(crate) fn make_constant(&mut self, value: Constant) -> u16 {
        trace!("make constant {:?}", value);

        self.constants.push(value);
        let idx = self.constants.len() - 1;
        if idx > core::u16::MAX as usize {
            panic!("too many constants (>65k, fix your program)");
        } else {
            idx as u16
        }
    }

    pub(crate) fn read_short(&self, offset: usize) -> u16 {
        trace!("read short {:x}", offset);

        let low = self.data[offset];
        let high = self.data[offset + 1];
        crate::encode_bytes(low, high)
    }

    // get a line number from a byte offset using run-length encoding
    pub(crate) fn get_line_from_index(&self, idx: usize) -> usize {
        let mut total_ops = 0;
        for (offset_line, num_ops) in self.lines.iter().enumerate() {
            total_ops += *num_ops;
            if total_ops > idx {
                return offset_line + 1;
            }
        }
        panic!("no line for idx {} {:?} {:?}", idx, self.lines, self.data);
    }

    // add one opcode to a line
    fn add_to_line(&mut self, line: usize) {
        while line > self.lines.len() {
            self.lines.push(0);
        }
        self.lines[line - 1] += 1;
    }

    pub fn disassemble(&self, name: &str) -> String {
        trace!("disassemble");
        use crate::runtime::op::Opcode;

        let mut s = format!(" -- {} --\n", name);
        s.push_str(&format!(" ++ constants\n"));
        for (idx, constant) in self.constants.iter().enumerate() {
            s.push_str(&format!("{:04x} {:?}\n", idx, constant));
        }
        s.push_str(&format!(" ++ code\n"));

        let mut prev_line = 0;
        let mut offset = 0;
        while offset < self.data.len() {
            let line = self.get_line_from_index(offset);

            let op = self.data[offset].into();

            s.push_str(&format!(
                "{:04x} {:02x} {} {:15}",
                offset,
                op as u8,
                if line == prev_line {
                    String::from("   |")
                } else {
                    format!("{:>4}", line)
                },
                format!("{:?}", op)
            ));

            offset = match op {
                Opcode::Pop | Opcode::Return => offset + 1,

                Opcode::Constant => {
                    let idx = self.read_short(offset + 1);
                    s.push_str(&format!(" @{:04x} {:?}", idx, self.constants[idx as usize]));
                    offset + 3
                }
                Opcode::Nil | Opcode::True | Opcode::False => offset + 1,

                Opcode::Negate
                | Opcode::Not
                | Opcode::Add
                | Opcode::Subtract
                | Opcode::Multiply
                | Opcode::Divide
                | Opcode::Modulo => offset + 1,

                Opcode::Equal
                | Opcode::Greater
                | Opcode::Less
                | Opcode::GreaterEqual
                | Opcode::LessEqual => offset + 1,

                Opcode::GetLocal | Opcode::SetLocal => {
                    let idx = self.read_short(offset + 1);
                    s.push_str(&format!(" ${}", idx));
                    offset + 3
                }
                Opcode::GetGlobal | Opcode::SetGlobal | Opcode::DeclareGlobal => {
                    let idx = self.read_short(offset + 1);
                    s.push_str(&format!(" g{:04x} {:?}", idx, self.constants[idx as usize]));
                    offset + 3
                }

                Opcode::Assert => offset + 1,
            };
            s.push('\n');

            prev_line = line;
        }

        s
    }

    pub fn disassemble_instruction(&self, offset: usize) -> String {
        trace!("disassemble_instruction");
        use crate::runtime::op::Opcode;

        let line = self.get_line_from_index(offset);

        let op = self.data[offset].into();

        let mut s = format!(
            "{:04x} {:02x} line {:<4} {:15}",
            offset,
            op as u8,
            line,
            format!("{:?}", op)
        );

        match op {
            Opcode::Constant => {
                let idx = self.read_short(offset + 1);
                s.push_str(&format!(" @{:04x} {:?}", idx, self.constants[idx as usize]));
            }
            Opcode::GetLocal | Opcode::SetLocal => {
                let idx = self.read_short(offset + 1);
                s.push_str(&format!(" ${}", idx));
            }
            Opcode::GetGlobal | Opcode::SetGlobal | Opcode::DeclareGlobal => {
                let idx = self.read_short(offset + 1);
                s.push_str(&format!(" g{:04x} {:?}", idx, self.constants[idx as usize]));
            }
            _ => {}
        }

        s
    }
}

#[cfg(test)]
mod test {
    use crate::runtime::op::Opcode;

    use super::Chunk;

    #[test]
    fn get_line_from_index() {
        let mut c = Chunk::default();
        c.write_u8(Opcode::Return, 1); // 0
        c.write_u8(Opcode::Return, 1); // 1
        c.write_u8(Opcode::Return, 1); // 2
        c.write_u8(Opcode::Return, 1); // 3
        c.write_u8(Opcode::Return, 1); // 4
        c.write_u8(Opcode::Return, 1); // 5
        c.write_u8(Opcode::Return, 2); // 6
        c.write_u8(Opcode::Return, 2); // 7
        c.write_u8(Opcode::Return, 2); // 8
        c.write_u8(Opcode::Return, 2); // 9
        c.write_u8(Opcode::Return, 2); // 10
        c.write_u8(Opcode::Return, 3); // 11
        c.write_u8(Opcode::Return, 3); // 12
        c.write_u8(Opcode::Return, 3); // 13
        c.write_u8(Opcode::Return, 3); // 14
        c.write_u8(Opcode::Return, 4); // 15
        c.write_u8(Opcode::Return, 4); // 16
        c.write_u8(Opcode::Return, 4); // 17
        c.write_u8(Opcode::Return, 4); // 18
        c.write_u8(Opcode::Return, 5); // 19

        assert_eq!(c.get_line_from_index(0), 1);
        assert_eq!(c.get_line_from_index(5), 1);
        assert_eq!(c.get_line_from_index(6), 2);
        assert_eq!(c.get_line_from_index(10), 2);
        assert_eq!(c.get_line_from_index(11), 3);
        assert_eq!(c.get_line_from_index(14), 3);
    }
}
