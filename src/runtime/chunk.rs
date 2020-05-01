//! Contains types for working with compiled Piccolo bytecode.

use crate::Constant;

use super::op::Opcode;

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
        trace!("write u8 {:04x}={:02x}", self.data.len(), byte);

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

    pub(crate) fn write_jump(&mut self, op: Opcode, line: usize) -> usize {
        trace!("write jump to idx {:x}", self.data.len());
        self.write_u8(op, line);
        self.write_u8(Opcode::Assert, line);
        self.write_u8(Opcode::False, line);
        self.data.len() - 2
    }

    pub(crate) fn patch_jump(&mut self, offset: usize) {
        let jump = self.data.len() - offset - 2;
        if jump > u16::MAX as usize {
            panic!("cannot jump further than u16::MAX instructions");
        } else {
            let (low, high) = crate::decode_bytes(jump as u16);
            trace!("patch jump at idx {:x}={:04x}", jump, offset);
            self.data[offset] = low;
            self.data[offset + 1] = high;
        }
    }

    // allows for duplicate constants, non-duplicates are checked in the compiler
    pub(crate) fn make_constant(&mut self, value: Constant) -> u16 {
        trace!("make constant {:?}", value);

        self.constants.push(value);
        let idx = self.constants.len() - 1;
        if idx > u16::MAX as usize {
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

        let mut s = format!(" -- {} --\n", name);
        s.push_str(" ++ constants\n");
        for (idx, constant) in self.constants.iter().enumerate() {
            s.push_str(&format!("{:04x} {:?}\n", idx, constant));
        }
        s.push_str(" ++ code\n");

        let mut offset = 0;
        while offset < self.data.len() {
            s.push_str(&self.disassemble_instruction(offset));
            s.push('\n');
            offset += super::op::op_len(self.data[offset].into());
        }

        s
    }

    pub fn disassemble_instruction(&self, offset: usize) -> String {
        let op = self.data[offset].into();
        let len = super::op::op_len(op);
        let bytes = format!(
            "{first:02x}{others}",
            first = op as u8,
            others = if len > 1 {
                let mut s = String::new();
                for i in 1..len {
                    s.push_str(&format!(" {:02x}", self.data[offset + i]));
                }
                s
            } else {
                String::from("")
            }
        );

        let line = self.get_line_from_index(offset);
        let line_str = format!("{:04x} {:>4}", offset, line);

        let op_str = format!("{:15}", format!("{:?}", op));

        let arg = match op {
            Opcode::Constant => {
                let idx = self.read_short(offset + 1);
                format!("@{:04x} ({:?})", idx, self.constants[idx as usize])
            }
            Opcode::GetLocal | Opcode::SetLocal => {
                let idx = self.read_short(offset + 1);
                format!("${}", idx)
            }
            Opcode::GetGlobal | Opcode::SetGlobal | Opcode::DeclareGlobal => {
                let idx = self.read_short(offset + 1);
                format!("g{:04x} ({:?})", idx, self.constants[idx as usize])
            }
            Opcode::Jump | Opcode::JumpFalse | Opcode::JumpTrue => {
                let idx = self.read_short(offset + 1);
                format!("+{:04x}", idx)
            }
            _ => String::new(),
        };

        format!(
            "{bytes:9} {line_str} | {op_str} {arg}",
            bytes = bytes,
            line_str = line_str,
            op_str = op_str,
            arg = arg
        )
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
