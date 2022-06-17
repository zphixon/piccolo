//! Types for working with compiled Piccolo bytecode.

use {
    crate::{
        compiler::SourcePos,
        runtime::{op::Opcode, value::Constant},
    },
    serde::{Deserialize, Serialize},
};

#[derive(Debug, Serialize, Deserialize)]
pub struct Module {
    chunks: Vec<Chunk>,
    constants: Vec<Constant>,
}

impl Module {
    pub(crate) fn new() -> Self {
        Self {
            chunks: vec![Chunk::default()],
            constants: vec![],
        }
    }

    // allows for duplicate constants, non-duplicates are checked in the compiler
    pub(crate) fn make_constant(&mut self, value: Constant) -> u16 {
        trace!("make constant {:?}", value);

        self.constants.push(value);
        let index = self.constants.len() - 1;
        if index > u16::MAX as usize {
            panic!("too many constants (>65k, fix your program)");
        } else {
            index as u16
        }
    }

    pub(crate) fn get_constant(&self, index: u16) -> &Constant {
        self.constants
            .get(index as usize)
            .unwrap_or_else(|| panic!("{} out of constant bounds", index))
    }

    pub(crate) fn chunk(&self, i: usize) -> &Chunk {
        &self.chunks[i]
    }

    pub(crate) fn chunk_mut(&mut self, i: usize) -> &mut Chunk {
        &mut self.chunks[i]
    }

    pub(crate) fn add_chunk(&mut self) -> usize {
        self.chunks.push(Chunk::default());
        self.chunks.len() - 1
    }

    pub(crate) fn index_of(&self, chunk: &Chunk) -> usize {
        self.chunks
            .iter()
            .position(|c| c.data == chunk.data)
            .unwrap()
    }
}

/// Stores a piece of compiled Piccolo bytecode.
#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Chunk {
    pub(crate) data: Vec<u8>,
    // each SourcePos represents one instruction
    pub(crate) lines: Vec<Vec<SourcePos>>,
}

impl Chunk {
    pub(crate) fn len(&self) -> usize {
        self.data.len()
    }

    pub(crate) fn write_u8<T: Into<u8>>(&mut self, byte: T, pos: SourcePos) {
        let byte = byte.into();
        trace!("write u8 {:04x}={:02x}", self.data.len(), byte);

        self.data.push(byte);
        self.add_to_line(pos);
    }

    pub(crate) fn write_u16<T: Into<u16>>(&mut self, bytes: T, pos: SourcePos) {
        let [low, high] = bytes.into().to_le_bytes();
        self.write_u8(low, pos);
        self.write_u8(high, pos);
    }

    pub(crate) fn write_arg_u16<T: Into<u8>>(&mut self, op: T, arg: u16, pos: SourcePos) {
        self.write_u8(op, pos);
        self.write_u16(arg, pos);
    }

    pub(crate) fn start_jump(&mut self, op: Opcode, pos: SourcePos) -> usize {
        trace!("write jump to index {:x}", self.data.len());
        self.write_u8(op, pos);
        self.write_u8(Opcode::Assert, pos);
        self.write_u8(Opcode::False, pos);
        self.data.len() - 2
    }

    pub(crate) fn patch_jump(&mut self, offset: usize) {
        let jump = self.data.len() - offset - 2;
        if jump > u16::MAX as usize {
            panic!("cannot jump further than u16::MAX instructions");
        } else {
            let [low, high] = (jump as u16).to_le_bytes();
            trace!("patch jump at index {:x}={:04x}", jump, offset);
            self.data[offset] = low;
            self.data[offset + 1] = high;
        }
    }

    pub(crate) fn write_jump_back(&mut self, offset: usize, pos: SourcePos) {
        // we haven't written the JumpBack instruction yet, so we need to add it
        // in order to calculate the actual offset when we write the jump instruction
        let offset = self.data.len() - offset + 3;
        self.write_arg_u16(Opcode::JumpBack, offset as u16, pos);
    }

    pub(crate) fn read_short(&self, offset: usize) -> u16 {
        trace!("read short {:x}", offset);

        let low = self.data[offset];
        let high = self.data[offset + 1];
        u16::from_le_bytes([low, high])
    }

    pub(crate) fn get_pos_from_index(&self, index: usize) -> SourcePos {
        let mut total_ops = 0;
        for (offset_line, line) in self.lines.iter().enumerate() {
            total_ops += line.len();
            if total_ops > index {
                let pos = line[total_ops - index - 1];
                debug_assert_eq!(pos.line, offset_line + 1);
                return pos;
            }
        }
        panic!(
            "no line for index {} {:?} {:?}",
            index, self.lines, self.data
        );
    }

    fn add_to_line(&mut self, pos: SourcePos) {
        while self.lines.len() < pos.line {
            self.lines.push(Vec::with_capacity(1));
        }
        self.lines[pos.line - 1].push(pos);
    }
}

pub fn disassemble(module: &Module, name: &str) -> String {
    trace!("disassemble");

    let mut s = format!(" -- {} --\n", name);
    s.push_str(" ++ constants\n");
    for (index, constant) in module.constants.iter().enumerate() {
        s.push_str(&format!("{:04x} {:?}\n", index, constant));
    }

    for (i, chunk) in module.chunks.iter().enumerate() {
        s.push_str(&format!(" ++ chunk {}\n", i));
        let mut offset = 0;
        while offset < chunk.data.len() {
            s.push_str(&disassemble_instruction(module, chunk, offset));
            s.push('\n');
            offset += super::op::op_len(chunk.data[offset].into());
        }
    }

    s
}

pub fn disassemble_instruction(module: &Module, chunk: &Chunk, offset: usize) -> String {
    let op = chunk.data[offset].into();
    let len = super::op::op_len(op);
    let bytes = format!(
        "{first:02x}{others}",
        first = op as u8,
        others = if len > 1 {
            let mut s = String::new();
            for i in 1..len {
                s.push_str(&format!(" {:02x}", chunk.data[offset + i]));
            }
            s
        } else {
            String::from("")
        }
    );

    let line = chunk.get_pos_from_index(offset);
    let line_str = format!("{:04x} {:>4}", offset, line);

    let op_str = format!("{:15}", format!("{:?}", op));

    let arg = match op {
        Opcode::Constant => {
            let index = chunk.read_short(offset + 1);
            format!("@{:04x} ({:?})", index, module.constants[index as usize])
        }
        Opcode::GetLocal | Opcode::SetLocal => {
            let index = chunk.read_short(offset + 1);
            format!("${}", index)
        }
        Opcode::GetGlobal | Opcode::SetGlobal | Opcode::DeclareGlobal => {
            let index = chunk.read_short(offset + 1);
            format!("g{:04x} ({:?})", index, module.constants[index as usize])
        }
        Opcode::JumpForward | Opcode::JumpFalse | Opcode::JumpTrue => {
            let index = chunk.read_short(offset + 1);
            format!("+{:04x}", index)
        }
        Opcode::JumpBack => {
            let index = chunk.read_short(offset + 1);
            format!("-{:04x}", index)
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
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn get_pos_from_index() {
        use crate::runtime::op::Opcode;

        let mut c = Chunk::default();
        c.write_u8(Opcode::Return, SourcePos::with_line(1)); // 0
        c.write_u8(Opcode::Return, SourcePos::with_line(1)); // 1
        c.write_u8(Opcode::Return, SourcePos::with_line(1)); // 2
        c.write_u8(Opcode::Return, SourcePos::with_line(1)); // 3
        c.write_u8(Opcode::Return, SourcePos::with_line(1)); // 4
        c.write_u8(Opcode::Return, SourcePos::with_line(1)); // 5
        c.write_u8(Opcode::Return, SourcePos::with_line(2)); // 6
        c.write_u8(Opcode::Return, SourcePos::with_line(2)); // 7
        c.write_u8(Opcode::Return, SourcePos::with_line(2)); // 8
        c.write_u8(Opcode::Return, SourcePos::with_line(2)); // 9
        c.write_u8(Opcode::Return, SourcePos::with_line(2)); // 10
        c.write_u8(Opcode::Return, SourcePos::with_line(3)); // 11
        c.write_u8(Opcode::Return, SourcePos::with_line(3)); // 12
        c.write_u8(Opcode::Return, SourcePos::with_line(3)); // 13
        c.write_u8(Opcode::Return, SourcePos::with_line(3)); // 14
        c.write_u8(Opcode::Return, SourcePos::with_line(4)); // 15
        c.write_u8(Opcode::Return, SourcePos::with_line(4)); // 16
        c.write_u8(Opcode::Return, SourcePos::with_line(4)); // 17
        c.write_u8(Opcode::Return, SourcePos::with_line(4)); // 18
        c.write_u8(Opcode::Return, SourcePos::with_line(5)); // 19

        assert_eq!(c.get_pos_from_index(0).line, 1);
        assert_eq!(c.get_pos_from_index(5).line, 1);
        assert_eq!(c.get_pos_from_index(6).line, 2);
        assert_eq!(c.get_pos_from_index(10).line, 2);
        assert_eq!(c.get_pos_from_index(11).line, 3);
        assert_eq!(c.get_pos_from_index(14).line, 3);
    }
}
