//! Types for working with compiled Piccolo bytecode.

use {
    crate::{
        compiler::SourcePos,
        runtime::{op::Opcode, value::Constant},
        trace,
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
        self.chunks.iter().position(|c| c.ops == chunk.ops).unwrap()
    }
}

/// Stores a piece of compiled Piccolo bytecode.
#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Chunk {
    pub(crate) ops: Vec<Opcode>,
    // each SourcePos represents one instruction
    pub(crate) lines: Vec<Vec<SourcePos>>,
}

impl Chunk {
    pub(crate) fn len(&self) -> usize {
        self.ops.len()
    }

    pub(crate) fn write(&mut self, op: Opcode, pos: SourcePos) {
        trace!("write {:04x}={op:?}", self.ops.len());

        self.ops.push(op);
        self.add_to_line(pos);
    }

    pub(crate) fn start_jump(&mut self, op: Opcode, pos: SourcePos) -> usize {
        trace!("write jump to index {:x}", self.ops.len());
        self.write(op, pos);
        self.ops.len() - 1
    }

    pub(crate) fn patch_jump(&mut self, offset: usize) {
        let jump = self.ops.len() - offset;
        if jump > u16::MAX as usize {
            panic!("cannot jump further than u16::MAX instructions");
        } else {
            trace!(
                "patch jump at index {offset:x}={jump:04x} ({:?})",
                self.ops[offset]
            );
            let jump = jump as u16;
            self.ops[offset] = match self.ops[offset] {
                Opcode::JumpForward(_) => Opcode::JumpForward(jump),
                Opcode::JumpBack(_) => Opcode::JumpBack(jump),
                Opcode::JumpTrue(_) => Opcode::JumpTrue(jump),
                Opcode::JumpFalse(_) => Opcode::JumpFalse(jump),
                _ => unreachable!(
                    "non-jump {jump} at {offset} = {:?}\n{:#?}",
                    self.ops[offset], self.ops
                ),
            };
        }
    }

    pub(crate) fn write_jump_back(&mut self, offset: usize, pos: SourcePos) {
        // we haven't written the JumpBack instruction yet, so we need to add it
        // in order to calculate the actual offset when we write the jump instruction
        let offset = self.ops.len() - offset;
        self.write(Opcode::JumpBack(offset as u16), pos);
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
            index, self.lines, self.ops
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
        while offset < chunk.ops.len() {
            s.push_str(&disassemble_instruction(module, chunk, offset));
            s.push('\n');
            offset += 1;
        }
    }

    s
}

pub fn disassemble_instruction(module: &Module, chunk: &Chunk, offset: usize) -> String {
    let op = chunk.ops[offset];

    let arg = match op {
        Opcode::Constant(index) => {
            format!("@{:04x} ({:?})", index, module.constants[index as usize])
        }
        Opcode::GetLocal(index) | Opcode::SetLocal(index) => {
            format!("${}", index)
        }
        Opcode::GetGlobal(index) | Opcode::SetGlobal(index) | Opcode::DeclareGlobal(index) => {
            format!("g{:04x} ({:?})", index, module.constants[index as usize])
        }
        Opcode::JumpForward(jump) | Opcode::JumpFalse(jump) | Opcode::JumpTrue(jump) => {
            format!("+{:04x} -> {:04x}", jump, offset + jump as usize)
        }
        Opcode::JumpBack(jump) => {
            format!("-{:04x} -> {:04x}", jump, offset - jump as usize)
        }
        _ => String::new(),
    };

    format!(
        "{:<6} {offset:04x} {:20} {arg}",
        format!("{}", chunk.get_pos_from_index(offset)),
        format!("{op:?}")
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn get_pos_from_index() {
        use crate::runtime::op::Opcode;

        let mut c = Chunk::default();
        c.write(Opcode::Return, SourcePos { line: 1, col: 1 }); // 0
        c.write(Opcode::Return, SourcePos { line: 1, col: 1 }); // 1
        c.write(Opcode::Return, SourcePos { line: 1, col: 1 }); // 2
        c.write(Opcode::Return, SourcePos { line: 1, col: 1 }); // 3
        c.write(Opcode::Return, SourcePos { line: 1, col: 1 }); // 4
        c.write(Opcode::Return, SourcePos { line: 1, col: 1 }); // 5
        c.write(Opcode::Return, SourcePos { line: 2, col: 1 }); // 6
        c.write(Opcode::Return, SourcePos { line: 2, col: 1 }); // 7
        c.write(Opcode::Return, SourcePos { line: 2, col: 1 }); // 8
        c.write(Opcode::Return, SourcePos { line: 2, col: 1 }); // 9
        c.write(Opcode::Return, SourcePos { line: 2, col: 1 }); // 10
        c.write(Opcode::Return, SourcePos { line: 3, col: 1 }); // 11
        c.write(Opcode::Return, SourcePos { line: 3, col: 1 }); // 12
        c.write(Opcode::Return, SourcePos { line: 3, col: 1 }); // 13
        c.write(Opcode::Return, SourcePos { line: 3, col: 1 }); // 14
        c.write(Opcode::Return, SourcePos { line: 4, col: 1 }); // 15
        c.write(Opcode::Return, SourcePos { line: 4, col: 1 }); // 16
        c.write(Opcode::Return, SourcePos { line: 4, col: 1 }); // 17
        c.write(Opcode::Return, SourcePos { line: 4, col: 1 }); // 18
        c.write(Opcode::Return, SourcePos { line: 5, col: 1 }); // 19

        assert_eq!(c.get_pos_from_index(0).line, 1);
        assert_eq!(c.get_pos_from_index(5).line, 1);
        assert_eq!(c.get_pos_from_index(6).line, 2);
        assert_eq!(c.get_pos_from_index(10).line, 2);
        assert_eq!(c.get_pos_from_index(11).line, 3);
        assert_eq!(c.get_pos_from_index(14).line, 3);
    }
}
