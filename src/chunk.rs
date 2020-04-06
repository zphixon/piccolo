use crate::value::Value;

use slotmap::DenseSlotMap;

// TODO: change lines to a reasonable number type
/// Stores a piece of compiled Piccolo bytecode.
#[derive(Default, Debug)]
pub struct Chunk {
    pub(crate) data: Vec<u8>,
    pub(crate) lines: Vec<usize>,
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    pub(crate) fn write<T: Into<u8>>(&mut self, byte: T, line: usize) {
        self.data.push(byte.into());
        self.lines.push(line);
    }

    pub(crate) fn make_constant(&mut self, value: Value) -> u16 {
        self.constants.push(value);
        let idx = self.constants.len() - 1;
        if idx > std::u16::MAX as usize {
            panic!("too many constants (>65k, fix your program)");
        } else {
            idx as u16
        }
    }

    // TODO: retain to a hashmap in compiler
    // cause get and has_constant are slow
    pub(crate) fn has_constant(&self, value: &Value) -> bool {
        self.constants
            .iter()
            .any(|v| {
                let r = v.eq(value, &DenseSlotMap::with_capacity(0));
                r.is_some() && r.unwrap()
            })
    }

    pub(crate) fn get_constant(&self, name: &str) -> u16 {
        for (idx, value) in self.constants.iter().enumerate() {
            if value.is_string() && value.ref_string() == name {
                return idx as u16;
            }
        }
        panic!("constant does not exist: {}", name);
    }

    pub(crate) fn get_line_from_index(&self, idx: usize) -> usize {
        self.lines[idx]
    }

    #[cfg(feature = "pc-debug")]
    pub fn disassemble(&self, name: &str) {
        use crate::op::Opcode;

        println!(" -- {} --", name);
        println!(" ++ constants");
        for (idx, constant) in self.constants.iter().enumerate() {
            println!("{:04x} {:?}", idx, constant);
        }
        println!(" ++ code");

        let mut prev_line = 0;
        let mut offset = 0;
        while offset < self.data.len() {
            let line = self.get_line_from_index(offset);

            let op = self.data[offset].into();

            print!(
                "{:04x} {} {:?}",
                offset,
                if line == prev_line {
                    String::from("   |")
                } else {
                    format!("{:>4}", line)
                },
                op
            );

            offset = match op {
                Opcode::Return => offset + 1,
                Opcode::Constant => {
                    let low = self.data[offset + 1];
                    let high = self.data[offset + 2];
                    let idx = crate::encode_bytes(low, high);
                    print!("#{:04x} {:?}", idx, self.constants[idx as usize]);
                    offset + 3
                }
                Opcode::Negate => offset + 1,
                Opcode::Add => offset + 1,
                Opcode::Subtract => offset + 1,
                Opcode::Multiply => offset + 1,
                Opcode::Divide => offset + 1,
                Opcode::Nil => offset + 1,
                Opcode::True => offset + 1,
                Opcode::False => offset + 1,
                Opcode::Not => offset + 1,
                Opcode::Equal => offset + 1,
                Opcode::Greater => offset + 1,
                Opcode::Less => offset + 1,
                Opcode::Pop => offset + 1,
                Opcode::DefineGlobal => {
                    let low = self.data[offset + 1];
                    let high = self.data[offset + 2];
                    let idx = crate::encode_bytes(low, high);
                    print!("#{:04x} {:?}", idx, self.constants[idx as usize]);
                    offset + 3
                }
                Opcode::GetGlobal => {
                    let low = self.data[offset + 1];
                    let high = self.data[offset + 2];
                    let idx = crate::encode_bytes(low, high);
                    print!("#{:04x} {:?}", idx, self.constants[idx as usize]);
                    offset + 3
                }
                Opcode::AssignGlobal => {
                    let low = self.data[offset + 1];
                    let high = self.data[offset + 2];
                    let idx = crate::encode_bytes(low, high);
                    print!("#{:04x} {:?}", idx, self.constants[idx as usize]);
                    offset + 3
                }
            };
            println!();

            prev_line = line;
        }
    }

    #[cfg(feature = "pc-debug")]
    pub(crate) fn disassemble_instruction(&self, offset: usize) {
        use crate::op::Opcode;

        let line = self.get_line_from_index(offset);

        let op = self.data[offset].into();

        print!("{:04} line {:>6} {:?}", offset, line, op);
        if let Opcode::Constant = op {
            print!(
                "#{:04} {:?}",
                self.data[offset + 1],
                self.constants[self.data[offset + 1] as usize]
            );
        }
        println!();
    }
}
