
use crate::op::Opcode;
use crate::value::Value;

// TODO: change lines to a reasonable number type
#[derive(Default)]
pub struct Chunk {
    pub(crate) data: Vec<u8>,
    pub(crate) lines: Vec<usize>,
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    pub fn write<T: Into<u8>>(&mut self, byte: T, line: usize) {
        self.data.push(byte.into());

        let len = self.lines.len();
        if line == len + 1 {
            self.lines.push(1);
        } else if line > len + 1 {
            unreachable!("The line number for the instruction is too large!");
        } else {
            self.lines[line - 1] += 1;
        }
    }

    pub fn constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disassemble(&self, name: &str) {
        println!(" -- {} --", name);

        let mut prev_line = 0;
        let mut offset = 0;
        while offset < self.data.len() {
            let line = self.get_line_from_index(offset);

            let op = self.data[offset].into();

            print!("{:04} {} {:?}", offset, if line == prev_line { String::from("   |") } else { format!("{:>4}", line) }, op);
            offset = match op {
                Opcode::Return => { offset + 1 },
                Opcode::Constant => {
                    print!("#{:04} {:?}", self.data[offset+1], self.constants[self.data[offset+1] as usize]);
                    offset + 2
                },
                Opcode::Negate => { offset + 1 },
                Opcode::Add => { offset + 1 },
                Opcode::Subtract => { offset + 1 },
                Opcode::Multiply => { offset + 1 },
                Opcode::Divide => { offset + 1 },
            };
            println!();

            prev_line = line;
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) {
        let line = self.get_line_from_index(offset);

        let op = self.data[offset].into();

        print!("{:04} {:<6} {:?}", offset, line, op);
        match op {
            Opcode::Constant => {
                print!("#{:04} {:?}", self.data[offset+1], self.constants[self.data[offset+1] as usize]);
            },
            _ => {
                print!(" <>");
            }
        };
        println!();
    }

    pub fn get_line_from_index(&self, idx: usize) -> usize {
        let mut sum = 0;
        for (k, v) in self.lines.iter().enumerate() {
            sum += *v;
            if sum > idx {
                return k + 1;
            }
        }

        panic!("no line number for token at index {}", idx);
    }
}
