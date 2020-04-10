use crate::value::Value;

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
        self.add_to_line(line);
    }

    // allows for duplicate constants, non-duplicates are checked in the compiler
    pub(crate) fn make_constant(&mut self, value: Value) -> u16 {
        self.constants.push(value);
        let idx = self.constants.len() - 1;
        if idx > std::u16::MAX as usize {
            panic!("too many constants (>65k, fix your program)");
        } else {
            idx as u16
        }
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
        panic!("no line for idx {}", idx);
    }

    // add one opcode to a line
    fn add_to_line(&mut self, line: usize) {
        while line - 1 >= self.lines.len() {
            self.lines.push(0);
        }
        self.lines[line - 1] += 1;
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
                Opcode::Assert => offset + 1,
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
