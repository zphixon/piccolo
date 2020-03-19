use crate::chunk::Chunk;
use crate::error::InterpretError;
use crate::value::Value;
use crate::op::Opcode;

pub struct Machine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl Machine {
    pub fn new(mut chunk: Chunk) -> Self {
        Machine {
            chunk,
            ip: 0,
            stack: vec![]
        }
    }

    pub fn interpret(&mut self) -> anyhow::Result<()> {
        loop {
            // if debug
            #[cfg(feature = "pc-debug")]
            {
                print!("┌─ {:?}\n└─ ", self.stack);
                self.chunk.disassemble_instruction(self.ip);
            }

            let inst = self.chunk.data[self.ip];
            self.ip += 1;

            let op = inst.into();
            match op {
                Opcode::Return => {
                    println!("{}", self.stack.pop().expect(&format!("empty stack: {}", self.chunk.get_line_from_index(self.ip-1))));
                    return Ok(());
                },
                Opcode::Constant => {
                    // TODO: remove unnecessary clone
                    let c = self.chunk.constants[self.chunk.data[self.ip] as usize].clone();
                    self.ip += 1;

                    self.stack.push(c);
                },
                Opcode::Negate => {
                    let v = self.stack.pop().unwrap();
                    self.stack.push(Value(-v.0));
                },
                Opcode::Add => {
                    let rhs = self.stack.pop().unwrap().0;
                    let lhs = self.stack.pop().unwrap().0;
                    self.stack.push(Value(lhs + rhs));
                },
                Opcode::Subtract => {
                    let rhs = self.stack.pop().unwrap().0;
                    let lhs = self.stack.pop().unwrap().0;
                    self.stack.push(Value(lhs - rhs));
                },
                Opcode::Multiply => {
                    let rhs = self.stack.pop().unwrap().0;
                    let lhs = self.stack.pop().unwrap().0;
                    self.stack.push(Value(lhs * rhs));
                },
                Opcode::Divide => {
                    let rhs = self.stack.pop().unwrap().0;
                    let lhs = self.stack.pop().unwrap().0;
                    self.stack.push(Value(lhs / rhs));
                },
            }
        }
    }
}
