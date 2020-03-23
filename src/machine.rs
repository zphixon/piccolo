use crate::chunk::Chunk;
use crate::error::PiccoloError;
use crate::op::Opcode;
use crate::value::Value;

pub struct Machine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl Machine {
    pub fn new(chunk: Chunk) -> Self {
        Machine {
            chunk,
            ip: 0,
            stack: vec![],
        }
    }

    pub fn interpret(&mut self) -> crate::Result<()> {
        loop {
            use PiccoloError::StackUnderflow;

            #[cfg(feature = "pc-debug")]
            {
                print!("┌─ {:?}\n└─ ", self.stack);
                self.chunk.disassemble_instruction(self.ip);
            }

            let line = self.chunk.get_line_from_index(self.ip);
            let inst = self.chunk.data[self.ip];
            self.ip += 1;

            let op = inst.into();
            match op {
                Opcode::Return => {
                    println!(
                        "{}",
                        self.stack.pop().unwrap_or_else(|| panic!(
                            "empty stack: {}",
                            self.chunk.get_line_from_index(self.ip - 1)
                        ))
                    );
                    return Ok(());
                }
                Opcode::Constant => {
                    let c = self.chunk.constants[self.chunk.data[self.ip] as usize].clone();
                    self.ip += 1;

                    self.stack.push(c);
                }
                Opcode::Negate => {
                    let v = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let v: f64 = v.into();
                    self.stack.push(Value::Double(-v));
                }
                Opcode::Add => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let rhs: f64 = rhs.into();
                    let lhs: f64 = lhs.into();
                    self.stack.push(Value::Double(lhs + rhs));
                }
                Opcode::Subtract => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let rhs: f64 = rhs.into();
                    let lhs: f64 = lhs.into();
                    self.stack.push(Value::Double(lhs - rhs));
                }
                Opcode::Multiply => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let rhs: f64 = rhs.into();
                    let lhs: f64 = lhs.into();
                    self.stack.push(Value::Double(lhs * rhs));
                }
                Opcode::Divide => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let rhs: f64 = rhs.into();
                    let lhs: f64 = lhs.into();
                    self.stack.push(Value::Double(lhs / rhs));
                }
            }
        }
    }
}
