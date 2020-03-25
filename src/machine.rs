use crate::chunk::Chunk;
use crate::error::PiccoloError;
use crate::op::Opcode;
use crate::value::Nil;
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

    pub fn interpret(&mut self) -> crate::Result<Value> {
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
                    return Ok(self.stack.pop().unwrap_or(Value::Nil(Nil)));
                }
                Opcode::Constant => {
                    let c = self.chunk.constants[self.chunk.data[self.ip] as usize].clone();
                    self.ip += 1;

                    self.stack.push(c);
                }
                Opcode::Nil => self.stack.push(Value::Nil(Nil)),
                Opcode::True => self.stack.push(Value::Bool(true)),
                Opcode::False => self.stack.push(Value::Bool(false)),
                Opcode::Negate => {
                    let v = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    if v.is_double() {
                        let v = v.into::<f64>();
                        self.stack.push(Value::Double(-v));
                    } else if v.is_integer() {
                        let v = v.into::<i64>();
                        self.stack.push(Value::Integer(-v));
                    } else {
                        return Err(PiccoloError::IncorrectType {
                            exp: "integer or double".into(),
                            got: v.type_name().to_owned(),
                            op: Opcode::Negate,
                            line,
                        }
                        .into());
                    }
                }
                Opcode::Not => {
                    let v = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    if v.is_truthy() {
                        self.stack.push(Value::Bool(false));
                    } else {
                        self.stack.push(Value::Bool(true));
                    }
                }
                Opcode::Add => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    if lhs.is_double() {
                        let lhs = lhs.into::<f64>();
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs + rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Double(lhs + rhs as f64));
                        } else {
                            return Err(PiccoloError::IncorrectType {
                                exp: "integer, double, or string".into(),
                                got: format!("double + {}", rhs.type_name()),
                                op: Opcode::Add,
                                line,
                            }
                            .into());
                        }
                    } else if lhs.is_integer() {
                        let lhs = lhs.into::<i64>();
                        if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Integer(lhs + rhs));
                        } else if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs as f64 + rhs));
                        } else {
                            return Err(PiccoloError::IncorrectType {
                                exp: "integer, double, or string".into(),
                                got: format!("integer + {}", rhs.type_name()),
                                op: Opcode::Add,
                                line,
                            }
                            .into());
                        }
                    } else if lhs.is_string() {
                        let mut lhs = lhs.into::<String>();
                        lhs.push_str(&format!("{}", rhs));
                        self.stack.push(Value::String(lhs));
                    } else {
                        return Err(PiccoloError::IncorrectType {
                            exp: "integer, double, or string".into(),
                            got: format!("{} + {}", lhs.type_name(), rhs.type_name()),
                            op: Opcode::Add,
                            line,
                        }
                        .into());
                    }
                }
                Opcode::Subtract => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs: f64 = lhs.into();
                    let rhs: f64 = rhs.into();
                    self.stack.push(Value::Double(lhs - rhs));
                }
                Opcode::Multiply => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs: f64 = lhs.into();
                    let rhs: f64 = rhs.into();
                    self.stack.push(Value::Double(lhs * rhs));
                }
                Opcode::Divide => {
                    let rhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs = self.stack.pop().ok_or(StackUnderflow { line, op })?;
                    let lhs: f64 = lhs.into();
                    let rhs: f64 = rhs.into();
                    self.stack.push(Value::Double(lhs / rhs));
                }
            }
        }
    }
}
