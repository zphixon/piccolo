use crate::chunk::Chunk;
use crate::error::{ErrorKind, PiccoloError};
use crate::op::Opcode;
use crate::value::Value;

use std::collections::HashMap;

/// Interprets compiled Piccolo bytecode.
#[derive(Default)]
pub struct Machine {
    chunk: Chunk,
    ip: usize,
    //strings: HashSet<Intern<String>>, //idfk
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
}

impl Machine {
    // TODO: make interpret hold a chunk rather than Machine owning it
    /// Creates a new machine.
    pub fn new(chunk: Chunk) -> Self {
        Machine {
            chunk,
            ip: 0,
            //strings: HashSet::new(),
            globals: HashMap::new(),
            stack: vec![],
        }
    }

    // TODO: determine if self.ip - 1 is necessary
    // this method is only ever called after self.ip is incremented
    // theoretically a program should never start with Opcode::Pop
    fn pop(&mut self) -> Result<Value, PiccoloError> {
        self.stack.pop().ok_or_else(|| {
            PiccoloError::new(ErrorKind::StackUnderflow {
                op: self.chunk.data[self.ip].into(),
            })
            .line(self.chunk.get_line_from_index(self.ip))
        })
    }

    fn peek(&self, dist: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - dist - 1)
    }

    fn constant(&self) -> Result<&Value, PiccoloError> {
        self.chunk
            .data
            .get(self.ip)
            .and_then(|low| {
                self.chunk.data.get(self.ip + 1).and_then(|high| {
                    self.chunk
                        .constants
                        .get(crate::encode_bytes(*low, *high) as usize)
                })
            })
            .ok_or_else(|| panic!("constant does not exist"))
    }

    /// Interprets the machine's bytecode, returning a Value.
    pub fn interpret(&mut self) -> Result<Value, PiccoloError> {
        while self.ip < self.chunk.data.len() {
            #[cfg(feature = "pc-debug")]
            {
                print!(" ┌─ {:?}\n └─ ", self.stack);
                self.chunk.disassemble_instruction(self.ip);
            }

            let line = self.chunk.get_line_from_index(self.ip);
            let inst = self.chunk.data[self.ip];
            self.ip += 1;

            let op = inst.into();
            match op {
                Opcode::Pop => {
                    if self.ip == self.chunk.data.len() {
                        return self.pop();
                    }
                    self.pop()?;
                }
                Opcode::Return => {
                    let v = self.pop()?;
                    println!("{}", v.fmt());
                }
                Opcode::DefineGlobal => {
                    if let Value::String(name) = self.constant()? {
                        let name = name.clone();
                        self.globals
                            .insert(name, self.stack[self.stack.len() - 1].try_clone());
                        self.pop()?;
                        self.ip += 2;
                    } else {
                        panic!("defined global with non-string name");
                    }
                }
                Opcode::GetGlobal => {
                    let name = self.constant()?.ref_string();
                    if let Some(var) = self.globals.get(name) {
                        self.stack.push(var.try_clone());
                    } else {
                        return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                            name: name.to_owned(),
                        })
                        .line(line));
                    }
                    self.ip += 2;
                }
                Opcode::AssignGlobal => {
                    if let Value::String(name) = self.constant()? {
                        let name = name.clone();
                        self.globals
                            .insert(name.clone(), self.peek(0).unwrap().try_clone())
                            .map_or_else(
                                || {
                                    Err(PiccoloError::new(ErrorKind::UndefinedVariable { name })
                                        .line(line))
                                },
                                |_| Ok(()),
                            )?;
                        self.pop()?;
                        self.ip += 2;
                    }
                }
                Opcode::Constant => {
                    let c = self.constant()?.try_clone();
                    self.ip += 2;

                    self.stack.push(c);
                }
                Opcode::Nil => self.stack.push(Value::Nil),
                Opcode::True => self.stack.push(Value::Bool(true)),
                Opcode::False => self.stack.push(Value::Bool(false)),
                Opcode::Negate => {
                    let v = self.pop()?;
                    if v.is_double() {
                        let v = v.into::<f64>();
                        self.stack.push(Value::Double(-v));
                    } else if v.is_integer() {
                        let v = v.into::<i64>();
                        self.stack.push(Value::Integer(-v));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: v.type_name().to_owned(),
                            op: Opcode::Negate,
                        })
                        .line(line));
                    }
                }
                Opcode::Not => {
                    let v = self.pop()?;
                    if v.is_truthy() {
                        self.stack.push(Value::Bool(false));
                    } else {
                        self.stack.push(Value::Bool(true));
                    }
                }
                Opcode::Equal => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(Value::Bool(
                        a.eq(&b).map_or(
                            Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: a.type_name().to_owned(),
                                got: b.type_name().to_owned(),
                                op,
                            })
                            .line(line)),
                            Ok,
                        )?,
                    ));
                }
                Opcode::Greater => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if rhs.is_bool() || lhs.is_bool() {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "that isn't bool".into(),
                            got: "bool".into(),
                            op,
                        })
                        .line(line));
                    }
                    self.stack.push(Value::Bool(
                        lhs.gt(&rhs).map_or(
                            Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: lhs.type_name().to_owned(),
                                got: rhs.type_name().to_owned(),
                                op,
                            })
                            .line(line)),
                            Ok,
                        )?,
                    ));
                }
                Opcode::Less => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if rhs.is_bool() || lhs.is_bool() {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "that isn't bool".into(),
                            got: "bool".into(),
                            op,
                        })
                        .line(line));
                    }
                    self.stack.push(Value::Bool(
                        rhs.gt(&lhs).map_or(
                            Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: lhs.type_name().to_owned(),
                                got: rhs.type_name().to_owned(),
                                op,
                            })
                            .line(line)),
                            Ok,
                        )?,
                    ));
                }
                Opcode::Add => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if lhs.is_double() {
                        let lhs = lhs.into::<f64>();
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs + rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Double(lhs + rhs as f64));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer, double, or string".into(),
                                got: format!("double + {}", rhs.type_name()),
                                op: Opcode::Add,
                            })
                            .line(line));
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
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer, double, or string".into(),
                                got: format!("integer + {}", rhs.type_name()),
                                op: Opcode::Add,
                            })
                            .line(line));
                        }
                    } else if lhs.is_string() {
                        let mut lhs = lhs.into::<String>();
                        lhs.push_str(&rhs.fmt().to_string());
                        self.stack.push(Value::String(lhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer, double, or string".into(),
                            got: format!("{} + {}", lhs.type_name(), rhs.type_name()),
                            op: Opcode::Add,
                        })
                        .line(line));
                    }
                }
                Opcode::Subtract => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if lhs.is_double() {
                        let lhs = lhs.into::<f64>();
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs - rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Double(lhs - rhs as f64));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("double - {}", rhs.type_name()),
                                op: Opcode::Subtract,
                            })
                            .line(line));
                        }
                    } else if lhs.is_integer() {
                        let lhs = lhs.into::<i64>();
                        if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Integer(lhs - rhs));
                        } else if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs as f64 - rhs));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("integer - {}", rhs.type_name()),
                                op: Opcode::Subtract,
                            })
                            .line(line));
                        }
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("{} - {}", lhs.type_name(), rhs.type_name()),
                            op: Opcode::Subtract,
                        })
                        .line(line));
                    }
                }
                Opcode::Multiply => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if lhs.is_double() {
                        let lhs = lhs.into::<f64>();
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs * rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Double(lhs * rhs as f64));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("double * {}", rhs.type_name()),
                                op: Opcode::Multiply,
                            })
                            .line(line));
                        }
                    } else if lhs.is_integer() {
                        let lhs = lhs.into::<i64>();
                        if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Integer(lhs * rhs));
                        } else if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs as f64 * rhs));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("integer * {}", rhs.type_name()),
                                op: Opcode::Multiply,
                            })
                            .line(line));
                        }
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("{} * {}", lhs.type_name(), rhs.type_name()),
                            op: Opcode::Multiply,
                        })
                        .line(line));
                    }
                }
                Opcode::Divide => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if lhs.is_double() {
                        let lhs = lhs.into::<f64>();
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs / rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Double(lhs / rhs as f64));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("double / {}", rhs.type_name()),
                                op: Opcode::Divide,
                            })
                            .line(line));
                        }
                    } else if lhs.is_integer() {
                        let lhs = lhs.into::<i64>();
                        if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Integer(lhs / rhs));
                        } else if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs as f64 / rhs));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("integer / {}", rhs.type_name()),
                                op: Opcode::Divide,
                            })
                            .line(line));
                        }
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("{} / {}", lhs.type_name(), rhs.type_name()),
                            op: Opcode::Divide,
                        })
                        .line(line));
                    }
                }
            }
        }
        Ok(Value::Nil)
    }
}
