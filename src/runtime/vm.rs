use crate::{ErrorKind, PiccoloError};

use super::{chunk::Chunk, op::Opcode, value::Value};

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

    // get a constant from the chunk
    fn constant(&self) -> Result<&Value, PiccoloError> {
        // Opcode::Constant takes a two-byte operand, meaning it's necessary
        // to decode the high and low bytes. the machine is little-endian with
        // constant addresses.
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

            // boolean argument to enable/disable string concatenation
            macro_rules! bin_op {
                ($opcode:path, $op:tt) => {
                    bin_op!($opcode, $op, false)
                };
                ($opcode:path, $op:tt, $concat:tt) => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    if lhs.is_double() {
                        let lhs = lhs.into::<f64>();
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs $op rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Double(lhs $op rhs as f64));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("double {} {}", stringify!($op), rhs.type_name()),
                                op: $opcode,
                            })
                            .line(line));
                        }
                    } else if lhs.is_integer() {
                        let lhs = lhs.into::<i64>();
                        if rhs.is_integer() {
                            let rhs = rhs.into::<i64>();
                            self.stack.push(Value::Integer(lhs $op rhs));
                        } else if rhs.is_double() {
                            let rhs = rhs.into::<f64>();
                            self.stack.push(Value::Double(lhs as f64 $op rhs));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("integer {} {}", stringify!($op), rhs.type_name()),
                                op: $opcode,
                            })
                            .line(line));
                        }
                    } else if $concat && lhs.is_string() {
                        let mut lhs = lhs.into::<String>();
                        lhs.push_str(&rhs.fmt().to_string());
                        self.stack.push(Value::String(lhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("{} {} {}", lhs.type_name(), stringify!($op), rhs.type_name()),
                            op: $opcode,
                        })
                        .line(line));
                    }
                };
            }

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
                    println!("{}", v);
                }
                Opcode::Assert => {
                    let v = self.pop()?;
                    if !v.is_truthy() {
                        return Err(PiccoloError::new(ErrorKind::AssertFailed).line(line));
                    }
                }
                Opcode::DefineGlobal => {
                    if let Value::String(name) = self.constant()? {
                        let name = name.clone();
                        self.globals.insert(
                            name,
                            self.stack[self.stack.len() - 1]
                                .try_clone()
                                .ok_or_else(|| {
                                    PiccoloError::new(ErrorKind::CannotClone {
                                        ty: self.peek(0).unwrap().type_name().to_owned(),
                                    })
                                    .line(line)
                                })?,
                        );
                        self.pop()?;
                        self.ip += 2;
                    } else {
                        panic!("defined global with non-string name");
                    }
                }
                Opcode::GetGlobal => {
                    let name = self.constant()?.ref_string();
                    if let Some(var) = self.globals.get(name) {
                        self.stack.push(var.try_clone().ok_or_else(|| {
                            PiccoloError::new(ErrorKind::CannotClone {
                                ty: var.type_name().to_owned(),
                            })
                            .line(line)
                            .msg_string(format!("global {}", name))
                        })?);
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
                            .insert(
                                name.clone(),
                                self.peek(0).unwrap().try_clone().ok_or_else(|| {
                                    PiccoloError::new(ErrorKind::CannotClone {
                                        ty: self.peek(0).unwrap().type_name().to_owned(),
                                    })
                                    .line(line)
                                    .msg_string(format!("global {}", name))
                                })?,
                            )
                            .map_or_else(
                                || {
                                    Err(PiccoloError::new(ErrorKind::UndefinedVariable { name })
                                        .line(line))
                                },
                                |_| Ok(()),
                            )?;
                        self.ip += 2;
                    }
                }
                Opcode::Constant => {
                    let c = self.constant()?;
                    let c = c.try_clone().ok_or_else(|| {
                        PiccoloError::new(ErrorKind::CannotClone {
                            ty: c.type_name().to_owned(),
                        })
                        .line(line)
                    })?;
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
                        lhs.lt(&rhs).map_or(
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
                Opcode::GreaterEqual => {
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
                        !lhs.lt(&rhs).map_or(
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
                Opcode::LessEqual => {
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
                        !lhs.gt(&rhs).map_or(
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
                    bin_op!(Opcode::Add, +, true);
                }
                Opcode::Subtract => {
                    bin_op!(Opcode::Subtract, -);
                }
                Opcode::Multiply => {
                    bin_op!(Opcode::Multiply, *);
                }
                Opcode::Divide => {
                    bin_op!(Opcode::Multiply, /);
                }
            }
        }
        Ok(self.stack.pop().unwrap_or(Value::Nil))
    }
}
