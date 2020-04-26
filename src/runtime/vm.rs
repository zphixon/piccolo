use crate::runtime::chunk::Constant;
use crate::runtime::memory::Heap;
use crate::{ErrorKind, PiccoloError};

use super::{chunk::Chunk, op::Opcode, value::Value};

use std::collections::HashMap;

/// Interprets compiled Piccolo bytecode.
pub struct Machine {
    chunk: Chunk,
    ip: usize,
    //strings: HashSet<Intern<String>>, //idfk
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
    heap: Heap,
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
            stack: Vec::new(),
            heap: Heap::new(1024),
        }
    }

    pub fn heap(&mut self) -> &mut Heap {
        &mut self.heap
    }

    // TODO: determine if self.ip - 1 is necessary
    // this method is only ever called after self.ip is incremented
    // theoretically a program should never start with Opcode::Pop
    fn pop(&mut self) -> Result<Value, PiccoloError> {
        self.stack.pop().ok_or_else(|| {
            PiccoloError::new(ErrorKind::StackUnderflow {
                op: self.chunk.data[self.ip - 1].into(),
            })
            .line(self.chunk.get_line_from_index(self.ip))
            .msg("file a bug report!")
        })
    }

    #[allow(dead_code)]
    fn peek_back(&self, dist: usize) -> Result<&Value, PiccoloError> {
        self.stack.get(self.stack.len() - dist - 1).ok_or_else(|| {
            PiccoloError::new(ErrorKind::StackUnderflow {
                op: self.chunk.data[self.ip - 1].into(),
            })
            .line(self.chunk.get_line_from_index(self.ip))
            .msg_string(format!("peek_back({})", dist))
        })
    }

    // get a constant from the chunk
    fn peek_constant(&self) -> &Constant {
        // Opcode::Constant takes a two-byte operand, meaning it's necessary
        // to decode the high and low bytes. the machine is little-endian with
        // constant addresses.
        self.chunk
            .constants
            .get(self.chunk.read_short(self.ip) as usize)
            .unwrap()
    }

    /// Interprets the machine's bytecode, returning a Constant.
    pub fn interpret(&mut self) -> Result<Constant, PiccoloError> {
        while self.ip < self.chunk.data.len() {
            #[cfg(feature = "pc-debug")]
            {
                let (exit_msg, exit_spc) = if self.ip + 1 == self.chunk.data.len() {
                    ("─vm─exit─ ", "───────── ")
                } else {
                    (" ", " ")
                };
                print!(
                    " ┌─{}{}\n └─{}",
                    exit_msg,
                    super::value::dbg_list(&self.stack, &self.heap),
                    exit_spc
                );
                self.chunk.disassemble_instruction(self.ip);
            }

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
                        let lhs = lhs.into::<f64>(&mut self.heap);
                        if rhs.is_double() {
                            let rhs = rhs.into::<f64>(&mut self.heap);
                            self.stack.push(Value::Double(lhs $op rhs));
                        } else if rhs.is_integer() {
                            let rhs = rhs.into::<i64>(&mut self.heap);
                            self.stack.push(Value::Double(lhs $op rhs as f64));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("double {} {}", stringify!($op), rhs.type_name(&self.heap)),
                                op: $opcode,
                            })
                            .line(self.chunk.get_line_from_index(self.ip)));
                        }
                    } else if lhs.is_integer() {
                        let lhs = lhs.into::<i64>(&mut self.heap);
                        if rhs.is_integer() {
                            let rhs = rhs.into::<i64>(&mut self.heap);
                            self.stack.push(Value::Integer(lhs $op rhs));
                        } else if rhs.is_double() {
                            let rhs = rhs.into::<f64>(&mut self.heap);
                            self.stack.push(Value::Double(lhs as f64 $op rhs));
                        } else {
                            return Err(PiccoloError::new(ErrorKind::IncorrectType {
                                exp: "integer or double".into(),
                                got: format!("integer {} {}", stringify!($op), rhs.type_name(&self.heap)),
                                op: $opcode,
                            })
                            .line(self.chunk.get_line_from_index(self.ip)));
                        }
                    } else if $concat && lhs.is_string(&self.heap) {
                        let value = format!("{}{}", lhs.fmt(&self.heap), rhs.fmt(&self.heap));
                        let ptr = self.heap.alloc(Box::new(value));
                        self.stack.push(Value::Object(ptr));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("{} {} {}", lhs.type_name(&self.heap), stringify!($op), rhs.type_name(&self.heap)),
                            op: $opcode,
                        })
                        .line(self.chunk.get_line_from_index(self.ip)));
                    }
                };
            }

            let op = inst.into();
            match op {
                Opcode::Pop => {
                    if self.ip == self.chunk.data.len() {
                        return Ok(Constant::from_value(self.pop()?, &mut self.heap));
                    }
                    self.pop()?;
                }
                Opcode::Return => {
                    let v = self.pop()?;
                    println!("{}", v.fmt(&self.heap));
                }
                Opcode::Constant => {
                    let c = self.peek_constant().clone().into_value(&mut self.heap);
                    self.stack.push(c);
                    self.ip += 2;
                }
                Opcode::Nil => self.stack.push(Value::Nil),
                Opcode::True => self.stack.push(Value::Bool(true)),
                Opcode::False => self.stack.push(Value::Bool(false)),

                Opcode::Negate => {
                    let v = self.pop()?;
                    if v.is_double() {
                        let v = v.into::<f64>(&mut self.heap);
                        self.stack.push(Value::Double(-v));
                    } else if v.is_integer() {
                        let v = v.into::<i64>(&mut self.heap);
                        self.stack.push(Value::Integer(-v));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: v.type_name(&self.heap).to_owned(),
                            op: Opcode::Negate,
                        })
                        .line(self.chunk.get_line_from_index(self.ip - 1)));
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
                Opcode::Modulo => {
                    bin_op!(Opcode::Multiply, %);
                }

                Opcode::Equal => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack
                        .push(Value::Bool(a.eq(&b, &self.heap).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: a.type_name(&self.heap).to_owned(),
                                    got: b.type_name(&self.heap).to_owned(),
                                    op,
                                })
                                .line(self.chunk.get_line_from_index(self.ip - 1)))
                            },
                            Ok,
                        )?));
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
                        .line(self.chunk.get_line_from_index(self.ip - 1)));
                    }
                    self.stack
                        .push(Value::Bool(lhs.gt(&rhs, &self.heap).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: lhs.type_name(&self.heap).to_owned(),
                                    got: rhs.type_name(&self.heap).to_owned(),
                                    op,
                                })
                                .line(self.chunk.get_line_from_index(self.ip - 1)))
                            },
                            Ok,
                        )?));
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
                        .line(self.chunk.get_line_from_index(self.ip - 1)));
                    }
                    self.stack
                        .push(Value::Bool(lhs.lt(&rhs, &self.heap).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: lhs.type_name(&self.heap).to_owned(),
                                    got: rhs.type_name(&self.heap).to_owned(),
                                    op,
                                })
                                .line(self.chunk.get_line_from_index(self.ip - 1)))
                            },
                            Ok,
                        )?));
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
                        .line(self.chunk.get_line_from_index(self.ip - 1)));
                    }
                    self.stack
                        .push(Value::Bool(!lhs.lt(&rhs, &self.heap).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: lhs.type_name(&self.heap).to_owned(),
                                    got: rhs.type_name(&self.heap).to_owned(),
                                    op,
                                })
                                .line(self.chunk.get_line_from_index(self.ip - 1)))
                            },
                            Ok,
                        )?));
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
                        .line(self.chunk.get_line_from_index(self.ip - 1)));
                    }
                    self.stack
                        .push(Value::Bool(!lhs.gt(&rhs, &self.heap).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: lhs.type_name(&self.heap).to_owned(),
                                    got: rhs.type_name(&self.heap).to_owned(),
                                    op,
                                })
                                .line(self.chunk.get_line_from_index(self.ip - 1)))
                            },
                            Ok,
                        )?));
                }

                // TODO: the try_clone deep clones the entire value which isn't what we want
                // one idea is to have Value::Object just be an index into a Vec<Option<Box<dyn Object>>>
                // which would be two layers of indirection, so it wouldn't be that fast but it's
                // the actual behavior that we want, and that would make it slightly easier to reason
                // about lifetime-wise...
                Opcode::GetLocal => {
                    let slot = self.chunk.read_short(self.ip);
                    self.stack.push(self.stack[slot as usize]);
                    self.ip += 2;
                }
                Opcode::SetLocal => {
                    let slot = self.chunk.read_short(self.ip);
                    self.stack[slot as usize] = self.pop()?;
                    self.ip += 2;
                }
                Opcode::GetGlobal => {
                    let name = self.peek_constant().ref_string();
                    if let Some(var) = self.globals.get(name) {
                        if let Some(var) = var.try_clone(&mut self.heap) {
                            self.stack.push(var);
                        } else {
                            return Err(PiccoloError::new(ErrorKind::CannotClone {
                                ty: var.type_name(&self.heap).to_owned(),
                            })
                            .line(self.chunk.get_line_from_index(self.ip - 1)));
                        }
                    } else {
                        return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                            name: name.to_owned(),
                        })
                        .line(self.chunk.get_line_from_index(self.ip - 1)));
                    }
                    self.ip += 2;
                }
                Opcode::SetGlobal => {
                    if let Constant::String(name) = self.peek_constant() {
                        let name = name.clone();
                        let value = self.pop()?;
                        if self.globals.insert(name.clone(), value).is_none() {
                            return Err(PiccoloError::new(ErrorKind::UndefinedVariable { name })
                                .line(self.chunk.get_line_from_index(self.ip - 1)));
                        }
                        self.ip += 2;
                    }
                }
                Opcode::DeclareGlobal => {
                    if let Constant::String(name) = self.peek_constant() {
                        let name = name.clone();
                        let value = self.pop()?;
                        self.globals.insert(name, value);
                        self.ip += 2;
                    } else {
                        panic!("defined global with non-string name");
                    }
                }

                Opcode::Assert => {
                    let v = self.pop()?;
                    if !v.is_truthy() {
                        return Err(PiccoloError::new(ErrorKind::AssertFailed)
                            .line(self.chunk.get_line_from_index(self.ip - 1)));
                    }
                }
            }
        }
        Ok(Constant::from_value(
            self.stack.pop().unwrap_or(Value::Nil),
            &mut self.heap,
        ))
    }
}
