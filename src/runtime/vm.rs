//! Contains `Machine`, the Piccolo bytecode interpreter.

use crate::runtime::{memory::Heap, ChunkOffset};
use crate::{Chunk, Constant, ErrorKind, PiccoloError, Value};

use super::op::Opcode;

//use std::collections::HashMap;

use fnv::FnvHashMap as HashMap;

/// Interprets compiled Piccolo bytecode.
///
/// Contains a [`Chunk`] from which it executes instructions, a global variable hash
/// table, a stack for temporary values and local variables, and a [`Heap`] for long-lived
/// objects that require heap allocation, like strings, class instances, and others.
///
/// [`Chunk`]: ../chunk/struct.Chunk.html
/// [`Heap`]: ../memory/struct.Heap.html
pub struct Machine {
    ip: ChunkOffset,
    globals: HashMap<String, Value>,
    stack: Vec<Value>,
    heap: Heap,
}

impl Default for Machine {
    fn default() -> Machine {
        Machine::new()
    }
}

impl Machine {
    /// Creates a new machine from a chunk.
    pub fn new() -> Self {
        Machine {
            ip: 0,
            globals: HashMap::default(),
            stack: Vec::new(),
            heap: Heap::new(1024),
        }
    }

    /// Get the [`Heap`] of a VM.
    ///
    /// [`Heap`]: ../memory/struct.Heap.html
    pub fn heap(&mut self) -> &mut Heap {
        &mut self.heap
    }

    // TODO: determine if self.ip - 1 is necessary
    // this method is only ever called after self.ip is incremented
    // theoretically a program should never start with Opcode::Pop
    fn pop(&mut self, chunk: &Chunk) -> Result<Value, PiccoloError> {
        self.stack.pop().ok_or_else(|| {
            PiccoloError::new(ErrorKind::StackUnderflow {
                op: chunk.data[self.ip - 1].into(),
            })
            .line(chunk.get_line_from_index(self.ip))
            .msg("file a bug report!")
        })
    }

    fn peek_back(&self, dist: usize, chunk: &Chunk) -> Result<&Value, PiccoloError> {
        self.stack.get(self.stack.len() - dist - 1).ok_or_else(|| {
            PiccoloError::new(ErrorKind::StackUnderflow {
                op: chunk.data[self.ip - 1].into(),
            })
            .line(chunk.get_line_from_index(self.ip))
            .msg_string(format!("peek_back({})", dist))
        })
    }

    // get a constant from the chunk
    fn peek_constant<'a>(&mut self, chunk: &'a Chunk) -> &'a Constant {
        trace!("peek_constant");
        // Opcode::Constant takes a two-byte operand, meaning it's necessary
        // to decode the high and low bytes. the machine is little-endian with
        // constant addresses.
        chunk
            .constants
            .get(self.read_short(chunk) as usize)
            .unwrap()
    }

    fn read_short(&mut self, chunk: &Chunk) -> u16 {
        let short = chunk.read_short(self.ip);
        self.ip += 2;
        short
    }

    /// Interprets the machine's bytecode, returning a Constant.
    pub fn start_at(
        &mut self,
        chunk: &Chunk,
        start: ChunkOffset,
    ) -> Result<Constant, PiccoloError> {
        self.ip = start;
        self.interpret(chunk)
    }

    // TODO: probably even move out the heap from the machine
    #[allow(clippy::cognitive_complexity)]
    pub fn interpret(&mut self, chunk: &Chunk) -> Result<Constant, PiccoloError> {
        while self.ip < chunk.data.len() {
            // debug/macros {{{
            debug!(
                " ┌─{}{}",
                if self.ip + 1 == chunk.data.len() {
                    "─vm─exit─ "
                } else {
                    " "
                },
                super::memory::dbg_list(&self.stack, &self.heap),
            );
            debug!(
                " └─{} {}",
                if self.ip + 1 == chunk.data.len() {
                    "───────── "
                } else {
                    " "
                },
                chunk.disassemble_instruction(self.ip)
            );

            macro_rules! bit_op {
                ($opcode:path, $op:tt) => {
                    let rhs = self.pop(chunk)?;
                    let lhs = self.pop(chunk)?;
                    if lhs.is_integer() && rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        let lhs = lhs.into::<i64>();
                        self.stack.push(Value::Integer(lhs $op rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer".into(),
                            got: format!(
                                "{} {} {}",
                                self.heap.type_name(&lhs),
                                stringify!($op),
                                self.heap.type_name(&rhs)
                            ),
                            op: $opcode,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                };
            }

            // boolean argument to enable/disable string concatenation
            macro_rules! bin_op {
                ($opcode:path, $op:tt, nostring) => {
                    bin_op!($opcode, $op, false)
                };
                ($opcode:path, $op:tt, string) => {
                    bin_op!($opcode, $op, true)
                };
                ($opcode:path, $op:tt, $allow_string:tt) => {
                    let rhs = self.pop(chunk)?;
                    let lhs = self.pop(chunk)?;
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
                                got: format!("double {} {}", stringify!($op), self.heap.type_name(&rhs)),
                                op: $opcode,
                            })
                            .line(chunk.get_line_from_index(self.ip)));
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
                                got: format!("integer {} {}", stringify!($op), self.heap.type_name(&rhs)),
                                op: $opcode,
                            })
                            .line(chunk.get_line_from_index(self.ip)));
                        }
                    } else if $allow_string && lhs.is_string() {
                        let value = format!("{}{}", self.heap.fmt(&lhs), self.heap.fmt(&rhs));
                        let ptr = self.heap.alloc_string(&value);
                        self.stack.push(ptr);
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("{} {} {}", self.heap.type_name(&lhs), stringify!($op), self.heap.type_name(&rhs)),
                            op: $opcode,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                };
            }
            // }}}

            let inst = chunk.data[self.ip];
            self.ip += 1;

            let op = inst.into();
            match op {
                Opcode::Pop => {
                    if self.ip == chunk.data.len() {
                        trace!("last instruction pop");
                        let value = self.pop(chunk)?;
                        return Ok(self.heap.value_into_constant(value));
                    }
                    self.pop(chunk)?;
                }
                Opcode::Return => {
                    let v = self.pop(chunk)?;
                    println!("{}", self.heap.fmt(&v));
                }
                Opcode::Constant => {
                    let c = self.peek_constant(chunk);
                    let v = self.heap.constant_into_value(c);
                    self.stack.push(v);
                }
                Opcode::Nil => self.stack.push(Value::Nil),
                Opcode::True => self.stack.push(Value::Bool(true)),
                Opcode::False => self.stack.push(Value::Bool(false)),

                Opcode::Negate => {
                    let v = self.pop(chunk)?;
                    if v.is_double() {
                        let v = v.into::<f64>();
                        self.stack.push(Value::Double(-v));
                    } else if v.is_integer() {
                        let v = v.into::<i64>();
                        self.stack.push(Value::Integer(-v));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: self.heap.type_name(&v).to_owned(),
                            op: Opcode::Negate,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                }
                Opcode::Not => {
                    let v = self.pop(chunk)?;
                    if v.is_truthy() {
                        self.stack.push(Value::Bool(false));
                    } else {
                        self.stack.push(Value::Bool(true));
                    }
                }
                Opcode::Add => {
                    bin_op!(Opcode::Add, +, string);
                }
                Opcode::Subtract => {
                    bin_op!(Opcode::Subtract, -, nostring);
                }
                Opcode::Multiply => {
                    bin_op!(Opcode::Multiply, *, nostring);
                }
                Opcode::Divide => {
                    bin_op!(Opcode::Divide, /, nostring);
                }
                Opcode::Modulo => {
                    bin_op!(Opcode::Modulo, %, nostring);
                }

                // comparison {{{
                Opcode::Equal => {
                    let a = self.pop(chunk)?;
                    let b = self.pop(chunk)?;
                    self.stack
                        .push(Value::Bool(self.heap.eq(&a, &b).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: self.heap.type_name(&a).to_owned(),
                                    got: self.heap.type_name(&b).to_owned(),
                                    op,
                                })
                                .line(chunk.get_line_from_index(self.ip)))
                            },
                            Ok,
                        )?));
                }
                Opcode::Greater => {
                    let rhs = self.pop(chunk)?;
                    let lhs = self.pop(chunk)?;
                    if rhs.is_bool() || lhs.is_bool() {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "that isn't bool".into(),
                            got: "bool".into(),
                            op,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                    self.stack
                        .push(Value::Bool(self.heap.gt(&lhs, &rhs).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: self.heap.type_name(&lhs).to_owned(),
                                    got: self.heap.type_name(&rhs).to_owned(),
                                    op,
                                })
                                .line(chunk.get_line_from_index(self.ip)))
                            },
                            Ok,
                        )?));
                }
                Opcode::Less => {
                    let rhs = self.pop(chunk)?;
                    let lhs = self.pop(chunk)?;
                    if rhs.is_bool() || lhs.is_bool() {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "that isn't bool".into(),
                            got: "bool".into(),
                            op,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                    self.stack
                        .push(Value::Bool(self.heap.lt(&lhs, &rhs).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: self.heap.type_name(&lhs).to_owned(),
                                    got: self.heap.type_name(&rhs).to_owned(),
                                    op,
                                })
                                .line(chunk.get_line_from_index(self.ip)))
                            },
                            Ok,
                        )?));
                }
                Opcode::GreaterEqual => {
                    let rhs = self.pop(chunk)?;
                    let lhs = self.pop(chunk)?;
                    if rhs.is_bool() || lhs.is_bool() {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "that isn't bool".into(),
                            got: "bool".into(),
                            op,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                    self.stack
                        .push(Value::Bool(!self.heap.lt(&lhs, &rhs).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: self.heap.type_name(&lhs).to_owned(),
                                    got: self.heap.type_name(&rhs).to_owned(),
                                    op,
                                })
                                .line(chunk.get_line_from_index(self.ip)))
                            },
                            Ok,
                        )?));
                }
                Opcode::LessEqual => {
                    let rhs = self.pop(chunk)?;
                    let lhs = self.pop(chunk)?;
                    if rhs.is_bool() || lhs.is_bool() {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "type that isn't bool".into(),
                            got: "bool".into(),
                            op,
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                    self.stack
                        .push(Value::Bool(!self.heap.gt(&lhs, &rhs).map_or_else(
                            || {
                                Err(PiccoloError::new(ErrorKind::IncorrectType {
                                    exp: self.heap.type_name(&lhs).to_owned(),
                                    got: self.heap.type_name(&rhs).to_owned(),
                                    op,
                                })
                                .line(chunk.get_line_from_index(self.ip)))
                            },
                            Ok,
                        )?));
                } // }}}

                Opcode::GetLocal => {
                    let slot = self.read_short(chunk);
                    self.stack.push(self.stack[slot as usize]);
                }
                Opcode::SetLocal => {
                    let slot = self.read_short(chunk);
                    self.stack[slot as usize] = self.pop(chunk)?;
                }
                Opcode::GetGlobal => {
                    let name = self.peek_constant(chunk).ref_string();
                    if let Some(var) = self.globals.get(name) {
                        self.stack.push(*var);
                    } else {
                        return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                            name: name.to_owned(),
                        })
                        .line(chunk.get_line_from_index(self.ip)));
                    }
                }
                Opcode::SetGlobal => {
                    if let Constant::String(name) = self.peek_constant(chunk) {
                        let name = name.clone();
                        let value = self.pop(chunk)?;
                        if self.globals.insert(name.clone(), value).is_none() {
                            return Err(PiccoloError::new(ErrorKind::UndefinedVariable { name })
                                .line(chunk.get_line_from_index(self.ip)));
                        }
                    }
                }
                Opcode::DeclareGlobal => {
                    if let Constant::String(name) = self.peek_constant(chunk) {
                        let name = name.clone();
                        let value = self.pop(chunk)?;
                        self.globals.insert(name, value);
                    } else {
                        panic!("defined global with non-string name");
                    }
                }

                Opcode::JumpForward => {
                    let offset = self.read_short(chunk);
                    debug!("jump ip {:x} -> {:x}", self.ip, self.ip + offset as usize);
                    self.ip += offset as usize;
                }
                Opcode::JumpFalse => {
                    let offset = self.read_short(chunk);
                    if !self.peek_back(0, chunk)?.is_truthy() {
                        debug!(
                            "jump false ip {:x} -> {:x}",
                            self.ip,
                            self.ip + offset as usize
                        );
                        self.ip += offset as usize;
                    }
                }
                Opcode::JumpTrue => {
                    let offset = self.read_short(chunk);
                    if self.peek_back(0, chunk)?.is_truthy() {
                        debug!(
                            "jump true ip {:x} -> {:x}",
                            self.ip,
                            self.ip + offset as usize
                        );
                        self.ip += offset as usize;
                    }
                }
                Opcode::JumpBack => {
                    let offset = self.read_short(chunk);
                    debug!("loop ip {:x} -> {:x}", self.ip, self.ip - offset as usize);
                    self.ip -= offset as usize;
                }

                Opcode::BitAnd => {
                    bit_op!(Opcode::BitAnd, &);
                }
                Opcode::BitOr => {
                    bit_op!(Opcode::BitOr, |);
                }
                Opcode::BitXor => {
                    bit_op!(Opcode::BitXor, ^);
                }
                Opcode::ShiftLeft => {
                    bit_op!(Opcode::ShiftLeft, <<);
                }
                Opcode::ShiftRight => {
                    bit_op!(Opcode::ShiftRight, >>);
                }

                Opcode::Assert => {
                    let v = self.pop(chunk)?;
                    if !v.is_truthy() {
                        return Err(PiccoloError::new(ErrorKind::AssertFailed)
                            .line(chunk.get_line_from_index(self.ip - 1)));
                    }
                }
            }

            trace!("next instruction");
        }

        Ok(self
            .heap
            .value_into_constant(self.stack.pop().unwrap_or(Value::Nil)))
    }
}
