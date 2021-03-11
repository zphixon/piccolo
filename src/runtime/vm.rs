use crate::{
    Chunk, ChunkOffset, Constant, ErrorKind, Function, Heap, Line, LocalSlotIndex, Module,
    NativeFunction, Object, Opcode, PiccoloError, Root, UniqueRoot, Value,
};

use fnv::FnvHashMap;

pub struct Frame<'a> {
    name: String,
    ip: ChunkOffset,
    base: ChunkOffset,
    chunk: &'a Chunk,
    function: Root<Function>,
    //closure: Root<Closure>,
}

impl Frame<'_> {
    fn step(&mut self) -> Opcode {
        let op = self.chunk.data[self.ip].into();
        self.ip += 1;
        op
    }
}

pub struct Machine<'a> {
    module: &'a Module,
    frames: Vec<Frame<'a>>,
    stack: UniqueRoot<Vec<Value>>,
    globals: UniqueRoot<FnvHashMap<String, Value>>,
}

#[derive(Copy, Clone)]
enum VmState {
    Continue,
    Stop(Value),
}

impl<'a> Machine<'a> {
    pub fn new(heap: &mut Heap, module: &'a Module) -> Self {
        let mut globals = heap.manage_unique(FnvHashMap::default());

        // TODO make this nicer
        globals.insert(
            String::from("print"),
            Value::NativeFunction({
                heap.manage(NativeFunction {
                    arity: 0,
                    name: "print".to_string(),
                    function: |values| {
                        let mut s = String::new();
                        for (i, value) in values.iter().enumerate() {
                            s.push_str(&format!("{}", value));
                            if i != values.len() {
                                s.push('\t');
                            }
                        }
                        println!("{}", s);
                        Value::Nil
                    },
                })
                .as_gc()
            }),
        );

        Machine {
            module,
            frames: Vec::new(),
            stack: heap.manage_unique(Vec::new()),
            globals,
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> &Value {
        &self.stack[self.stack.len() - 1]
    }

    fn read_short(&mut self) -> u16 {
        let s = self.current_chunk().read_short(self.current_ip());
        self.current_frame_mut().ip += 2;
        s
    }

    fn read_constant(&mut self) -> Constant {
        let constant_index = self.read_short();
        let c = self.module.get_constant(constant_index).clone();
        c
    }

    fn push_string(&mut self, heap: &mut Heap, string: String) {
        let root = heap.manage(string);
        self.push(Value::String(root.as_gc()));
    }

    pub fn interpret(&mut self, heap: &mut Heap) -> Result<Value, PiccoloError> {
        // :)
        let f = heap.manage(Function::new(0, String::new(), 0));
        //self.push(Value::Function(f.as_gc()));

        self.frames.push(Frame {
            name: String::new(),
            base: 0,
            ip: 0,
            chunk: &self.module.chunk(0),
            function: f,
        });

        loop {
            match self
                .interpret_next_instruction(heap)
                .map_err(|e| e.line(self.current_line()))?
            {
                VmState::Continue => {}
                VmState::Stop(value) => return Ok(value),
            }
        }
    }

    fn interpret_next_instruction(&mut self, heap: &mut Heap) -> Result<VmState, PiccoloError> {
        // TODO: move to Opcode::Return
        if self.current_ip() + 1 > self.current_chunk().len() {
            return Ok(VmState::Stop(Value::Nil));
        }

        // debug {{{
        debug!(
            " ┌─{} {}.{:04x} {:?}",
            if self.current_ip() + 1 == self.current_chunk().len() {
                "─vm─exit─"
            } else {
                ""
            },
            self.module.index_of(self.current_chunk()),
            self.current_ip(),
            self.stack,
        );
        debug!(
            " └─{} {}",
            if self.current_ip() + 1 == self.current_chunk().len() {
                "─────────"
            } else {
                ""
            },
            crate::runtime::chunk::disassemble_instruction(
                self.module,
                self.current_chunk(),
                self.current_ip()
            )
        );
        // }}}

        // bit/bin op {{{
        macro_rules! bit_op {
            ($opcode:path, $op:tt) => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_integer() && rhs.is_integer() {
                    let rhs = rhs.into::<i64>();
                    let lhs = lhs.into::<i64>();
                    self.push(Value::Integer(lhs $op rhs));
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer".into(),
                        got: format!(
                            "{} {} {}",
                            lhs.type_name(),
                            stringify!($op),
                            rhs.type_name(),
                        ),
                        op: $opcode,
                    }));
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
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs $op rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Double(lhs $op rhs as f64));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double {} {}", stringify!($op), rhs.type_name()),
                            op: $opcode,
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Integer(lhs $op rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 $op rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer {} {}", stringify!($op), rhs.type_name()),
                            op: $opcode,
                        }));
                    }
                } else if $allow_string && lhs.is_string() {
                    let value = format!("{}{}", &lhs, &rhs);
                    self.push_string(heap, value);
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} {} {}", lhs.type_name(), stringify!($op), rhs.type_name()),
                        op: $opcode,
                    }));
                }
            };
        }
        // }}}

        let op = self.current_frame_mut().step();
        match op {
            Opcode::Pop => {
                if self.current_ip() == self.current_chunk().len() {
                    trace!("last instruction pop");
                    let value = self.pop();
                    return Ok(VmState::Stop(value));
                }
                self.pop();
            }
            Opcode::Return => {
                let result = self.pop();
                let frame = self.frames.pop().unwrap();
                // close upvalues
                self.stack.truncate(frame.base);
                if self.frames.len() == 0 {
                    return Ok(VmState::Stop(result));
                }
                self.push(result);
            }
            Opcode::Constant => {
                let c = self.read_constant();
                self.push(Value::from_constant(c, heap));
            }
            Opcode::Nil => self.push(Value::Nil),
            Opcode::True => self.push(Value::Bool(true)),
            Opcode::False => self.push(Value::Bool(false)),

            Opcode::Negate => {
                let v = self.pop();
                if v.is_double() {
                    let v = v.into::<f64>();
                    self.push(Value::Double(-v));
                } else if v.is_integer() {
                    let v = v.into::<i64>();
                    self.push(Value::Integer(-v));
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: v.type_name().to_owned(),
                        op: Opcode::Negate,
                    }));
                }
            }
            Opcode::Not => {
                let v = self.pop();
                if v.is_truthy() {
                    self.push(Value::Bool(false));
                } else {
                    self.push(Value::Bool(true));
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
                let a = self.pop();
                let b = self.pop();
                self.push(Value::Bool(a.eq(&b).map_or_else(
                    || {
                        Err(PiccoloError::new(ErrorKind::CannotCompare {
                            exp: a.type_name().to_owned(),
                            got: b.type_name().to_owned(),
                        }))
                    },
                    Ok,
                )?));
            }
            Opcode::Greater => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(lhs.gt(&rhs).map_or_else(
                    || {
                        Err(PiccoloError::new(ErrorKind::CannotCompare {
                            exp: lhs.type_name().to_owned(),
                            got: rhs.type_name().to_owned(),
                        }))
                    },
                    Ok,
                )?));
            }
            Opcode::Less => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(lhs.lt(&rhs).map_or_else(
                    || {
                        Err(PiccoloError::new(ErrorKind::CannotCompare {
                            exp: lhs.type_name().to_owned(),
                            got: rhs.type_name().to_owned(),
                        }))
                    },
                    Ok,
                )?));
            }
            Opcode::GreaterEqual => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(!lhs.lt(&rhs).map_or_else(
                    || {
                        Err(PiccoloError::new(ErrorKind::CannotCompare {
                            exp: lhs.type_name().to_owned(),
                            got: rhs.type_name().to_owned(),
                        }))
                    },
                    Ok,
                )?));
            }
            Opcode::LessEqual => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(!lhs.gt(&rhs).map_or_else(
                    || {
                        Err(PiccoloError::new(ErrorKind::CannotCompare {
                            exp: lhs.type_name().to_owned(),
                            got: rhs.type_name().to_owned(),
                        }))
                    },
                    Ok,
                )?));
            } // }}}

            Opcode::GetLocal => {
                let slot = self.read_short() as usize + self.current_frame().base;
                self.push(self.stack[slot as usize].clone());
            }
            Opcode::SetLocal => {
                let slot = self.read_short() as LocalSlotIndex;
                self.stack[slot as usize] = self.pop();
            }
            Opcode::GetGlobal => {
                let constant = self.read_constant();
                let name = constant.ref_string();

                if self.globals.contains_key(name) {
                    let var = self.globals[name].clone();
                    self.push(var);
                } else {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.to_owned(),
                    }));
                }
            }
            Opcode::SetGlobal => {
                let constant = self.read_constant();
                let name = constant.ref_string();

                let value = self.pop();
                if self.globals.insert(name.to_string(), value).is_none() {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.to_string(),
                    }));
                }
            }
            Opcode::DeclareGlobal => {
                let constant = self.read_constant();
                let name = constant.ref_string();

                let value = self.pop();
                self.globals.insert(name.to_string(), value);
            }

            Opcode::JumpForward => {
                let offset = self.read_short() as ChunkOffset;

                debug!(
                    "jump ip {:x} -> {:x}",
                    self.current_ip(),
                    self.current_ip() + offset
                );
                self.current_frame_mut().ip += offset;
            }
            Opcode::JumpFalse => {
                let offset = self.read_short() as ChunkOffset;

                if !self.peek().is_truthy() {
                    debug!(
                        "jump false ip {:x} -> {:x}",
                        self.current_ip(),
                        self.current_frame_mut().ip + offset
                    );

                    self.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpTrue => {
                let offset = self.read_short() as ChunkOffset;

                if self.peek().is_truthy() {
                    debug!(
                        "jump true ip {:x} -> {:x}",
                        self.current_ip(),
                        self.current_frame_mut().ip + offset
                    );

                    self.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpBack => {
                let offset = self.read_short() as ChunkOffset;

                debug!(
                    "loop ip {:x} -> {:x}",
                    self.current_ip(),
                    self.current_ip() - offset
                );
                self.current_frame_mut().ip -= offset;
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

            Opcode::Call => {
                let arity = self.read_short();
                // using the chunk index from the function value, change current frame to be in the chunk specified
                // basically push a call frame whose chunk is the chunk_index of the function
                // match function kind
                if let Value::Function(f) = self.peek() {
                    //let f = self.pop().as_function(); // TODO???
                    self.frames.push(Frame {
                        name: f.name().to_string(),
                        ip: 0,
                        base: (self.stack.len() as u16 - arity - 1) as usize,
                        chunk: self.module.chunk(f.chunk()),
                        function: heap.root(*f),
                    });
                } else if let Value::NativeFunction(_) = self.peek() {
                    let f = self.pop().as_native_function();
                    let mut args = vec![];
                    for _ in 0..arity {
                        args.insert(0, self.pop());
                    }
                    self.push((f.function)(&args));
                }
            }

            Opcode::Assert => {
                let v = self.pop();
                let assertion = self.read_constant();
                if !v.is_truthy() {
                    let assertion = assertion.ref_string().to_owned();
                    return Err(PiccoloError::new(ErrorKind::AssertFailed { assertion }));
                }
            }
        }

        Ok(VmState::Continue)
    }

    fn current_line(&self) -> Line {
        self.current_chunk()
            .get_line_from_index(self.current_ip() - 1)
    }

    fn current_ip(&self) -> ChunkOffset {
        self.current_frame().ip
    }

    fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut Frame<'a> {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        self.current_frame().chunk
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn how_could_this_happen_to_me() {
        //env_logger::init();

        let src = r#"""+(11*3)+"heehee""#;
        let ast = crate::compiler::parser::parse(&mut crate::compiler::scanner::Scanner::new(src))
            .expect("parse");
        let mut emitter = crate::compiler::emitter::Emitter::new();
        let module = crate::compiler::emitter::compile(&ast).expect("emit");

        println!("{}", crate::runtime::chunk::disassemble(&module, ""));

        let mut heap = crate::runtime::memory::Heap::default();

        let mut vm = Machine::new(&mut heap, &module);
        vm.interpret(&mut heap).unwrap();
    }
}
