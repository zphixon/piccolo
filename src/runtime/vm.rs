use crate::{
    compiler::SourcePos,
    debug,
    error::{ErrorKind, PiccoloError},
    runtime::{
        chunk::{Chunk, Module},
        memory::{Heap, Root, UniqueRoot},
        object::{Function, NativeFunction, Object},
        op::Opcode,
        value::Value,
    },
    trace,
};
use fnv::FnvHashMap;
use std::fmt::Write;

pub struct Frame<'chunk, 'value> {
    name: String,
    ip: usize,
    base: usize,
    chunk: &'chunk Chunk,
    function: Root<'value, Function>,
    //closure: Root<Closure>,
}

impl Frame<'_, '_> {
    fn step(&mut self) -> Opcode {
        let op = self.chunk.ops[self.ip];
        self.ip += 1;
        op
    }
}

pub struct FrameStack<'chunk, 'value> {
    frames: Vec<Frame<'chunk, 'value>>,
}

impl<'chunk, 'value> FrameStack<'chunk, 'value> {
    fn len(&self) -> usize {
        self.frames.len()
    }

    fn push(&mut self, frame: Frame<'chunk, 'value>) {
        self.frames.push(frame);
    }

    fn pop(&mut self) -> Frame<'chunk, 'value> {
        self.frames.pop().unwrap()
    }

    fn current_line(&self) -> SourcePos {
        self.current_chunk()
            .get_pos_from_index(self.current_ip() - 1)
    }

    fn current_ip(&self) -> usize {
        self.current_frame().ip
    }

    fn current_frame<'this>(&'this self) -> &'this Frame<'chunk, 'value> {
        self.frames.last().unwrap()
    }

    fn current_frame_mut<'this>(&'this mut self) -> &'this mut Frame<'chunk, 'value> {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        self.current_frame().chunk
    }

    fn unwind(&self) -> Vec<crate::error::Callsite> {
        let mut calls = Vec::new();
        for frame in self.frames.iter().take(self.frames.len() - 1) {
            calls.push(crate::error::Callsite {
                name: frame.name.clone(),
                pos: frame.chunk.get_pos_from_index(frame.ip),
            })
        }
        calls
    }
}

pub type PiccoloFunction = for<'value> fn(&[Value<'value>]) -> Value<'value>;

pub struct Machine<'value> {
    stack: UniqueRoot<'value, Vec<Value<'value>>>,
    globals: UniqueRoot<'value, FnvHashMap<String, Value<'value>>>,
    native_functions: FnvHashMap<String, PiccoloFunction>,
    ip: usize,
}

#[derive(Copy, Clone)]
enum VmState<'value> {
    Continue,
    ReturnFromTop(Value<'value>, usize),
    Stop(Value<'value>),
}

fn print<'value>(values: &[Value<'value>]) -> Value<'value> {
    let mut s = String::new();
    for (i, value) in values.iter().enumerate() {
        write!(s, "{value}").unwrap();
        if i != values.len() {
            s.push('\t');
        }
    }
    println!("{s}");
    Value::Nil
}

fn rand<'value>(_: &[Value<'value>]) -> Value<'value> {
    Value::Double(rand::random())
}

impl<'value> Machine<'value> {
    pub fn new(heap: &mut Heap<'value>) -> Self {
        let mut globals = heap.manage_unique(FnvHashMap::default());

        // TODO make this nicer
        globals.insert(
            String::from("print"),
            Value::NativeFunction({
                heap.manage(NativeFunction {
                    arity: 0,
                    name: "print".to_string(),
                })
                .as_gc()
            }),
        );

        globals.insert(
            String::from("rand"),
            Value::NativeFunction({
                heap.manage(NativeFunction {
                    arity: 0,
                    name: "rand".to_string(),
                })
                .as_gc()
            }),
        );

        let mut native_functions = FnvHashMap::default();
        native_functions.insert(String::from("print"), print as PiccoloFunction);
        native_functions.insert(String::from("rand"), rand as PiccoloFunction);

        Machine {
            stack: heap.manage_unique(Vec::new()),
            globals,
            native_functions,
            ip: 0,
        }
    }

    fn push(&mut self, value: Value<'value>) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value<'value> {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> &Value<'value> {
        &self.stack[self.stack.len() - 1]
    }

    fn peek_back(&self, len: usize) -> &Value<'value> {
        &self.stack[self.stack.len() - 1 - len]
    }

    fn push_string(&mut self, heap: &mut Heap<'value>, string: String) {
        let root = heap.manage(string);
        self.push(Value::String(root.as_gc()));
    }

    pub fn clear_stack_and_move_to_end_of_module(&mut self, module: &Module) {
        self.stack.clear();
        self.ip = module.chunk(0).len();
    }

    pub fn interpret(
        &mut self,
        heap: &mut Heap<'value>,
        module: &Module,
    ) -> Result<Value<'value>, PiccoloError> {
        self.interpret_from(heap, module, 0)
    }

    pub fn interpret_continue(
        &mut self,
        heap: &mut Heap<'value>,
        module: &Module,
    ) -> Result<Value<'value>, PiccoloError> {
        self.interpret_from(heap, module, self.ip)
    }

    fn interpret_from<'chunk>(
        &mut self,
        heap: &mut Heap<'value>,
        module: &'chunk Module,
        ip: usize,
    ) -> Result<Value<'value>, PiccoloError> {
        // :)
        let f = heap.manage(Function::new(0, String::from("top level"), 0));
        //self.push(Value::Function(f.as_gc()));

        let mut frames = FrameStack {
            frames: vec![Frame {
                name: String::from("top level"),
                base: 0,
                ip,
                chunk: module.chunk(0),
                function: f,
            }],
        };

        // loop until stop, return from top, or error
        // TODO refactor to make this not look like hot garbage
        loop {
            let result = self.interpret_next_instruction(heap, module, &mut frames);
            if let Ok(state) = result {
                match state {
                    VmState::Continue => {}
                    VmState::Stop(value) => {
                        self.ip = frames.current_ip();
                        return Ok(value);
                    }
                    VmState::ReturnFromTop(value, ip) => {
                        self.ip = ip;
                        return Ok(value);
                    }
                }
            } else if result.is_err() {
                self.ip = frames.current_ip();
                result
                    .map_err(|err| err.pos(frames.current_line()).stack_trace(frames.unwind()))?;
            }
        }
    }

    fn interpret_next_instruction<'chunk>(
        &mut self,
        heap: &mut Heap<'value>,
        module: &'chunk Module,
        frames: &mut FrameStack<'chunk, 'value>,
    ) -> Result<VmState<'value>, PiccoloError> {
        // TODO: move to Opcode::Return
        if frames.current_ip() + 1 > frames.current_chunk().len() {
            return Ok(VmState::Stop(Value::Nil));
        }

        // debug {{{
        debug!(
            " ┌─{} {}.{:04x} {:?}",
            if frames.current_ip() + 1 == frames.current_chunk().len() {
                "─vm─exit─"
            } else {
                ""
            },
            module.index_of(frames.current_chunk()),
            frames.current_ip(),
            self.stack
        );
        debug!(
            " └─{} {}",
            if frames.current_ip() + 1 == frames.current_chunk().len() {
                "─────────"
            } else {
                ""
            },
            crate::runtime::chunk::disassemble_instruction(
                module,
                frames.current_chunk(),
                frames.current_ip()
            )
        );
        // }}}

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

        let op = frames.current_frame_mut().step();
        match op {
            Opcode::Pop => {
                if frames.current_ip() == frames.current_chunk().len() {
                    trace!("last instruction pop");
                    let value = self.pop();
                    return Ok(VmState::ReturnFromTop(value, frames.current_ip()));
                }
                self.pop();
            }
            Opcode::Return => {
                let result = self.pop();
                let frame = frames.pop();
                // close upvalues
                self.stack.truncate(frame.base);
                if frames.len() == 0 {
                    return Ok(VmState::ReturnFromTop(result, frame.ip));
                }
                self.push(result);
            }
            Opcode::Constant(index) => {
                let c = module.get_constant(index);
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

            // binary ops {{{
            Opcode::Add => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs + rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Double(lhs + rhs as f64));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double + {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Integer(lhs.wrapping_add(rhs)));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 + rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer + {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else if lhs.is_string() {
                    let value = format!("{lhs}{rhs}");
                    self.push_string(heap, value);
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} + {}", lhs.type_name(), rhs.type_name()),
                        op,
                    }));
                }
            }

            Opcode::Subtract => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs - rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Double(lhs - rhs as f64));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double - {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Integer(lhs.wrapping_sub(rhs)));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 - rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer - {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} - {}", lhs.type_name(), rhs.type_name()),
                        op,
                    }));
                }
            }

            Opcode::Multiply => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs * rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Double(lhs * rhs as f64));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double * {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Integer(lhs.wrapping_mul(rhs)));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 * rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer * {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} * {}", lhs.type_name(), rhs.type_name()),
                        op,
                    }));
                }
            }

            Opcode::Divide => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs / rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Double(lhs / rhs as f64));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double / {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        if rhs == 0 {
                            return Err(PiccoloError::new(ErrorKind::DivideByZero));
                        }
                        self.push(Value::Integer(lhs / rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 / rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer / {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} / {}", lhs.type_name(), rhs.type_name()),
                        op,
                    }));
                }
            }

            Opcode::Modulo => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs % rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value::Double(lhs % rhs as f64));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double % {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        if rhs == 0 {
                            return Err(PiccoloError::new(ErrorKind::DivideByZero));
                        }
                        self.push(Value::Integer(lhs % rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 % rhs));
                    } else {
                        return Err(PiccoloError::new(ErrorKind::IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer % {}", rhs.type_name()),
                            op,
                        }));
                    }
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} % {}", lhs.type_name(), rhs.type_name()),
                        op,
                    }));
                }
            }
            // }}}

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

            Opcode::GetLocal(slot) => {
                let slot = slot as usize + frames.current_frame().base;
                debug!("get local slot {}", slot);
                self.push(self.stack[slot]);
            }
            Opcode::SetLocal(slot) => {
                let slot = slot as usize + frames.current_frame().base;
                debug!("set local slot {}", slot);
                self.stack[slot] = self.pop();
            }
            Opcode::GetGlobal(index) => {
                let constant = module.get_constant(index);
                let name = constant.ref_string();

                if self.globals.contains_key(name) {
                    self.push(self.globals[name]);
                } else {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.to_owned(),
                    }));
                }
            }
            Opcode::SetGlobal(index) => {
                let constant = module.get_constant(index);
                let name = constant.ref_string();

                let value = self.pop();
                if self.globals.insert(name.to_string(), value).is_none() {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.to_string(),
                    }));
                }
            }
            Opcode::DeclareGlobal(index) => {
                let constant = module.get_constant(index);
                let name = constant.ref_string();

                let value = self.pop();
                self.globals.insert(name.to_string(), value);
            }

            Opcode::JumpForward(offset) => {
                let offset = offset as usize - 1;

                debug!(
                    "jump ip {:x} -> {:x}",
                    frames.current_ip(),
                    frames.current_ip() + offset
                );
                frames.current_frame_mut().ip += offset;
            }
            Opcode::JumpFalse(offset) => {
                let offset = offset as usize - 1;

                if !self.peek().is_truthy() {
                    debug!(
                        "jump false ip {:x} -> {:x}",
                        frames.current_ip(),
                        frames.current_frame_mut().ip + offset
                    );

                    frames.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpTrue(offset) => {
                let offset = offset as usize - 1;

                if self.peek().is_truthy() {
                    debug!(
                        "jump true ip {:x} -> {:x}",
                        frames.current_ip(),
                        frames.current_frame_mut().ip + offset
                    );

                    frames.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpBack(offset) => {
                let offset = offset as usize + 1;

                debug!(
                    "loop ip {:x} -> {:x}",
                    frames.current_ip(),
                    frames.current_ip() - offset
                );
                frames.current_frame_mut().ip -= offset;
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
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_integer() && rhs.is_integer() {
                    let rhs_i64 = rhs.into::<i64>();
                    let rhs = match rhs_i64.try_into() {
                        Ok(rhs) => rhs,
                        Err(_) => {
                            return Err(PiccoloError::new(ErrorKind::InvalidShift {
                                value: rhs_i64,
                            }));
                        }
                    };

                    let lhs = lhs.into::<i64>();
                    self.push(Value::Integer(lhs.wrapping_shl(rhs)));
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer".into(),
                        got: format!("{} << {}", lhs.type_name(), rhs.type_name(),),
                        op,
                    }));
                }
            }

            Opcode::ShiftRight => {
                let rhs = self.pop();
                let lhs = self.pop();
                if lhs.is_integer() && rhs.is_integer() {
                    let rhs_i64 = rhs.into::<i64>();
                    let rhs = match rhs_i64.try_into() {
                        Ok(rhs) => rhs,
                        Err(_) => {
                            return Err(PiccoloError::new(ErrorKind::InvalidShift {
                                value: rhs_i64,
                            }));
                        }
                    };

                    let lhs = lhs.into::<i64>();
                    self.push(Value::Integer(lhs.wrapping_shr(rhs)));
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "integer".into(),
                        got: format!("{} << {}", lhs.type_name(), rhs.type_name(),),
                        op,
                    }));
                }
            }

            Opcode::Call(arity) => {
                if let Value::Function(_) = self.peek_back(arity as usize) {
                    let f = self.peek_back(arity as usize).as_function();

                    if f.arity() != arity as usize {
                        return Err(PiccoloError::new(ErrorKind::IncorrectArity {
                            name: f.name().to_owned(),
                            exp: f.arity(),
                            got: arity as usize,
                        }));
                    }

                    debug!(
                        "go to chunk {} base {}",
                        f.chunk(),
                        self.stack.len() as u16 - 1 - arity
                    );

                    frames.push(Frame {
                        name: f.name().to_string(),
                        ip: 0,
                        base: (self.stack.len() as u16 - arity - 1) as usize,
                        chunk: module.chunk(f.chunk()),
                        function: heap.root(f),
                    });
                } else if let Value::NativeFunction(_) = self.peek_back(arity as usize) {
                    let mut args = vec![];
                    for _ in 0..arity {
                        args.insert(0, self.pop());
                    }
                    let f = self.pop().as_native_function();
                    self.push(self.native_functions[&f.name](&args));
                } else {
                    return Err(PiccoloError::new(ErrorKind::IncorrectType {
                        exp: "fn".to_owned(),
                        got: self.peek_back(arity as usize).type_name().to_owned(),
                        op,
                    }));
                }
            }

            Opcode::Assert(index) => {
                let v = self.pop();
                let assertion = module.get_constant(index);
                if !v.is_truthy() {
                    let assertion = assertion.ref_string().to_owned();
                    return Err(PiccoloError::new(ErrorKind::AssertFailed { assertion }));
                }
            }
        }

        Ok(VmState::Continue)
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn how_could_this_happen_to_me() {
        use crate::{
            compiler::{emitter, parser},
            runtime::{chunk, memory::Heap, vm::Machine},
        };

        let src = r#"""+(11*3)+"heehee""#;
        let ast = parser::parse(src).expect("parse");
        let module = emitter::compile(&ast).expect("emit");

        println!("{}", chunk::disassemble(&module, ""));

        let mut heap = Heap::default();

        let mut vm = Machine::new(&mut heap);
        vm.interpret(&mut heap, &module).unwrap();
    }
}
