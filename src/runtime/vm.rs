use crate::{
    Chunk, Constant, ErrorKind, Function, Heap, Module, NativeFunction, Object, Opcode,
    PiccoloError, Root, UniqueRoot, Value,
};

use fnv::FnvHashMap;

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
        let op = self.chunk.data[self.ip].into();
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

    fn current_line(&self) -> usize {
        self.current_chunk()
            .get_line_from_index(self.current_ip() - 1)
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

    fn read_short(&mut self) -> u16 {
        let s = self.current_chunk().read_short(self.current_ip());
        self.current_frame_mut().ip += 2;
        s
    }

    fn read_constant(&mut self, module: &Module) -> Constant {
        let constant_index = self.read_short();
        let c = module.get_constant(constant_index).clone();
        c
    }

    fn unwind(&self) -> Vec<crate::error::Callsite> {
        let mut calls = Vec::new();
        for frame in self.frames.iter().take(self.frames.len() - 1) {
            calls.push(crate::error::Callsite {
                name: frame.name.clone(),
                line: frame.chunk.get_line_from_index(frame.ip),
            })
        }
        calls
    }
}

type PiccoloFunction = for<'value> fn(&[Value<'value>]) -> Value<'value>;
//type PiccoloFunction<'a> = fn(&[Value<'a>]) -> Value<'a>;

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
        s.push_str(&format!("{}", value));
        if i != values.len() {
            s.push('\t');
        }
    }
    println!("{}", s);
    Value::Nil
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

        let mut native_functions = FnvHashMap::default();
        native_functions.insert(String::from("print"), print as PiccoloFunction);

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
                chunk: &module.chunk(0),
                function: f,
            }],
        };

        // loop until stop, return from top, or error
        // TODO refactor to make this not look like hot garbage
        loop {
            let result = self.interpret_next_instruction(heap, module, &mut frames);
            if result.is_ok() {
                let state = result.unwrap();
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
                    .map_err(|err| err.line(frames.current_line()).stack_trace(frames.unwind()))?;
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
            self.stack,
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
            Opcode::Constant => {
                let c = frames.read_constant(module);
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
                let slot = frames.read_short() as usize + frames.current_frame().base;
                debug!("get local slot {}", slot);
                self.push(self.stack[slot].clone());
            }
            Opcode::SetLocal => {
                let slot = frames.read_short() as usize + frames.current_frame().base;
                debug!("set local slot {}", slot);
                self.stack[slot] = self.pop();
            }
            Opcode::GetGlobal => {
                let constant = frames.read_constant(module);
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
                let constant = frames.read_constant(module);
                let name = constant.ref_string();

                let value = self.pop();
                if self.globals.insert(name.to_string(), value).is_none() {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.to_string(),
                    }));
                }
            }
            Opcode::DeclareGlobal => {
                let constant = frames.read_constant(module);
                let name = constant.ref_string();

                let value = self.pop();
                self.globals.insert(name.to_string(), value);
            }

            Opcode::JumpForward => {
                let offset = frames.read_short() as usize;

                debug!(
                    "jump ip {:x} -> {:x}",
                    frames.current_ip(),
                    frames.current_ip() + offset
                );
                frames.current_frame_mut().ip += offset;
            }
            Opcode::JumpFalse => {
                let offset = frames.read_short() as usize;

                if !self.peek().is_truthy() {
                    debug!(
                        "jump false ip {:x} -> {:x}",
                        frames.current_ip(),
                        frames.current_frame_mut().ip + offset
                    );

                    frames.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpTrue => {
                let offset = frames.read_short() as usize;

                if self.peek().is_truthy() {
                    debug!(
                        "jump true ip {:x} -> {:x}",
                        frames.current_ip(),
                        frames.current_frame_mut().ip + offset
                    );

                    frames.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpBack => {
                let offset = frames.read_short() as usize;

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
                bit_op!(Opcode::ShiftLeft, <<);
            }
            Opcode::ShiftRight => {
                bit_op!(Opcode::ShiftRight, >>);
            }

            Opcode::Call => {
                let arity = frames.read_short();
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

            Opcode::Assert => {
                let v = self.pop();
                let assertion = frames.read_constant(module);
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
    use crate::debug::*;
    use crate::prelude::*;

    #[test]
    fn how_could_this_happen_to_me() {
        //env_logger::init();

        let src = r#"""+(11*3)+"heehee""#;
        let ast = parse(&mut Scanner::new(src)).expect("parse");
        let module = compile(&ast).expect("emit");

        println!("{}", disassemble(&module, ""));

        let mut heap = Heap::default();

        let mut vm = Machine::new(&mut heap);
        vm.interpret(&mut heap, &module).unwrap();
    }
}
