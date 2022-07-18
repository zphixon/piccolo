use crate::{
    compiler::Pos,
    debug,
    error::PiccoloError,
    make_error,
    runtime::{
        chunk::{Chunk, Module},
        interner::StringPtr,
        op::Opcode,
        value::{Array, Value},
        Context, ContextMut, Object, This,
    },
    trace, warn,
};
use fnv::FnvHashMap;

pub struct Frame<'chunk> {
    name: StringPtr,
    ip: usize,
    base: usize,
    chunk: &'chunk Chunk,
    // TODO: these need to be part of the iterator yielding rooted Objects
    //function: Function,
    //closure: Root<Closure>,
}

impl Frame<'_> {
    fn step(&mut self) -> Opcode {
        let op = self.chunk.ops[self.ip];
        self.ip += 1;
        op
    }

    #[allow(dead_code)]
    fn current_op(&self) -> Opcode {
        self.chunk.ops[self.ip]
    }
}

pub struct FrameStack<'chunk> {
    frames: Vec<Frame<'chunk>>,
}

impl<'chunk> FrameStack<'chunk> {
    fn len(&self) -> usize {
        self.frames.len()
    }

    fn push(&mut self, frame: Frame<'chunk>) {
        self.frames.push(frame);
    }

    fn pop(&mut self) -> Frame<'chunk> {
        self.frames.pop().unwrap()
    }

    fn current_pos(&self) -> Pos {
        self.current_chunk()
            .get_pos_from_index(self.current_ip() - 1)
    }

    fn current_ip(&self) -> usize {
        self.current_frame().ip
    }

    fn current_frame<'this>(&'this self) -> &'this Frame<'chunk> {
        self.frames.last().unwrap()
    }

    fn current_frame_mut<'this>(&'this mut self) -> &'this mut Frame<'chunk> {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        self.current_frame().chunk
    }

    fn unwind(&self, ctx: Context) -> Vec<crate::error::Callsite> {
        warn!("unwinding stack");
        let mut calls = Vec::new();
        for frame in self.frames.iter().take(self.frames.len() - 1) {
            warn!("-- {}", ctx.interner.get_string(frame.name));
            calls.push(crate::error::Callsite {
                name: ctx.interner.get_string(frame.name).to_string(),
                pos: frame.chunk.get_pos_from_index(frame.ip),
            })
        }
        calls
    }
}

#[derive(Debug, Default)]
pub struct Machine {
    stack: Vec<Value>,
    pub(crate) globals: FnvHashMap<StringPtr, Value>,
    ip: usize,
}

#[derive(Copy, Clone)]
enum VmState {
    Continue,
    ReturnFromTop(Value, usize),
    Stop(Value),
}

impl Machine {
    pub fn new() -> Self {
        Machine::default()
    }

    pub fn roots(&self) -> impl Iterator<Item = &crate::runtime::memory::Ptr> {
        self.stack
            .iter()
            .flat_map(|value| {
                if let Value::Object(ptr) = value {
                    Some(ptr)
                } else {
                    None
                }
            })
            .chain(self.globals.values().flat_map(|value| {
                if let Value::Object(ptr) = value {
                    Some(ptr)
                } else {
                    None
                }
            }))
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

    fn peek_back(&self, len: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - len]
    }

    pub fn clear_stack_and_move_to_end_of_module(&mut self, module: &Module) {
        self.stack.clear();
        self.ip = module.chunk(0).len();
    }

    pub fn interpret(
        &mut self,
        ctx: &mut ContextMut,
        module: &Module,
    ) -> Result<Value, PiccoloError> {
        self.interpret_from(ctx, module, 0)
    }

    pub fn interpret_continue(
        &mut self,
        ctx: &mut ContextMut,
        module: &Module,
    ) -> Result<Value, PiccoloError> {
        self.interpret_from(ctx, module, self.ip)
    }

    fn interpret_from<'chunk>(
        &mut self,
        ctx: &mut ContextMut,
        module: &'chunk Module,
        ip: usize,
    ) -> Result<Value, PiccoloError> {
        // :)
        //let f = heap.manage(Function::new(0, String::from("top level"), 0));
        //self.push(Value::Function(f.as_gc()));

        let mut frames = FrameStack {
            frames: vec![Frame {
                name: ctx.interner.allocate_str("top level"),
                base: 0,
                ip,
                chunk: module.chunk(0),
                //function: f,
            }],
        };

        // loop until stop, return from top, or error
        // TODO refactor to make this not look like hot garbage
        loop {
            let result = self.interpret_next_instruction(ctx, module, &mut frames);
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
                result.map_err(|err| {
                    err.pos(frames.current_pos())
                        .stack_trace(frames.unwind(ctx.as_ref()))
                })?;
            }
        }
    }

    fn interpret_next_instruction<'chunk>(
        &mut self,
        ctx: &mut ContextMut,
        module: &'chunk Module,
        frames: &mut FrameStack<'chunk>,
    ) -> Result<VmState, PiccoloError> {
        // TODO: move to Opcode::Return
        if frames.current_ip() + 1 > frames.current_chunk().len() {
            return Ok(VmState::Stop(Value::Nil));
        }

        // debug {{{
        debug!(
            " ┌─{} {}.{:04x} {}",
            if frames.current_ip() + 1 == frames.current_chunk().len() {
                "─vm─exit─"
            } else {
                ""
            },
            module.index_of(frames.current_chunk()),
            frames.current_ip(),
            {
                #[cfg(feature = "color")]
                {
                    use tcolor::{Color, ColorString};
                    let mut s = ColorString::new_fg("[", Color::BrightBlue);
                    for (i, value) in self.stack.iter().enumerate() {
                        s.push(value.color_format(ctx.as_ref()));
                        if i + 1 != self.stack.len() {
                            s.push_string(", ", Color::BrightBlue);
                        }
                    }
                    s.push_string("]", Color::BrightBlue);
                    s
                }
                #[cfg(not(feature = "color"))]
                {
                    let mut s = String::from("[");
                    for (i, value) in self.stack.iter().enumerate() {
                        s.push_str(&value.debug_format(ctx.as_ref()));
                        if i + 1 != self.stack.len() {
                            s.push_str(", ");
                        }
                    }
                    s.push(']');
                    s
                }
            }
        );
        debug!(
            " └─{} {}",
            if frames.current_ip() + 1 == frames.current_chunk().len() {
                "─────────"
            } else {
                ""
            },
            {
                #[cfg(feature = "cli")]
                let value = crate::pretty::disassemble_instruction(
                    ctx.interner,
                    module,
                    frames.current_chunk(),
                    frames.current_ip(),
                );

                #[cfg(not(feature = "cli"))]
                let value = format!("{:?}", frames.current_frame().current_op());

                value
            }
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
                    return Err(make_error!(IncorrectType {
                        exp: "integer".into(),
                        got: format!(
                            "{} {} {}",
                            lhs.type_name(ctx.as_ref()),
                            stringify!($op),
                            rhs.type_name(ctx.as_ref()),
                        ),
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
                self.push(Value::from_constant(c));
            }
            Opcode::Nil => self.push(Value::Nil),
            Opcode::Bool(b) => self.push(Value::Bool(b)),
            Opcode::Integer(u) => self.push(Value::Integer(u as i64)),

            Opcode::Array(len) => {
                let len = len as usize;
                let mut values = Vec::with_capacity(len);
                for _ in 0..len {
                    values.push(self.pop());
                }
                values.reverse();

                self.push(Value::Object(ctx.heap.allocate(Array::new_with(values))));
            }

            Opcode::Negate => {
                let v = self.pop();
                if v.is_double() {
                    let v = v.into::<f64>();
                    self.push(Value::Double(-v));
                } else if v.is_integer() {
                    let v = v.into::<i64>();
                    self.push(Value::Integer(-v));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: v.type_name(ctx.as_ref()).to_owned(),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double + {}", rhs.type_name(ctx.as_ref())),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer + {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else if lhs.is_string() {
                    let value = format!("{}{}", lhs.format(ctx.as_ref()), rhs.format(ctx.as_ref()));
                    self.push(Value::String(ctx.interner.allocate_string(value)));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!(
                            "{} + {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref())
                        ),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double - {}", rhs.type_name(ctx.as_ref())),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer - {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!(
                            "{} - {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref())
                        ),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double * {}", rhs.type_name(ctx.as_ref())),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer * {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!(
                            "{} * {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref())
                        ),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double / {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        if rhs == 0 {
                            return Err(make_error!(DivideByZero));
                        }
                        self.push(Value::Integer(lhs / rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 / rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer / {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!(
                            "{} / {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref())
                        ),
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
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double % {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        if rhs == 0 {
                            return Err(make_error!(DivideByZero));
                        }
                        self.push(Value::Integer(lhs % rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value::Double(lhs as f64 % rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer % {}", rhs.type_name(ctx.as_ref())),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!(
                            "{} % {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref())
                        ),
                    }));
                }
            }
            // }}}

            // comparison {{{
            Opcode::Equal => {
                let a = self.pop();
                let b = self.pop();
                self.push(Value::Bool(a.eq(ctx.as_ref(), b)?));
            }
            Opcode::Greater => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(lhs.gt(ctx.as_ref(), rhs)?));
            }
            Opcode::Less => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(lhs.lt(ctx.as_ref(), rhs)?));
            }
            Opcode::GreaterEqual => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(!lhs.lt(ctx.as_ref(), rhs)?));
            }
            Opcode::LessEqual => {
                let rhs = self.pop();
                let lhs = self.pop();
                self.push(Value::Bool(!lhs.gt(ctx.as_ref(), rhs)?));
            } // }}}

            Opcode::GetLocal(slot) => {
                let slot = slot as usize + frames.current_frame().base;
                trace!("get local slot {}", slot);
                self.push(self.stack[slot]);
            }
            Opcode::SetLocal(slot) => {
                let slot = slot as usize + frames.current_frame().base;
                trace!("set local slot {}", slot);
                self.stack[slot] = self.pop();
            }
            Opcode::GetGlobal(index) => {
                let constant = module.get_constant(index);
                let ptr = constant.string_ptr();

                if self.globals.contains_key(&ptr) {
                    self.push(self.globals[&ptr]);
                } else {
                    return Err(make_error!(UndefinedVariable {
                        name: ctx.interner.get_string(ptr).to_string(),
                    }));
                }
            }
            Opcode::SetGlobal(index) => {
                let constant = module.get_constant(index);
                let ptr = constant.string_ptr();

                let value = self.pop();
                if self.globals.insert(ptr, value).is_none() {
                    return Err(make_error!(UndefinedVariable {
                        name: ctx.interner.get_string(ptr).to_string(),
                    }));
                }
            }
            Opcode::DeclareGlobal(index) => {
                let constant = module.get_constant(index);
                let ptr = constant.string_ptr();

                let value = self.pop();
                self.globals.insert(ptr, value);
            }

            Opcode::Get => {
                let index = self.pop();
                let value = self.pop();
                self.push(value.get(ctx.as_ref(), This::None, index)?);
            }

            Opcode::Set => {
                let index = self.pop();
                if self.peek().is_object() {
                    let ptr = self.pop().as_ptr();
                    let value = self.pop();
                    unsafe { ctx.heap.get_mut(ptr).set(ctx.as_ref(), index, value)? };
                } else {
                    return Err(make_error!(CannotGet {
                        object: self.peek().type_name(ctx.as_ref()).to_string(),
                        index: index.format(ctx.as_ref()),
                    }));
                }
            }

            Opcode::JumpForward(offset) => {
                let offset = offset as usize - 1;

                trace!(
                    "jump ip {:x} -> {:x}",
                    frames.current_ip(),
                    frames.current_ip() + offset
                );
                frames.current_frame_mut().ip += offset;
            }
            Opcode::JumpFalse(offset) => {
                let offset = offset as usize - 1;

                if !self.peek().is_truthy() {
                    trace!(
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
                    trace!(
                        "jump true ip {:x} -> {:x}",
                        frames.current_ip(),
                        frames.current_frame_mut().ip + offset
                    );

                    frames.current_frame_mut().ip += offset;
                }
            }
            Opcode::JumpBack(offset) => {
                let offset = offset as usize + 1;

                trace!(
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
                            return Err(make_error!(InvalidShift { value: rhs_i64 }));
                        }
                    };

                    let lhs = lhs.into::<i64>();
                    self.push(Value::Integer(lhs.wrapping_shl(rhs)));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer".into(),
                        got: format!(
                            "{} << {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref()),
                        ),
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
                            return Err(make_error!(InvalidShift { value: rhs_i64 }));
                        }
                    };

                    let lhs = lhs.into::<i64>();
                    self.push(Value::Integer(lhs.wrapping_shr(rhs)));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer".into(),
                        got: format!(
                            "{} << {}",
                            lhs.type_name(ctx.as_ref()),
                            rhs.type_name(ctx.as_ref())
                        ),
                    }));
                }
            }

            Opcode::Call(arity) => {
                let arity = arity as usize;
                if let Value::Function(_) = self.peek_back(arity as usize) {
                    let f = self.peek_back(arity).as_function();

                    if !f.arity.is_compatible(arity) {
                        return Err(make_error!(IncorrectArity {
                            name: ctx.interner.get_string(f.name).to_string(),
                            exp: f.arity,
                            got: arity,
                        }));
                    }

                    trace!(
                        "go to chunk {} base {}",
                        f.chunk,
                        self.stack.len() - 1 - arity
                    );

                    frames.push(Frame {
                        name: f.name,
                        ip: 0,
                        base: self.stack.len() - arity - 1,
                        chunk: module.chunk(f.chunk),
                        //function: heap.allocate(f),
                    });
                } else if let Value::BuiltinFunction(_) = self.peek_back(arity) {
                    let mut args = Vec::with_capacity(arity);
                    for _ in 0..arity {
                        args.push(self.pop());
                    }
                    let f = self.pop().as_builtin_function();
                    match f.this {
                        This::Ptr(ptr) => args.push(Value::Object(ptr)),
                        This::String(ptr) => args.push(Value::String(ptr)),
                        _ => {}
                    }
                    args.reverse();

                    if f.arity.is_compatible(args.len()) {
                        self.push((f.ptr)(ctx, &args)?);
                    } else {
                        return Err(make_error!(IncorrectArity {
                            name: ctx.interner.get_string(f.name()).to_string(),
                            exp: f.arity,
                            got: arity,
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "fn".to_owned(),
                        got: self.peek_back(arity).type_name(ctx.as_ref()).to_owned(),
                    }));
                }
            }

            Opcode::Assert(index) => {
                let v = self.pop();
                let assertion = module.get_constant(index);
                if !v.is_truthy() {
                    let ptr = assertion.string_ptr();
                    let assertion = ctx.interner.get_string(ptr).to_owned();
                    return Err(make_error!(AssertFailed { assertion }));
                }
            }
        }

        Ok(VmState::Continue)
    }
}
