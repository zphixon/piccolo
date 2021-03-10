use crate::{
    Chunk, ChunkOffset, Constant, ErrorKind, Function, Gc, Heap, Line, LocalSlotIndex,
    NativeFunction, Object, PiccoloError, UniqueRoot,
};

use super::op::Opcode;

use crate::runtime::chunk::Module;
use fnv::FnvHashMap;

// value2 {{{
#[derive(Copy, Clone, Debug)]
pub enum Value2 {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(Gc<String>),
    Function(Gc<Function>),
    NativeFunction(Gc<NativeFunction>),
    Object(Gc<dyn Object>),
    Nil,
}

//impl std::fmt::Debug for Value2 {
//    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        write!(f, "value2")
//    }
//}

impl Value2 {
    /// A value is only false-y if it is of type bool and false, or of type nil.
    /// All other values are truth-y.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value2::Bool(b) => *b,
            Value2::Nil => false,
            _ => true,
        }
    }

    pub fn from_constant(c: Constant, h: &mut Heap) -> Value2 {
        match c {
            Constant::Bool(v) => Value2::Bool(v),
            Constant::Integer(v) => Value2::Integer(v),
            Constant::Double(v) => Value2::Double(v),
            Constant::String(v) => Value2::String({
                let root = h.manage(v);
                root.as_gc()
            }),
            Constant::Function(v) => Value2::Function({
                let root = h.manage(v);
                root.as_gc()
            }),
            Constant::Nil => Value2::Nil,
        }
    }

    pub fn into_constant(self) -> Constant {
        match self {
            Value2::Bool(v) => Constant::Bool(v),
            Value2::Integer(v) => Constant::Integer(v),
            Value2::Double(v) => Constant::Double(v),
            Value2::String(v) => Constant::String(String::clone(&*v)),
            Value2::Function(_) => Constant::Nil,
            Value2::NativeFunction(_) => Constant::Nil,
            Value2::Object(_) => Constant::Nil,
            Value2::Nil => Constant::Nil,
        }
    }

    pub fn into<T>(self) -> T
    where
        Value2: Into<T>,
    {
        std::convert::Into::into(self)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value2::String(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value2::Bool(_))
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Value2::Integer(_))
    }

    pub fn is_double(&self) -> bool {
        matches!(self, Value2::Double(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value2::Function(_))
    }

    pub fn as_function(&self) -> Gc<Function> {
        assert!(self.is_function());
        match self {
            Value2::Function(f) => *f,
            _ => panic!(),
        }
    }

    pub fn is_native_function(&self) -> bool {
        matches!(self, Value2::NativeFunction(_))
    }

    pub fn as_native_function(&self) -> Gc<NativeFunction> {
        assert!(self.is_native_function());
        match self {
            Value2::NativeFunction(f) => *f,
            _ => panic!(),
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value2::Nil)
    }

    pub fn eq(&self, other: &Value2) -> Option<bool> {
        Some(match self {
            Value2::Bool(l) => match other {
                Value2::Bool(r) => l == r,
                _ => None?,
            },
            Value2::Integer(l) => match other {
                Value2::Integer(r) => l == r,
                Value2::Double(r) => *l as f64 == *r,
                _ => None?,
            },
            Value2::Double(l) => match other {
                Value2::Integer(r) => *l == *r as f64,
                Value2::Double(r) => l == r,
                _ => None?,
            },
            Value2::String(l) => match other {
                Value2::String(r) => **l == **r,
                _ => None?,
            },
            Value2::Nil => match other {
                Value2::Nil => true,
                _ => None?,
            },
            _ => None?,
        })
    }

    pub fn gt(&self, other: &Value2) -> Option<bool> {
        Some(match self {
            Value2::Integer(l) => match other {
                Value2::Integer(r) => l > r,
                Value2::Double(r) => *l as f64 > *r,
                _ => None?,
            },
            Value2::Double(l) => match other {
                Value2::Integer(r) => *l > *r as f64,
                Value2::Double(r) => l > r,
                _ => None?,
            },
            _ => None?,
        })
    }

    pub fn lt(&self, other: &Value2) -> Option<bool> {
        Some(match self {
            Value2::Integer(l) => match other {
                Value2::Integer(r) => l < r,
                Value2::Double(r) => (*l as f64) < (*r),
                _ => None?,
            },
            Value2::Double(l) => match other {
                Value2::Integer(r) => *l < *r as f64,
                Value2::Double(r) => l < r,
                _ => None?,
            },
            _ => None?,
        })
    }
}

impl Object for Value2 {
    fn trace(&self) {
        match self {
            Value2::Bool(v) => v.trace(),
            Value2::Integer(v) => v.trace(),
            Value2::Double(v) => v.trace(),
            Value2::String(v) => v.trace(),
            Value2::Function(v) => v.trace(),
            Value2::NativeFunction(v) => v.trace(),
            Value2::Object(v) => v.trace(),
            Value2::Nil => {}
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value2::Bool(v) => v.type_name(),
            Value2::Integer(v) => v.type_name(),
            Value2::Double(v) => v.type_name(),
            Value2::String(v) => v.type_name(),
            Value2::Function(v) => v.type_name(),
            Value2::NativeFunction(v) => v.type_name(),
            Value2::Object(v) => v.type_name(),
            Value2::Nil => "nil",
        }
    }
}

impl std::fmt::Display for Value2 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value2::Bool(v) => write!(f, "{}", v),
            Value2::Integer(v) => write!(f, "{}", v),
            Value2::Double(v) => write!(f, "{}", v),
            Value2::String(v) => write!(f, "{}", v),
            Value2::Function(v) => write!(f, "{}", v),
            Value2::NativeFunction(v) => write!(f, "{}", v),
            Value2::Object(v) => write!(f, "{}", v.format()),
            Value2::Nil => write!(f, "nil"),
        }
    }
}

impl Into<bool> for Value2 {
    fn into(self) -> bool {
        match self {
            Value2::Bool(v) => v,
            _ => panic!("could not cast to bool"),
        }
    }
}

impl Into<i64> for Value2 {
    fn into(self) -> i64 {
        match self {
            Value2::Integer(v) => v,
            _ => panic!("could not cast to i64"),
        }
    }
}

impl Into<f64> for Value2 {
    fn into(self) -> f64 {
        match self {
            Value2::Double(v) => v,
            _ => panic!("could not cast to f64"),
        }
    }
}
// }}}

pub struct Frame<'a> {
    name: String,
    ip: ChunkOffset,
    base: ChunkOffset,
    chunk: &'a Chunk,
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
    stack: UniqueRoot<Vec<Value2>>,
    globals: UniqueRoot<FnvHashMap<String, Value2>>,
}

#[derive(Copy, Clone)]
enum VmState {
    Continue,
    Stop(Value2),
}

impl<'a> Machine<'a> {
    pub fn new(heap: &mut Heap, module: &'a Module) -> Self {
        let mut globals = heap.manage_unique(FnvHashMap::default());

        // TODO make this nicer
        globals.insert(
            String::from("print"),
            Value2::NativeFunction({
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
                        Value2::Nil
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

    fn push(&mut self, value: Value2) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value2 {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> &Value2 {
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
        self.push(Value2::String(root.as_gc()));
    }

    pub fn interpret(&mut self, heap: &mut Heap) -> Result<Value2, PiccoloError> {
        // :)
        //let f = heap.manage(Function::new(0, String::new(), 0));
        //self.push(Value2::Function(f.as_gc()));

        self.frames.push(Frame {
            name: String::new(),
            base: 0,
            ip: 0,
            chunk: &self.module.chunk(0),
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
            return Ok(VmState::Stop(Value2::Nil));
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
                    self.push(Value2::Integer(lhs $op rhs));
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
                        self.push(Value2::Double(lhs $op rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        self.push(Value2::Double(lhs $op rhs as f64));
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
                        self.push(Value2::Integer(lhs $op rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        self.push(Value2::Double(lhs as f64 $op rhs));
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
                self.push(Value2::from_constant(c, heap));
            }
            Opcode::Nil => self.push(Value2::Nil),
            Opcode::True => self.push(Value2::Bool(true)),
            Opcode::False => self.push(Value2::Bool(false)),

            Opcode::Negate => {
                let v = self.pop();
                if v.is_double() {
                    let v = v.into::<f64>();
                    self.push(Value2::Double(-v));
                } else if v.is_integer() {
                    let v = v.into::<i64>();
                    self.push(Value2::Integer(-v));
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
                    self.push(Value2::Bool(false));
                } else {
                    self.push(Value2::Bool(true));
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
                self.push(Value2::Bool(a.eq(&b).map_or_else(
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
                self.push(Value2::Bool(lhs.gt(&rhs).map_or_else(
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
                self.push(Value2::Bool(lhs.lt(&rhs).map_or_else(
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
                self.push(Value2::Bool(!lhs.lt(&rhs).map_or_else(
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
                self.push(Value2::Bool(!lhs.gt(&rhs).map_or_else(
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
                if let Value2::Function(f) = self.peek() {
                    let f = self.pop().as_function(); // TODO???
                    self.frames.push(Frame {
                        name: f.name().to_string(),
                        ip: 0,
                        base: (self.stack.len() as u16 - arity - 1) as usize,
                        chunk: self.module.chunk(f.chunk()),
                    })
                } else if let Value2::NativeFunction(f) = self.peek() {
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
