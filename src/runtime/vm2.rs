use crate::runtime::{object::Function, ChunkOffset};
use crate::{Chunk, Constant, ErrorKind, Gc, Heap, Line, Object, PiccoloError, Root, UniqueRoot};

use super::op::Opcode;

use fnv::FnvHashMap;

// value2 {{{
#[derive(Clone, Debug)]
pub enum Value2 {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(Gc<String>),
    Function(Gc<Function>),
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

    pub fn type_name(&self) -> &'static str {
        match self {
            Value2::Bool(_) => "bool",
            Value2::Integer(_) => "integer",
            Value2::Double(_) => "double",
            Value2::String(_) => "string",
            Value2::Function(_) => "fn",
            Value2::Nil => "nil",
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
            Value2::Bool(_) => {}
            Value2::Integer(_) => {}
            Value2::Double(_) => {}
            Value2::String(v) => v.trace(),
            Value2::Function(v) => v.trace(),
            Value2::Nil => {}
        }
    }
}
// }}}

#[derive(Default)]
struct Module {
    chunks: Vec<Chunk>,
    constants: Vec<Constant>,
}

struct Frame<'a> {
    pc: ChunkOffset,
    base: ChunkOffset,
    chunk: &'a Chunk,
}

impl Frame<'_> {
    fn step(&mut self) -> Opcode {
        let op = self.chunk.data[self.pc].into();
        self.pc += 1;
        op
    }
}

pub struct Vm2<'a> {
    module: &'a Module,
    frames: Vec<Frame<'a>>,
    stack: UniqueRoot<Vec<Value2>>,
    globals: UniqueRoot<FnvHashMap<String, Value2>>,
}

#[derive(PartialEq, Copy, Clone)]
enum VmState {
    Continue,
    Stop,
}

impl<'a> Vm2<'a> {
    fn new(heap: &mut Heap, module: &'a Module) -> Self {
        Vm2 {
            module,
            frames: Vec::new(),
            stack: heap.manage_unique(Vec::new()),
            globals: heap.manage_unique(FnvHashMap::default()),
        }
    }

    fn push(&mut self, value: Value2) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value2 {
        self.stack.pop().unwrap()
    }

    fn interpret(&mut self, heap: &mut Heap) -> Result<(), PiccoloError> {
        trace!("huh1");
        let f = heap.manage(Function::new(0, String::new(), 0));
        self.push(Value2::Function(f.as_gc()));

        self.frames.push(Frame {
            base: 0,
            pc: 0,
            chunk: &self.module.chunks[0],
        });

        trace!("huh2");
        while self
            .interpret_next_instruction()
            .map_err(|e| e.line(self.current_line()))?
            == VmState::Continue
        {}

        Ok(())
    }

    fn interpret_next_instruction(&mut self) -> Result<VmState, PiccoloError> {
        trace!("huh3");
        // debug/macros {{{
        let taack: &Vec<Value2> = &*self.stack;
        debug!(
            " ┌─{}{:?}",
            if self.current_pc() + 1 == self.current_chunk().len() {
                "─vm─exit─ "
            } else {
                " "
            },
            taack,
        );
        trace!("huh4");
        debug!(
            " └─{} {}",
            if self.current_pc() + 1 == self.current_chunk().len() {
                "───────── "
            } else {
                " "
            },
            self.current_chunk()
                .disassemble_instruction(self.current_pc())
        );

        //macro_rules! bit_op {
        //    ($opcode:path, $op:tt) => {
        //        let rhs = self.pop()?;
        //        let lhs = self.pop()?;
        //        if lhs.is_integer() && rhs.is_integer() {
        //            let rhs = rhs.into::<i64>();
        //            let lhs = lhs.into::<i64>();
        //            self.push(Value::Integer(lhs $op rhs));
        //        } else {
        //            return Err(PiccoloError::new(ErrorKind::IncorrectType {
        //                exp: "integer".into(),
        //                got: format!(
        //                    "{} {} {}",
        //                    lhs.type_name(),
        //                    stringify!($op),
        //                    rhs.type_name(),
        //                ),
        //                op: $opcode,
        //            }));
        //        }
        //    };
        //}

        //// boolean argument to enable/disable string concatenation
        //macro_rules! bin_op {
        //    ($opcode:path, $op:tt, nostring) => {
        //        bin_op!($opcode, $op, false)
        //    };
        //    ($opcode:path, $op:tt, string) => {
        //        bin_op!($opcode, $op, true)
        //    };
        //    ($opcode:path, $op:tt, $allow_string:tt) => {
        //        let rhs = self.pop()?;
        //        let lhs = self.pop()?;
        //        if lhs.is_double() {
        //            let lhs = lhs.into::<f64>();
        //            if rhs.is_double() {
        //                let rhs = rhs.into::<f64>();
        //                self.push(Value::Double(lhs $op rhs));
        //            } else if rhs.is_integer() {
        //                let rhs = rhs.into::<i64>();
        //                self.push(Value::Double(lhs $op rhs as f64));
        //            } else {
        //                return Err(PiccoloError::new(ErrorKind::IncorrectType {
        //                    exp: "integer or double".into(),
        //                    got: format!("double {} {}", stringify!($op), rhs.type_name()),
        //                    op: $opcode,
        //                }));
        //            }
        //        } else if lhs.is_integer() {
        //            let lhs = lhs.into::<i64>();
        //            if rhs.is_integer() {
        //                let rhs = rhs.into::<i64>();
        //                self.push(Value::Integer(lhs $op rhs));
        //            } else if rhs.is_double() {
        //                let rhs = rhs.into::<f64>();
        //                self.push(Value::Double(lhs as f64 $op rhs));
        //            } else {
        //                return Err(PiccoloError::new(ErrorKind::IncorrectType {
        //                    exp: "integer or double".into(),
        //                    got: format!("integer {} {}", stringify!($op), rhs.type_name()),
        //                    op: $opcode,
        //                }));
        //            }
        //        } else if $allow_string && lhs.is_string() {
        //            let value = format!("{}{}", &lhs, &rhs);
        //            self.stack.push(Value::String(value));
        //        } else {
        //            return Err(PiccoloError::new(ErrorKind::IncorrectType {
        //                exp: "integer or double".into(),
        //                got: format!("{} {} {}", lhs.type_name(), stringify!($op), rhs.type_name()),
        //                op: $opcode,
        //            }));
        //        }
        //    };
        //}
        // }}}

        let instr = self.current_frame_mut().step();

        Ok(VmState::Continue)
    }

    fn current_line(&self) -> Line {
        self.current_chunk().get_line_from_index(self.current_pc())
    }

    fn current_pc(&self) -> ChunkOffset {
        self.current_frame().pc
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
        env_logger::init();

        let src = "11*3+\"heehee\"";
        let ast = crate::compiler::parser::parse(&mut crate::compiler::scanner::Scanner::new(src))
            .expect("parse");
        let mut emitter = crate::compiler::emitter::Emitter::new();
        crate::compiler::emitter::compile_ast(&mut emitter, &ast).expect("emit");
        let chunk = emitter.into_chunk();

        let mut module = Module::default();
        module.constants = chunk.constants.clone();
        module.chunks.push(chunk);

        let mut heap = crate::runtime::memory::Heap::default();

        let mut vm = Vm2::new(&mut heap, &module);
        vm.interpret(&mut heap).unwrap();
    }
}
