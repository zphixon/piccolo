use crate::{
    error::{ErrorKind, PiccoloError},
    runtime::{builtin, chunk::Module, memory2::Object, op::Opcode, Heap, Value},
    trace,
};
use fnv::FnvHashMap;
use std::hash::BuildHasherDefault;

pub struct Vm {
    stack: Vec<Value>,
    ip: usize,
    current_chunk: usize,
    globals: FnvHashMap<String, Value>,
}

impl Vm {
    pub fn new(heap: &Heap) -> Self {
        let mut vm = Vm {
            stack: Vec::new(),
            ip: 0,
            current_chunk: 0,
            globals: FnvHashMap::with_capacity_and_hasher(0, BuildHasherDefault::default()),
        };

        vm.globals.insert(
            String::from("print"),
            Value::NativeFunction(heap.get_native("print").unwrap()),
        );
        vm.globals.insert(
            String::from("rand"),
            Value::NativeFunction(heap.get_native("rand").unwrap()),
        );
        vm.globals.insert(
            String::from("toString"),
            Value::NativeFunction(heap.get_native("toString").unwrap()),
        );

        vm
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    pub fn step(&mut self, heap: &mut Heap, program: &Module) -> Result<(), PiccoloError> {
        let op = program.chunk(self.current_chunk).ops[self.ip];
        let stack_str = {
            let mut s = String::new();
            for item in self.stack.iter() {
                s.push('[');
                s.push_str(&item.debug_format(heap));
                s.push_str("] ");
            }
            s
        };
        trace!("{op:?} {stack_str}");

        match op {
            Opcode::Pop => {
                self.pop();
            }

            //Opcode::Return => {}
            Opcode::Constant(idx) => {
                self.stack.push(Value::from_constant(
                    program.get_constant(idx).clone(),
                    heap,
                ));
            }

            //Opcode::Nil => {}
            //Opcode::True => {}
            //Opcode::False => {}
            //Opcode::Negate => {}
            //Opcode::Not => {}
            //Opcode::Add => {}
            //Opcode::Subtract => {}
            //Opcode::Multiply => {}
            //Opcode::Divide => {}
            //Opcode::Modulo => {}
            //Opcode::Equal => {}
            //Opcode::Greater => {}
            //Opcode::Less => {}
            //Opcode::GreaterEqual => {}
            //Opcode::LessEqual => {}
            //Opcode::GetLocal(idx) => {}
            //Opcode::SetLocal(idx) => {}
            Opcode::GetGlobal(idx) => {
                let name = program.get_constant(idx).ref_string();
                if self.globals.contains_key(name) {
                    self.stack.push(self.globals[name]);
                } else {
                    return Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                        name: name.to_string(),
                    })
                    .pos(
                        program
                            .chunk(self.current_chunk)
                            .get_pos_from_index(self.ip),
                    ));
                }
            }

            //Opcode::SetGlobal(idx) => {}
            Opcode::DeclareGlobal(idx) => {
                let name = program.get_constant(idx).to_string();
                let value = self.pop();
                self.globals.insert(name, value);
            }

            //Opcode::JumpForward(offset) => {}
            //Opcode::JumpFalse(offset) => {}
            //Opcode::JumpTrue(offset) => {}
            //Opcode::JumpBack(offset) => {}
            //Opcode::BitAnd => {}
            //Opcode::BitOr => {}
            //Opcode::BitXor => {}
            //Opcode::ShiftLeft => {}
            //Opcode::ShiftRight => {}
            Opcode::Call(num_args) => {
                trace!("num_args={num_args}");
                let num_args = num_args as usize;
                let mut args = Vec::with_capacity(num_args);
                for _ in 0..num_args {
                    let arg = self.pop();
                    trace!("arg={}", arg.debug_format(heap));
                    args.push(arg);
                }
                let func = self.pop();
                trace!("func={}", func.debug_format(heap));
                match func {
                    Value::Function(func) => if func.arity.is_compatible(num_args) {},

                    Value::NativeFunction(func) => {
                        self.stack.push(func.call(heap, &args)?);
                    }

                    Value::Object(ptr) => {
                        self.stack.push(heap.get(ptr).unwrap().call(heap, &args)?);
                    }

                    v => {
                        todo!("{}", v.type_name(heap));
                    }
                }
            }

            //Opcode::Assert(idx) => {}
            _ => todo!(),
        }

        self.ip += 1;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        #[cfg(feature = "log")]
        my_log::init();

        let module = crate::compiler::compile_chunk(r#"fn x() do print("x") end x()"#).unwrap();
        crate::debug!("{}", crate::runtime::chunk::disassemble(&module, ""));
        let mut heap = Heap::new();
        let mut vm = Vm::new(&mut heap);
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        vm.step(&mut heap, &module).unwrap();
        panic!();
    }
}
