use crate::{
    debug,
    error::PiccoloError,
    make_error,
    runtime::{
        interner::{Interner, StringPtr},
        memory::Heap,
        op::Opcode,
        value::{Array, Constant, Value},
        Object,
    },
    trace,
};
use fnv::FnvHashMap;

pub mod builtin;
pub mod compiler;

#[derive(Debug, Default)]
pub struct State {
    pub heap: Heap,
    pub interner: Interner,
    pub stack: Vec<Value>,
    pub globals: FnvHashMap<StringPtr, Value>,
}

impl State {
    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}

pub trait Func: FuncClone {
    fn call(&mut self, state: &mut State) -> Result<(), PiccoloError>;
}

pub trait FuncClone {
    fn clone_func(&self) -> Box<dyn Func>;
}

impl<T: Func + Clone + 'static> FuncClone for T {
    fn clone_func(&self) -> Box<dyn Func> {
        Box::new(self.clone())
    }
}

pub enum Fragment {
    Op(Opcode),
    Func(Box<dyn Func>),
}

impl std::fmt::Debug for Fragment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fragment::Op(op) => write!(f, "{:?}", op),
            Fragment::Func(_) => write!(f, "Func(..)"),
        }
    }
}

#[derive(Default)]
pub struct Program {
    constants: Vec<Constant>,
    fragments: Vec<Fragment>,
}

impl Program {
    pub fn push_func(&mut self, func: Box<dyn Func>) {
        debug!("push func");
        self.fragments.push(Fragment::Func(func));
    }

    pub(crate) fn push_op(&mut self, op: Opcode) {
        debug!("push op {:?}", op);
        self.fragments.push(Fragment::Op(op));
    }

    pub(crate) fn add_constant(&mut self, value: Constant) -> u16 {
        debug!("add constant");
        let index = self.constants.len().try_into().unwrap();
        self.constants.push(value);
        index
    }

    pub(crate) fn push_constant_op(&mut self, value: Constant) {
        debug!("push constant");
        let index = self.add_constant(value);
        self.push_op(Opcode::Constant(index));
    }

    pub(crate) fn get_constant(&self, index: u16) -> &Constant {
        debug!("get constant {index}");
        self.constants.get(index as usize).unwrap()
    }

    pub(crate) fn num_constants(&self) -> usize {
        self.constants.len()
    }

    pub fn run_with(&mut self, state: &mut State) -> Result<(), PiccoloError> {
        let mut i = 0;
        while i < self.fragments.len() {
            trace!(
                "{:?} {:?} {:?}",
                self.fragments[i],
                state.stack,
                state.globals
            );
            match self.fragments[i] {
                Fragment::Op(op) => self.do_op(state, op)?,
                Fragment::Func(ref mut f) => f.call(state)?,
            }

            i += 1;
        }

        Ok(())
    }

    fn do_op(&mut self, state: &mut State, op: Opcode) -> Result<(), PiccoloError> {
        trace!("{op:?}");

        macro_rules! bit_op {
            ($opcode:path, $op:tt) => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_integer() && rhs.is_integer() {
                    let rhs = rhs.into::<i64>();
                    let lhs = lhs.into::<i64>();
                    state.stack.push(Value::Integer(lhs $op rhs));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer".into(),
                        got: format!(
                            "{} {} {}",
                            lhs.type_name2(),
                            stringify!($op),
                            rhs.type_name2(),
                        ),
                    }));
                }
            };
        }

        match op {
            Opcode::GetLocal(index) => {
                let value = state.stack[index as usize].clone();
                state.stack.push(value);
            }

            Opcode::Pop => {
                state.pop();
            }
            Opcode::Constant(index) => {
                state
                    .stack
                    .push(Value::from_constant(&self.constants[index as usize]));
            }
            Opcode::Nil => state.stack.push(Value::Nil),
            Opcode::Bool(b) => state.stack.push(Value::Bool(b)),
            Opcode::Integer(u) => state.stack.push(Value::Integer(u as i64)),

            Opcode::Array(len) => {
                let len = len as usize;
                let mut values = Vec::with_capacity(len);
                for _ in 0..len {
                    values.push(state.pop());
                }
                values.reverse();

                state
                    .stack
                    .push(Value::Object(state.heap.allocate(Array::new_with(values))));
            }

            Opcode::Negate => {
                let v = state.pop();
                if v.is_double() {
                    let v = v.into::<f64>();
                    state.stack.push(Value::Double(-v));
                } else if v.is_integer() {
                    let v = v.into::<i64>();
                    state.stack.push(Value::Integer(-v));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: v.type_name2().to_owned(),
                    }));
                }
            }

            Opcode::Not => {
                let v = state.pop();
                if v.is_truthy() {
                    state.stack.push(Value::Bool(false));
                } else {
                    state.stack.push(Value::Bool(true));
                }
            }

            // binary ops {{{
            Opcode::Add => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs + rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Double(lhs + rhs as f64));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double + {}", rhs.type_name2()),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Integer(lhs.wrapping_add(rhs)));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs as f64 + rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer + {}", rhs.type_name2()),
                        }));
                    }
                } else if lhs.is_string() {
                    let value = format!("{}{}", lhs.format2(), rhs.format2());
                    state
                        .stack
                        .push(Value::String(state.interner.allocate_string(value)));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} + {}", lhs.type_name2(), rhs.type_name2()),
                    }));
                }
            }

            Opcode::Subtract => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs - rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Double(lhs - rhs as f64));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double - {}", rhs.type_name2()),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Integer(lhs.wrapping_sub(rhs)));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs as f64 - rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer - {}", rhs.type_name2()),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} - {}", lhs.type_name2(), rhs.type_name2()),
                    }));
                }
            }

            Opcode::Multiply => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs * rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Double(lhs * rhs as f64));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double * {}", rhs.type_name2()),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Integer(lhs.wrapping_mul(rhs)));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs as f64 * rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer * {}", rhs.type_name2()),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} * {}", lhs.type_name2(), rhs.type_name2()),
                    }));
                }
            }

            Opcode::Divide => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs / rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Double(lhs / rhs as f64));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double / {}", rhs.type_name2()),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        if rhs == 0 {
                            return Err(make_error!(DivideByZero));
                        }
                        state.stack.push(Value::Integer(lhs / rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs as f64 / rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer / {}", rhs.type_name2()),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} / {}", lhs.type_name2(), rhs.type_name2()),
                    }));
                }
            }

            Opcode::Modulo => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_double() {
                    let lhs = lhs.into::<f64>();
                    if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs % rhs));
                    } else if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        state.stack.push(Value::Double(lhs % rhs as f64));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("double % {}", rhs.type_name2()),
                        }));
                    }
                } else if lhs.is_integer() {
                    let lhs = lhs.into::<i64>();
                    if rhs.is_integer() {
                        let rhs = rhs.into::<i64>();
                        if rhs == 0 {
                            return Err(make_error!(DivideByZero));
                        }
                        state.stack.push(Value::Integer(lhs % rhs));
                    } else if rhs.is_double() {
                        let rhs = rhs.into::<f64>();
                        state.stack.push(Value::Double(lhs as f64 % rhs));
                    } else {
                        return Err(make_error!(IncorrectType {
                            exp: "integer or double".into(),
                            got: format!("integer % {}", rhs.type_name2()),
                        }));
                    }
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer or double".into(),
                        got: format!("{} % {}", lhs.type_name2(), rhs.type_name2()),
                    }));
                }
            }
            // }}}

            // comparison {{{
            Opcode::Equal => {
                let a = state.pop();
                let b = state.pop();
                state.stack.push(Value::Bool(a.eq2(b)?));
            }
            Opcode::Greater => {
                let rhs = state.pop();
                let lhs = state.pop();
                state.stack.push(Value::Bool(lhs.gt2(rhs)?));
            }
            Opcode::Less => {
                let rhs = state.pop();
                let lhs = state.pop();
                state.stack.push(Value::Bool(lhs.lt2(rhs)?));
            }
            Opcode::GreaterEqual => {
                let rhs = state.pop();
                let lhs = state.pop();
                state.stack.push(Value::Bool(!lhs.lt2(rhs)?));
            }
            Opcode::LessEqual => {
                let rhs = state.pop();
                let lhs = state.pop();
                state.stack.push(Value::Bool(!lhs.gt2(rhs)?));
            } // }}}

            Opcode::GetGlobal(index) => {
                let constant = self.get_constant(index);
                let ptr = constant.string_ptr();

                if state.globals.contains_key(&ptr) {
                    state.stack.push(state.globals[&ptr]);
                } else {
                    return Err(make_error!(UndefinedVariable {
                        name: state.interner.get_string(ptr).to_string(),
                    }));
                }
            }
            Opcode::SetGlobal(index) => {
                let constant = self.get_constant(index);
                let ptr = constant.string_ptr();

                let value = state.pop();
                if state.globals.insert(ptr, value).is_none() {
                    return Err(make_error!(UndefinedVariable {
                        name: state.interner.get_string(ptr).to_string(),
                    }));
                }
            }
            Opcode::DeclareGlobal(index) => {
                let constant = self.get_constant(index);
                let ptr = constant.string_ptr();

                let value = state.pop();
                state.globals.insert(ptr, value);
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
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_integer() && rhs.is_integer() {
                    let rhs_i64 = rhs.into::<i64>();
                    let rhs = match rhs_i64.try_into() {
                        Ok(rhs) => rhs,
                        Err(_) => {
                            return Err(make_error!(InvalidShift { value: rhs_i64 }));
                        }
                    };

                    let lhs = lhs.into::<i64>();
                    state.stack.push(Value::Integer(lhs.wrapping_shl(rhs)));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer".into(),
                        got: format!("{} << {}", lhs.type_name2(), rhs.type_name2(),),
                    }));
                }
            }

            Opcode::ShiftRight => {
                let rhs = state.pop();
                let lhs = state.pop();
                if lhs.is_integer() && rhs.is_integer() {
                    let rhs_i64 = rhs.into::<i64>();
                    let rhs = match rhs_i64.try_into() {
                        Ok(rhs) => rhs,
                        Err(_) => {
                            return Err(make_error!(InvalidShift { value: rhs_i64 }));
                        }
                    };

                    let lhs = lhs.into::<i64>();
                    state.stack.push(Value::Integer(lhs.wrapping_shr(rhs)));
                } else {
                    return Err(make_error!(IncorrectType {
                        exp: "integer".into(),
                        got: format!("{} << {}", lhs.type_name2(), rhs.type_name2()),
                    }));
                }
            }

            Opcode::Assert(index) => {
                let v = state.pop();
                let assertion = self.get_constant(index);
                if !v.is_truthy() {
                    let ptr = assertion.string_ptr();
                    let assertion = state.interner.get_string(ptr).to_owned();
                    return Err(make_error!(AssertFailed { assertion }));
                }
            }

            _ => todo!("{op:?}"),
        }

        Ok(())
    }
}
