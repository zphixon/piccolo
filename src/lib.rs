extern crate anyhow;
extern crate num_enum;
extern crate thiserror;

pub mod chunk;
pub mod compiler;
pub mod error;
pub mod machine;
pub mod op;
pub mod scanner;
pub mod value;

pub use anyhow::Result;

pub fn interpret(src: &str) -> Result<value::Value> {
    use chunk::Chunk;
    use compiler::Compiler;
    use machine::Machine;
    use scanner::Scanner;

    Machine::new(Compiler::compile(
        Chunk::default(),
        &Scanner::new(src).scan_tokens()?,
    )?)
    .interpret()
}

#[cfg(test)]
mod tests {
    use crate::chunk::Chunk;
    use crate::machine::Machine;
    use crate::op::Opcode;
    use crate::value::Value;
    use crate::compiler::Precedence;

    #[test]
    fn concat() {
        let mut c = Chunk::default();
        let s1 = c.constant(Value::String("ye".into()));
        let s2 = c.constant(Value::String("et".into()));

        c.write(Opcode::Constant, 1);
        c.write(s1 as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(s2 as u8, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().unwrap();
    }

    #[test]
    fn precedence_ord() {
        assert!(Precedence::And > Precedence::Or);
    }

    #[test]
    fn math_multiply_add() {
        let mut c = Chunk::default();
        let one = c.constant(Value::Double(1.0));
        let two = c.constant(Value::Double(2.0));
        let three = c.constant(Value::Double(3.0));

        // 1 * 2 + 3

        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Multiply, 1);
        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret();
    }

    #[test]
    fn math_add_multiply() {
        let mut c = Chunk::default();
        let one = c.constant(Value::Double(1.0));
        let two = c.constant(Value::Double(2.0));
        let three = c.constant(Value::Double(3.0));

        // 1 + 2 * 3

        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Multiply, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().expect("not ok :(");
    }

    #[test]
    fn math_sub_sub() {
        let mut c = Chunk::default();
        let one = c.constant(Value::Double(1.0));
        let two = c.constant(Value::Double(2.0));
        let three = c.constant(Value::Double(3.0));

        // 3 - 2 - 1

        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Subtract, 1);
        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Subtract, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().expect("not ok :(");
    }

    #[test]
    fn math_complex() {
        let mut c = Chunk::default();
        let one = c.constant(Value::Double(1.0));
        let two = c.constant(Value::Double(2.0));
        let three = c.constant(Value::Double(3.0));
        let four = c.constant(Value::Double(4.0));
        let five = c.constant(Value::Double(5.0));

        // 1 + 2 * 3 - 4 / -5

        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Multiply, 1);
        c.write(Opcode::Constant, 1);
        c.write(four as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(five as u8, 1);
        c.write(Opcode::Negate, 1);
        c.write(Opcode::Divide, 1);
        c.write(Opcode::Subtract, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().expect("not ok :(");
    }

    #[test]
    fn get_line_from_index() {
        let mut c = Chunk::default();
        c.write(Opcode::Return, 1); // 0
        c.write(Opcode::Return, 1); // 1
        c.write(Opcode::Return, 1); // 2
        c.write(Opcode::Return, 1); // 3
        c.write(Opcode::Return, 1); // 4
        c.write(Opcode::Return, 1); // 5
        c.write(Opcode::Return, 2); // 6
        c.write(Opcode::Return, 2); // 7
        c.write(Opcode::Return, 2); // 8
        c.write(Opcode::Return, 2); // 9
        c.write(Opcode::Return, 2); // 10
        c.write(Opcode::Return, 3); // 11
        c.write(Opcode::Return, 3); // 12
        c.write(Opcode::Return, 3); // 13
        c.write(Opcode::Return, 3); // 14
        c.write(Opcode::Return, 4); // 15
        c.write(Opcode::Return, 4); // 16
        c.write(Opcode::Return, 4); // 17
        c.write(Opcode::Return, 4); // 18
        c.write(Opcode::Return, 5); // 19

        assert_eq!(c.get_line_from_index(0), 1);
        assert_eq!(c.get_line_from_index(5), 1);
        assert_eq!(c.get_line_from_index(6), 2);
        assert_eq!(c.get_line_from_index(10), 2);
        assert_eq!(c.get_line_from_index(11), 3);
        assert_eq!(c.get_line_from_index(14), 3);
    }
}
