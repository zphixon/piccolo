
extern crate piccolo;
extern crate rustyline;

use piccolo::chunk::Chunk;
use piccolo::value::Value;
use piccolo::op::Opcode;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() -> piccolo::Result<()> {
    let mut args = std::env::args();

    if args.len() == 1 {
        let mut rl = Editor::<()>::new();
        loop {
            match rl.readline("-- ") {
                Ok(line) => {},
                Err(ReadlineError::Interrupted) => {},
                _ => break,
            }
        }
    } else {
        let args: Vec<String> = args.collect();
        if !args.iter().any(|a| &*a == "pingus") {
            let contents = std::fs::read_to_string(&args[1]).expect("filename doesn't exist");
            piccolo::interpret(&contents)?;
        } else {
            let mut c = Chunk::default();

            let one = c.constant(Value(1.0));
            let two = c.constant(Value(2.0));
            let three = c.constant(Value(3.0));
            let four = c.constant(Value(4.0));
            let five = c.constant(Value(5.0));

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

            c.disassemble("poop");

            let mut vm = piccolo::machine::Machine::new(c);
            println!(" -- interpret --");
            vm.interpret()?;
        }
    }

    Ok(())
}
