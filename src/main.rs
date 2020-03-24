extern crate piccolo;
extern crate rustyline;

use piccolo::chunk::Chunk;
use piccolo::op::Opcode;
use piccolo::value::Value;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() -> piccolo::Result<()> {
    let args = std::env::args();

    if args.len() == 1 {
        let mut rl = Editor::<()>::new();
        loop {
            match rl.readline("-- ") {
                Ok(line) => {
                    #[cfg(feature = "pc-debug")]
                    {
                        let tokens = piccolo::scanner::Scanner::new(&line).scan_tokens()?;
                        println!("****** tokens");
                        piccolo::scanner::print_tokens(&tokens);
                        println!("****** compiler");
                        let chunk =
                            piccolo::compiler::Compiler::compile(Chunk::default(), &tokens)?;
                        println!("****** chunk");
                        chunk.disassemble("line");
                        let mut vm = piccolo::machine::Machine::new(chunk);
                        println!("****** result");
                        println!("{}", vm.interpret()?);
                    }

                    #[cfg(not(feature = "pc-debug"))]
                    {
                        let r = piccolo::interpret(&line);
                        if let Ok(v) = r {
                            println!("{}", v);
                        } else if let Err(e) = r {
                            println!("{}", e);
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {}
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

            let one = c.constant(Value::Double(1.0));
            let two = c.constant(Value::Double(2.0));
            let three = c.constant(Value::Double(3.0));
            let four = c.constant(Value::Double(4.0));
            let five = c.constant(Value::Double(5.0));
            let int = c.constant(Value::Integer(21));
            let s = c.constant(Value::String("pee".into()));

            c.write(Opcode::Constant, 1);
            c.write(int as u8, 1);
            c.write(Opcode::Constant, 1);
            c.write(one as u8, 1);
            c.write(Opcode::Add, 1);
            //c.write(Opcode::Constant, 1);
            //c.write(one as u8, 1);
            //c.write(Opcode::Constant, 1);
            //c.write(int as u8, 1);
            //c.write(Opcode::Add, 1);

            //c.write(Opcode::Constant, 1);
            //c.write(one as u8, 1);
            //c.write(Opcode::Constant, 1);
            //c.write(two as u8, 1);
            //c.write(Opcode::Constant, 1);
            //c.write(three as u8, 1);
            //c.write(Opcode::Multiply, 1);
            //c.write(Opcode::Constant, 1);
            //c.write(four as u8, 1);
            //c.write(Opcode::Constant, 1);
            //c.write(five as u8, 1);
            //c.write(Opcode::Negate, 1);
            //c.write(Opcode::Divide, 1);
            //c.write(Opcode::Subtract, 1);
            //c.write(Opcode::Add, 1);
            //c.write(Opcode::Negate, 1);
            //c.write(Opcode::Return, 1);

            c.write(Opcode::Return, 1);
            c.disassemble("poop");

            let mut vm = piccolo::machine::Machine::new(c);
            println!(" -- interpret --");
            println!("{}", vm.interpret()?);
        }
    }

    Ok(())
}
