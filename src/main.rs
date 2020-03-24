extern crate piccolo;
extern crate rustyline;

use piccolo::chunk::Chunk;
use piccolo::op::Opcode;
use piccolo::value::Value;

use piccolo::compiler::Compiler;
use piccolo::machine::Machine;
use piccolo::scanner;
use piccolo::scanner::Scanner;
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
                        let tokens = Scanner::new(&line).scan_tokens()?;
                        println!("****** tokens");
                        scanner::print_tokens(&tokens);
                        println!("****** compiler");
                        let chunk = Compiler::compile(Chunk::default(), &tokens)?;
                        println!("****** chunk");
                        chunk.disassemble("line");
                        let mut vm = Machine::new(chunk);
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
        let contents = std::fs::read_to_string(&args[1]).expect("filename doesn't exist");

        #[cfg(feature = "pc-debug")]
        {
            let tokens = Scanner::new(&contents).scan_tokens()?;
            println!("****** tokens");
            scanner::print_tokens(&tokens);
            println!("****** compiler");
            let chunk = Compiler::compile(Chunk::default(), &tokens)?;
            println!("****** chunk");
            chunk.disassemble("line");
            let mut vm = Machine::new(chunk);
            println!("****** result");
            println!("{}", vm.interpret()?);
        }

        #[cfg(not(feature = "pc-debug"))]
        {
            piccolo::interpret(&contents)?;
        }
    }

    Ok(())
}
