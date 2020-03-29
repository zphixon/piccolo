extern crate piccolo;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() -> piccolo::Result<()> {
    let args = std::env::args();

    if args.len() == 1 {
        let mut rl = Editor::<()>::new();
        rl.load_history(".piccolo_history")
            .or_else(|_| std::fs::File::create(".piccolo_history").map(|_| ()))
            .unwrap();

        loop {
            match rl.readline("-- ") {
                Ok(line) => {
                    rl.add_history_entry(&line);

                    #[cfg(feature = "pc-debug")]
                    {
                        use piccolo::Chunk;
                        use piccolo::Compiler;
                        use piccolo::Machine;
                        use piccolo::Scanner;

                        let tokens = Scanner::new(&line).scan_tokens();
                        if let Ok(tokens) = tokens {
                            println!("****** tokens");
                            piccolo::print_tokens(&tokens);
                            println!("****** compiler");
                            let chunk = Compiler::compile(Chunk::default(), &tokens);
                            if let Ok(chunk) = chunk {
                                println!("****** chunk");
                                chunk.disassemble("line");
                                let mut vm = Machine::new(chunk);
                                println!("****** result");
                                println!("{:#?}", vm.interpret());
                            } else {
                                println!("{}", chunk.err().unwrap());
                            }
                        } else {
                            println!("{}", tokens.err().unwrap());
                        }
                    }

                    #[cfg(not(feature = "pc-debug"))]
                    {
                        let r = piccolo::interpret(&line);
                        if let Ok(v) = r {
                            println!("{:?}", v);
                        } else if let Err(e) = r {
                            println!("{}", e);
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {}
                _ => break,
            }
        }
        rl.save_history(".piccolo_history").unwrap();
    } else {
        let args: Vec<String> = args.collect();
        let contents = std::fs::read_to_string(&args[1]).expect("filename doesn't exist");

        #[cfg(feature = "pc-debug")]
        {
            use piccolo::Chunk;
            use piccolo::Compiler;
            use piccolo::Machine;
            use piccolo::Scanner;

            let tokens = Scanner::new(&contents).scan_tokens()?;
            println!("****** tokens");
            piccolo::print_tokens(&tokens);
            println!("****** compiler");
            let chunk = Compiler::compile(Chunk::default(), &tokens)?;
            println!("****** chunk");
            chunk.disassemble("file");
            let mut vm = Machine::new(chunk);
            println!("****** result");
            println!("{}", vm.interpret()?.fmt(vm.heap()));
        }

        #[cfg(not(feature = "pc-debug"))]
        {
            piccolo::interpret(&contents)?;
        }
    }

    Ok(())
}
