extern crate piccolo;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let args = std::env::args();

    if args.len() == 1 {
        repl();
    } else {
        let args: Vec<String> = args.collect();
        let contents = std::fs::read_to_string(&args[1]).expect("filename doesn't exist");
        file(&contents);
    }
}

#[cfg(not(feature = "pc-debug"))]
fn file(contents: &str) {
    if let Err(e) = piccolo::interpret(contents) {
        if e.len() == 1 {
            println!("Error {}", e[0])
        } else {
            println!("{} Errors:", e.len());
            for e in e.iter() {
                println!("    {}", e);
            }
        }
    }
}

#[cfg(not(feature = "pc-debug"))]
fn repl() {
    let mut rl = Editor::<()>::new();
    rl.load_history(".piccolo_history")
        .or_else(|_| std::fs::File::create(".piccolo_history").map(|_| ()))
        .unwrap();

    loop {
        match rl.readline("-- ") {
            Ok(line) => {
                rl.add_history_entry(&line);

                let r = piccolo::interpret(&line);
                if let Ok(v) = r {
                    println!("{:?}", v);
                } else if let Err(e) = r {
                    if e.len() == 1 {
                        println!("Error {}", e[0])
                    } else {
                        println!("{} Errors:", e.len());
                        for e in e.iter() {
                            println!("    {}", e);
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {}
            _ => break,
        }
    }
    rl.save_history(".piccolo_history").unwrap();
}

#[cfg(feature = "pc-debug")]
fn file(contents: &str) {
    use piccolo::Chunk;
    use piccolo::Compiler;
    use piccolo::Machine;
    use piccolo::Scanner;

    let tokens = Scanner::new(contents).scan_tokens();
    println!("****** tokens");
    if let Ok(tokens) = tokens {
        piccolo::print_tokens(&tokens);
        println!("****** compiler");
        let chunk = Compiler::compile(Chunk::default(), &tokens);
        if let Ok(chunk) = chunk {
            println!("****** chunk");
            chunk.disassemble("file");
            let mut vm = Machine::new(chunk);
            println!("****** result");
            let result = vm.interpret();
            if let Err(result) = result {
                println!("{}", result);
            } else {
                println!("****** ok");
            }
        } else {
            let _: Vec<_> = chunk
                .unwrap_err()
                .iter()
                .map(|err| {
                    println!("{}", err);
                })
                .collect();
        }
    } else {
        let _: Vec<_> = tokens
            .unwrap_err()
            .iter()
            .map(|err| {
                println!("{}", err);
            })
            .collect();
    }
}

#[cfg(feature = "pc-debug")]
fn repl() {
    use piccolo::Chunk;
    use piccolo::Compiler;
    use piccolo::Machine;
    use piccolo::Scanner;

    let mut rl = Editor::<()>::new();
    rl.load_history(".piccolo_history")
        .or_else(|_| std::fs::File::create(".piccolo_history").map(|_| ()))
        .unwrap();

    loop {
        match rl.readline("-- ") {
            Ok(line) => {
                rl.add_history_entry(&line);

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
                        println!("{:?}", vm.interpret());
                    } else {
                        let e = chunk.err().unwrap();
                        if e.len() == 1 {
                            println!("Error {}", e[0])
                        } else {
                            println!("{} Errors:", e.len());
                            for e in e.iter() {
                                println!("    {}", e);
                            }
                        }
                    }
                } else {
                    let e = tokens.err().unwrap();
                    if e.len() == 1 {
                        println!("Error {}", e[0])
                    } else {
                        println!("{} Errors:", e.len());
                        for e in e.iter() {
                            println!("    {}", e);
                        }
                    }
                }
            }

            Err(ReadlineError::Interrupted) => {}
            _ => break,
        }
    }
    rl.save_history(".piccolo_history").unwrap();
}
