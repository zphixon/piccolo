extern crate piccolo;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::{PathBuf, Path};
use piccolo::Constant;

fn main() {
    let args = std::env::args();

    if args.len() == 1 {
        repl();
    } else {
        let args: Vec<String> = args.collect();
        let path = PathBuf::from(&args[1]);
        file(&path);
    }
}

#[cfg(not(feature = "pc-debug"))]
fn file(path: &Path) {
    if let Err(e) = piccolo::do_file(path) {
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
                    if v != Constant::Nil {
                        println!("{:?}", v);
                    }
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
fn file(path: &Path) {
    use piccolo::compiler::ast::AstPrinter;
    use piccolo::{Chunk, Emitter, Machine, Parser, Scanner};

    let contents = std::fs::read_to_string(path).unwrap();

    println!("****** parse");
    match Parser::new().parse(&mut Scanner::new(&contents)) {
        Ok(ast) => {
            println!("****** ast\n{}", AstPrinter::print(&ast));
            match Emitter::new(Chunk::default()).compile(&ast) {
                Ok(chunk) => {
                    println!("****** chunk");
                    chunk.disassemble("");
                    let mut vm = Machine::new(chunk);
                    match vm.interpret() {
                        Ok(value) => println!("{:?}", value),
                        Err(e) => println!("interpret error: {}", e),
                    }
                }
                Err(errors) => {
                    println!("compile error:");
                    for e in errors {
                        println!("{}", e);
                    }
                }
            }
        }
        Err(errors) => {
            println!("parse error:");
            for e in errors {
                println!("{}", e);
            }
        }
    }
}

#[cfg(feature = "pc-debug")]
fn repl() {
    use piccolo::compiler::ast::AstPrinter;
    use piccolo::{Chunk, Emitter, Machine, Parser, Scanner};

    let mut rl = Editor::<()>::new();
    rl.load_history(".piccolo_history")
        .or_else(|_| std::fs::File::create(".piccolo_history").map(|_| ()))
        .unwrap();

    loop {
        match rl.readline("-- ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".piccolo_history").unwrap();

                println!("****** parse");
                match Parser::new().parse(&mut Scanner::new(&line)) {
                    Ok(ast) => {
                        println!("****** ast\n{}", AstPrinter::print(&ast));
                        match Emitter::new(Chunk::default()).compile(&ast) {
                            Ok(chunk) => {
                                println!("****** chunk");
                                chunk.disassemble("");
                                let mut vm = Machine::new(chunk);
                                match vm.interpret() {
                                    Ok(value) => println!("{:?}", value),
                                    Err(e) => println!("interpret error: {}", e),
                                }
                            }
                            Err(errors) => {
                                println!("compile error:");
                                for e in errors {
                                    println!("{}", e);
                                }
                            }
                        }
                    }
                    Err(errors) => {
                        println!("parse error:");
                        for e in errors {
                            println!("{}", e);
                        }
                    }
                }
            }

            Err(ReadlineError::Interrupted) => {}
            _ => break,
        }
    }
}
