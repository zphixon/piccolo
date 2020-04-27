extern crate env_logger;
extern crate piccolo;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::path::{Path, PathBuf};

fn main() {
    #[cfg(feature = "pc-debug")]
    env_logger::init();

    let args = std::env::args();

    if args.len() == 1 {
        repl();
    } else {
        let args: Vec<String> = args.collect();
        let path = PathBuf::from(&args[1]);
        file(&path);
    }
}

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

fn repl() {
    use piccolo::Constant;
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
