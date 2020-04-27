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

fn print_errors(errors: Vec<piccolo::PiccoloError>) {
    if errors.len() == 1 {
        println!("Error {}", errors[0])
    } else {
        println!("{} Errors:", errors.len());
        for e in errors.iter() {
            println!("    {}", e);
        }
    }
}

fn file(path: &Path) {
    if let Err(errors) = piccolo::do_file(path) {
        print_errors(errors);
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

                match piccolo::interpret(&line) {
                    Ok(v) => {
                        if v != Constant::Nil {
                            println!("{:?}", v);
                        }
                    }
                    Err(errors) => print_errors(errors),
                }
            }

            Err(ReadlineError::Interrupted) => {}
            _ => break,
        }
    }
    rl.save_history(".piccolo_history").unwrap();
}
