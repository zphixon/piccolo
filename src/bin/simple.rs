#[cfg(feature = "cli")]
mod simple {
    use rustyline::Editor;

    pub fn main() {
        let args = std::env::args().collect::<Vec<_>>();
        if args.len() > 1 {
            let src = std::fs::read_to_string(&args[1]).unwrap();
            match piccolo::interpret(&src) {
                Ok((env, value)) => println!("{}", env.format(value)),
                Err(errors) => {
                    for error in errors {
                        println!("Error: {error}");
                    }
                }
            }

            return;
        }

        let mut rl = Editor::<()>::new();
        const PROMPT: &str = "> ";

        while let Ok(line) = rl.readline(PROMPT) {
            match piccolo::interpret(&line) {
                Ok((env, value)) => println!("{}", env.format(value)),
                Err(errors) => {
                    for error in errors {
                        println!("Error: {error}");
                    }
                }
            }
        }
    }
}

#[cfg(feature = "cli")]
fn main() {
    simple::main();
}

#[cfg(not(feature = "cli"))]
fn main() {
    panic!("Requires --features cli");
}
