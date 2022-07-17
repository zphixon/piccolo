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
    } else {
        repl();
    }
}

#[cfg(not(features = "cli"))]
fn repl() {
    panic!("simple repl requires --features cli");
}

#[cfg(features = "cli")]
fn repl() {
    use rustyline::Editor;
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
