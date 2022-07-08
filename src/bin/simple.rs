use rustyline::Editor;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() > 1 {
        let src = std::fs::read_to_string(&args[1]).unwrap();
        match piccolo::interpret(&src) {
            Ok(constant) => println!("{constant}"),
            Err(errors) => {
                for error in errors {
                    println!("Error: {error}");
                }
            }
        }

        return;
    }

    let mut rl = Editor::<()>::new();
    const PROMPT: &'static str = "> ";

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => match piccolo::interpret(&line) {
                Ok(constant) => println!("{constant}"),
                Err(errors) => {
                    for error in errors {
                        println!("Error: {error}");
                    }
                }
            },
            Err(_) => break,
        }
    }
}
