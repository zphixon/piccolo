use rustyline::Editor;

fn main() {
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
