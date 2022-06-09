use {
    gumdrop::Options,
    piccolo::prelude::*,
    rustyline::{error::ReadlineError, Editor},
};

#[derive(Options)]
struct Args {
    #[options(help = "Print this message.", short = "h")]
    help: bool,

    #[options(free, help = "File to run. If no file is given, opens a REPL.")]
    filename: Option<String>,

    #[options(
        help = "Show the tokens scanned from the file or each line of REPL input.",
        short = "t"
    )]
    print_tokens: bool,

    #[options(
        help = "Show the AST parsed from the file or each line of REPL input.",
        short = "a"
    )]
    print_ast: bool,

    #[options(
        help = "Show compiled bytecode from the file or each line of REPL input.",
        short = "c"
    )]
    print_compiled: bool,

    #[options(
        help = "Quit after parsing and compiling a file. Requires filename.",
        short = "v"
    )]
    verify_syntax: bool,

    #[options(
        help = "Continue an interactive session in the repl after running a file. Overrides -v/--verify-syntax.",
        short = "i"
    )]
    interactive: bool,

    #[options(help = "Run string argument. Conflicts with filename.", short = "e")]
    string: Option<String>,
}

fn main() -> Result<(), Vec<PiccoloError>> {
    #[cfg(feature = "pc-debug")]
    env_logger::init();

    let args = Args::parse_args_default_or_exit();
    if args.string.is_some() && args.filename.is_some() {
        println!("Cannot use both -e/--string and filename.");
        return Ok(());
    }

    if args.verify_syntax && args.filename.is_none() {
        println!("Cannot verify syntax without a filename.");
        return Ok(());
    }

    if let Some((emitter, heap, vm)) = if let Some(filename) = args.filename.as_ref() {
        let source = std::fs::read_to_string(filename).map_err(|e| vec![PiccoloError::from(e)])?;
        maybe_exec(&args, &source, &filename)?
    } else if let Some(string) = args.string.as_ref() {
        maybe_exec(&args, &string, "-e/--string")?
    } else {
        Some((None, None, None))
    } {
        // filename   string   interactive   result
        // x          x        *             not possible
        // x                                 exit
        //            x                      exit
        // x                   x             repl
        //            x        x             repl
        //                     *             repl
        if args.filename.is_some() && args.interactive
            || args.string.is_some() && args.interactive
            || args.filename.is_none() && args.string.is_none()
        {
            repl(&args, emitter, heap, vm)?;
        }
    }

    Ok(())
}

fn maybe_exec<'source, 'heap>(
    args: &Args,
    source: &'source str,
    name: &str,
) -> Result<Option<(Option<Emitter>, Option<Heap<'heap>>, Option<Machine<'heap>>)>, Vec<PiccoloError>>
{
    let mut scanner = Scanner::new(source);

    if args.print_tokens {
        println!("=== tokens ===");
        let tokens = scanner.scan_all()?;
        piccolo::debug::print_tokens(&tokens);
        scanner = Scanner::new(&source);
    }

    let ast = piccolo::compiler::parser::parse(&mut scanner)?;

    if args.print_ast {
        println!("=== ast ===\n{}", piccolo::debug::print_ast(&ast));
    }

    let mut emitter = Emitter::new();
    piccolo::compiler::emitter::compile_with(&mut emitter, &ast)?;

    if args.print_compiled {
        println!(
            "=== module ===\n{}",
            piccolo::runtime::chunk::disassemble(emitter.module(), name)
        );
    }

    if args.verify_syntax {
        return Ok(None);
    }

    let mut heap = Heap::default();
    let mut vm = Machine::new(&mut heap);

    vm.interpret(&mut heap, emitter.module())?;

    Ok(Some((Some(emitter), Some(heap), Some(vm))))
}

fn print_errors(errors: Vec<PiccoloError>) {
    if errors.len() == 1 {
        println!("Error {}", errors[0])
    } else {
        println!("{} Errors:", errors.len());
        for e in errors.iter() {
            println!("    {}", e);
        }
    }
}

fn repl<'heap>(
    args: &Args,
    emitter: Option<Emitter>,
    heap: Option<Heap<'heap>>,
    machine: Option<Machine<'heap>>,
) -> Result<(), Vec<PiccoloError>> {
    use piccolo::debug::*;

    let mut rl = Editor::<()>::new();
    rl.load_history(".piccolo_history")
        .or_else(|_| std::fs::File::create(".piccolo_history").map(|_| ()))
        .unwrap();

    let mut heap = heap.unwrap_or_else(Heap::default);
    let mut machine = machine.unwrap_or_else(|| Machine::new(&mut heap));
    let mut emitter = emitter.unwrap_or_else(Emitter::new);

    let mut input = String::new();
    let mut prompt = "-- ";

    loop {
        match rl.readline(prompt) {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".piccolo_history")
                    .expect("cannot open .piccolo_history");

                input.push_str(&line);
                input.push('\n');

                let mut scanner = Scanner::new(&input);

                if args.print_tokens {
                    println!("=== tokens ===");
                    let tokens = scanner.scan_all().map_err(|e| print_errors(vec![e]));
                    if let Ok(tokens) = tokens {
                        piccolo::debug::print_tokens(&tokens);
                    }
                    scanner = Scanner::new(&input);
                }

                let parse = parse(&mut scanner);
                match parse {
                    Ok(ast) => {
                        if args.print_ast {
                            println!("=== ast ===\n{}", print_ast(&ast));
                        }

                        let _: Result<(), ()> = compile_with(&mut emitter, &ast)
                            .and_then(|_| {
                                if args.print_compiled {
                                    println!(
                                        "=== module ===\n{}",
                                        disassemble(emitter.module(), "")
                                    );
                                }
                                machine
                                    .interpret_continue(&mut heap, emitter.module())
                                    .map_err(|e| vec![e])
                            })
                            .map_err(print_errors)
                            .map(|value| println!("{:?}", value));

                        prompt = "-- ";
                        input = String::new();
                    }

                    Err(errors) => {
                        if errors.iter().any(|error| !error.was_eof()) {
                            prompt = "-- ";
                            input = String::new();
                            print_errors(errors);
                        } else {
                            prompt = "---- ";
                            // continue
                        }
                    }
                }
            }

            Err(ReadlineError::Interrupted) => {}
            _ => break Ok(()),
        }
    }
}
