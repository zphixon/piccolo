use gumdrop::Options;
use piccolo::{
    compiler::{
        self,
        emitter::{self, Emitter},
        parser,
        scanner::Scanner,
    },
    error::PiccoloError,
    runtime::{chunk, memory::Heap, vm::Machine, Object},
};
use rustyline::{
    error::ReadlineError, Cmd, ConditionalEventHandler, Editor, Event, EventContext, EventHandler,
    KeyEvent, Movement, RepeatCount,
};

#[derive(Options, Debug)]
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
        help = "Quit after parsing and compiling a file or string. Requires filename or -e/--eval. Overrides -i/--interactive.",
        short = "v"
    )]
    verify_syntax: bool,

    #[options(
        help = "Continue an interactive session in the repl after running a file.",
        short = "i"
    )]
    interactive: bool,

    #[options(help = "Run string argument. Conflicts with filename.", short = "e")]
    eval: Option<String>,
}

#[allow(unused_must_use)]
fn main() {
    run().map_err(print_errors);
}

fn run() -> Result<(), Vec<PiccoloError>> {
    #[cfg(feature = "log")]
    my_log::init();

    let args = Args::parse_args_default_or_exit();
    piccolo::debug!("{args:#?}");

    let Args {
        print_tokens,
        print_ast,
        print_compiled,
        verify_syntax,
        interactive,
        ..
    } = args;

    match args {
        Args { help: true, .. } => unreachable!(),

        Args {
            verify_syntax: true,
            interactive: true,
            ..
        } => {
            println!("Cannot use both -v/--verify-syntax and -i/--interactive.");
            Ok(())
        }

        Args {
            eval: Some(_),
            filename: Some(_),
            ..
        } => {
            println!("Cannot use both -e/--eval and filename.");
            Ok(())
        }

        Args {
            verify_syntax: true,
            eval: None,
            filename: None,
            ..
        } => {
            println!("Cannot use -v/--verify-syntax without a filename or string.");
            Ok(())
        }

        Args {
            filename: Some(filename),
            ..
        } => {
            let source =
                std::fs::read_to_string(&filename).map_err(|e| vec![PiccoloError::from(e)])?;

            maybe_exec_then_repl(
                &source,
                &filename,
                print_tokens,
                print_ast,
                print_compiled,
                verify_syntax,
                interactive,
            )
        }

        Args {
            eval: Some(eval), ..
        } => maybe_exec_then_repl(
            &eval,
            "-e/--eval",
            print_tokens,
            print_ast,
            print_compiled,
            verify_syntax,
            interactive,
        ),

        Args {
            eval: None,
            filename: None,
            ..
        } => {
            let emitter = Emitter::new();
            let mut heap = Heap::new();
            let machine = Machine::new(&mut heap);
            repl(
                emitter,
                heap,
                machine,
                print_tokens,
                print_ast,
                print_compiled,
            )
        }
    }
}

fn maybe_exec_then_repl(
    source: &str,
    name_for_module: &str,
    print_tokens: bool,
    print_ast: bool,
    print_compiled: bool,
    verify_syntax: bool,
    interactive: bool,
) -> Result<(), Vec<PiccoloError>> {
    if print_tokens {
        println!("=== tokens ===");
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_all()?;
        compiler::print_tokens(&tokens);
    }

    let ast = parser::parse(source)?;

    if print_ast {
        println!("=== ast ===\n{}", compiler::ast::print_ast(&ast));
    }

    let mut emitter = Emitter::new();
    emitter::compile_with(&mut emitter, &ast)?;

    if print_compiled {
        println!(
            "=== module ===\n{}",
            chunk::disassemble(emitter.module(), name_for_module)
        );
    }

    if !verify_syntax {
        let mut heap = Heap::new();
        let mut machine = Machine::new(&mut heap);

        machine.interpret(&mut heap, emitter.module())?;

        if interactive {
            repl(
                emitter,
                heap,
                machine,
                print_tokens,
                print_ast,
                print_compiled,
            )?;
        }
    }

    Ok(())
}

fn print_errors(errors: Vec<PiccoloError>) {
    if errors.is_empty() {
    } else if errors.len() == 1 {
        println!("Error {}", errors[0])
    } else {
        println!("{} Errors:", errors.len());
        for e in errors.iter() {
            println!("    {e}");
        }
    }
}

struct MyCtrlCHandler;
impl ConditionalEventHandler for MyCtrlCHandler {
    fn handle(&self, _: &Event, _: RepeatCount, _: bool, ctx: &EventContext) -> Option<Cmd> {
        if ctx.line().is_empty() {
            Some(Cmd::Interrupt)
        } else {
            Some(Cmd::Kill(Movement::WholeBuffer))
        }
    }
}

fn repl(
    mut emitter: Emitter,
    mut heap: Heap,
    mut machine: Machine,
    print_tokens: bool,
    print_ast: bool,
    print_compiled: bool,
) -> Result<(), Vec<PiccoloError>> {
    let mut rl = Editor::<()>::new();

    rl.load_history(".piccolo_history")
        .or_else(|_| std::fs::File::create(".piccolo_history").map(|_| ()))
        .unwrap();

    rl.bind_sequence(
        KeyEvent::ctrl('c'),
        EventHandler::Conditional(Box::new(MyCtrlCHandler)),
    );

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

                if print_tokens {
                    println!("=== tokens ===");
                    let scanner = Scanner::new(&input);
                    let tokens = scanner.scan_all().map_err(|e| print_errors(vec![e]));
                    if let Ok(tokens) = tokens {
                        compiler::print_tokens(&tokens);
                    }
                }

                let parse = parser::parse(&input);
                match parse {
                    Ok(ast) => {
                        if print_ast {
                            println!("=== ast ===\n{}", compiler::ast::print_ast(&ast));
                        }

                        let _: Result<(), ()> = emitter::compile_with(&mut emitter, &ast)
                            .and_then(|_| {
                                if print_compiled {
                                    println!(
                                        "=== module ===\n{}",
                                        chunk::disassemble(emitter.module(), "")
                                    );
                                }
                                machine
                                    .interpret_continue(&mut heap, emitter.module())
                                    .map_err(|e| vec![e])
                            })
                            .map_err(|errs| {
                                print_errors(errs);
                                machine.clear_stack_and_move_to_end_of_module(emitter.module());
                                emitter.reset_after_errors();
                            })
                            .map(|value| println!("{}", value.debug_format(&heap)));

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

            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break Ok(()),

            Err(err) => {
                println!("{err}");
                return Err(vec![PiccoloError::unknown(err)]);
            }
        }
    }
}
