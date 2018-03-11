extern crate piccolo;
extern crate rustyline;

use rustyline::Editor;
use rustyline::error::ReadlineError;

fn main() {
    let mut interp = piccolo::interp::Interpreter::new();
    let mut rl = Editor::<()>::new();
    rl.load_history(".piccolo_history").unwrap();

    loop {
        let input = rl.readline("-- ");

        match input {
            Ok(line) => {
                rl.add_history_entry(&line);

                //interp.reset_err();
                let s = piccolo::scanner::Scanner::new(line).scan_tokens();

                if s.is_err() {
                    println!("{}", s.err().unwrap());
                } else {
                    let p = piccolo::parser::Parser::new(s.unwrap()).parse();

                    if p.is_err() {
                        for err in p.err().unwrap() {
                            println!("{}", err);
                        }
                    //println!("{}", p.err().unwrap());
                    } else {
                        let p = p.unwrap();
                        if p.is_empty() {
                            continue;
                        }

                        if p.len() == 1 {
                            if let piccolo::stmt::Stmt::StmtExpr(piccolo::stmt::StmtExpr(
                                ref stmt,
                            )) = p[0]
                            {
                                let mut v = interp.evaluate(stmt);

                                if v.is_err() {
                                    println!("{}", v.err().unwrap());
                                    continue;
                                } else {
                                    let val = &mut v.unwrap();
                                    println!("{:?}", val);
                                }
                            } else {
                                let mut v = interp.interpret(&p);

                                if v.is_err() {
                                    println!("{}", v.err().unwrap());
                                } else if let Some(mut ret) = v.unwrap() {
                                    println!("{:?}", ret);
                                }
                            }
                        } else {
                            let i = interp.interpret(&p);

                            if i.is_err() {
                                println!("{}", i.err().unwrap());
                            }
                        }
                    }
                }
            }

            Err(ReadlineError::Interrupted) => break,

            Err(ReadlineError::Eof) => {
                println!("bye!");
                break;
            }

            Err(e) => {
                println!("err: {:?}", e);
                break;
            }
        }
    }

    rl.save_history(".piccolo_history").unwrap();
}
