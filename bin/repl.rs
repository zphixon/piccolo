
extern crate piccolo;
extern crate rustyline;

use rustyline::Editor;
use rustyline::error::ReadlineError;

fn main() {
    let mut interp = piccolo::interp::Interpreter::new();
    let mut rl = Editor::<()>::new();

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
                        println!("{}", p.err().unwrap());
                    } else {
                        let p = p.unwrap();
                        if p.is_empty() { continue }

                        if p.len() == 1 {
                            if let piccolo::stmt::Stmt::StmtExpr(piccolo::stmt::StmtExpr(ref stmt)) = p[0] {
                                let mut v = interp.evaluate(stmt);

                                if v.is_err() {
                                    println!("{}", v.err().unwrap());
                                    continue
                                } else {
                                    let val = &mut v.unwrap();
                                    println!("{:?}", std::rc::Rc::make_mut(val).borrow());
                                }
                            } else {
                                let mut v = interp.interpret(&p);

                                if v.is_err() {
                                    println!("{}", v.err().unwrap());
                                } else {
                                    if let Some(mut ret) = v.unwrap() {
                                        //let val = &mut ret.unwrap();
                                        println!("{:?}", std::rc::Rc::get_mut(&mut ret).unwrap().borrow());
                                    }
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
            },

            Err(ReadlineError::Interrupted) => {
                break
            },

            Err(ReadlineError::Eof) => {
                println!("bye!");
                break
            },

            Err(e) => {
                println!("err: {:?}", e);
                break
            }
        }
    }
}
