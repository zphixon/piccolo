
extern crate piccolo;
extern crate rustyline;

use rustyline::Editor;
use rustyline::error::ReadlineError;

fn main() {
    let mut rl = Editor::<()>::new();
    let mut interp = piccolo::interp::Interpreter::new();

    loop {
        let input = rl.readline("-- ");

        match input {
            Ok(line) => {
                interp.reset_err();
                let s = piccolo::scanner::Scanner::new(line).scan_tokens();
                if s.is_err() {
                    println!("{}", s.err().unwrap());
                } else {
                    let p = piccolo::parser::Parser::new(s.unwrap()).parse();
                    if p.is_err() {
                        println!("{}", p.err().unwrap());
                    } else {
                        let p = p.unwrap();
                        if p.len() == 0 { continue }
                        if let piccolo::stmt::Stmt::StmtExpr(piccolo::stmt::StmtExpr(ref stmt)) = p[0] {
                            let v = interp.eval(stmt);
                            if v.is_err() {
                                println!("{}", v.err().unwrap());
                                continue
                            } else {
                                println!("{}", v.unwrap());
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
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
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

