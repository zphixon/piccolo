
extern crate piccolo;

fn main() {
    let code = "32 + -4.5 - 3 == 72 * 3 && 4 != 5";
    //let code = "false == false";
    //let code = "\"strang\" == \"string\"";
    //let code = "\"string\" == \"string\"";
    let r = piccolo::scanner::Scanner::new(code.into()).scan_tokens();
    if r.is_err() {
        println!("{}", r.err().unwrap());
    } else {
        let p = piccolo::parser::Parser::new(r.unwrap()).parse();
        if p.is_err() {
            println!("{}", p.err().unwrap());
        } else {
            let i = piccolo::interp::Interpreter::new().interpret(&p.unwrap());
            if i.is_err() {
                println!("{}", i.err().unwrap());
            } else {
                println!("{:?}", i.unwrap());
            }
        }
    }
}

