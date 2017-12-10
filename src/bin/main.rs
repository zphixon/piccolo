
extern crate piccolo;

fn main() {
    //let code = "32 + -4.5 - 3 == 72 * 3 && 4 != 5";
    //let code = "false == false";
    //let code = "\"strang\" == \"string\"";
    //let code = "\"string\" == \"string\"";
    //let code = "me 32 + 32\n\nme true\n\n\nme \"it is wednesday, my dudes\"\n";
    let code = "test = 32\n\nme 33 + 24";

    let s = piccolo::scanner::Scanner::new(code.into()).scan_tokens();

    if s.is_err() {
        println!("scan err\n\n{}", s.err().unwrap());
    } else {
        println!("tokens:");
        for tok in s.clone().unwrap() {
            println!("{:?}", tok);
        }
        println!();

        let p = piccolo::parser::Parser::new(s.unwrap()).parse();

        if p.is_err() {
            println!("parse err\\n{}", p.err().unwrap());
        } else {
            println!("statements:");
            for stmt in p.clone().unwrap() {
                println!("{:?}", stmt);
            }

            println!("\noutput:");
            let i = piccolo::interp::Interpreter::new().interpret(p.unwrap());

            println!();

            if i.is_err() {
                println!("runtime err\n\n{}", i.err().unwrap());
            } else {
                println!("huzzah");
            }
        }
    }
}

