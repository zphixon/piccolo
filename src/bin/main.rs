
extern crate piccolo;

fn main() {
    //let code = "32 + -4.5 - 3 == 72 * 3 && 4 != 5";
    //let code = "false == false";
    //let code = "\"strang\" == \"string\"";
    //let code = "\"string\" == \"string\"";
    let code = "me 32 + 32\n\nme true\n\n\nme \"it is wednesday,\\nmy dudes\"\n";

    let s = piccolo::scanner::Scanner::new(code.into()).scan_tokens();

    if s.is_err() {
        println!("{}", s.err().unwrap());
    } else {
        for tok in s.clone().unwrap() {
            println!("{:?}", tok);
        }

        let p = piccolo::parser::Parser::new(s.unwrap()).parse();

        if p.is_err() {
            println!("{}", p.err().unwrap());
        } else {
            for stmt in p.clone().unwrap() {
                println!("{:?}", stmt);
            }

            let i = piccolo::interp::Interpreter::new().interpret(p.unwrap());

            if i.is_err() {
                println!("{}", i.err().unwrap());
            } else {
                println!("huzzah");
            }
        }
    }
}

