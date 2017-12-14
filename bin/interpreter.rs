
extern crate piccolo;

fn main() {
    let code =
        //"32 + -4.5 - 3 == 72 * 3 && 4 != 5";
        //"false == false";
        //"\"strang\" == \"string\"";
        //"\"string\" == \"string\"";
        //"me 32 + 32\n\nme true\n\n\nme \"it is wednesday, my dudes\"\n";
        //"a = 0.1\n\nb=0.2\nme a + b == 0.3\na = 9\nme a + b";
        //"a = 2 b = 3 a b = 4";
        //"me a = 2";
        //"x = \"yes\"\nx or or or or";
        //"a = 1\nb = 1\nc = 1\ndo\n  a = 2\n  b = 2\n  do\n    a = 3\n    me a\n    me b\n    me c\n  end\n  me a\n  me b\n  me c\nend\nme a\nme b\nme c\n";
        //"x = true\nif x do\n  me \"hey, not bad\"\nend\n";
        "x = nil\nif x do\n  me \"crepe\"\nelse\n  me \"no crepe\"\nend\n";

    println!("program:");
    for (k, v) in code.lines().enumerate() {
        println!("{}\t{}", k + 1, v);
    }
    println!();

    let s = piccolo::scanner::Scanner::new(code.into()).scan_tokens();

    if s.is_err() {
        println!("scan err!\n{}", s.err().unwrap());
    } else {
        println!("tokens:");
        for tok in s.clone().unwrap() {
            println!("{:?}", tok);
        }
        println!();

        let p = piccolo::parser::Parser::new(s.unwrap()).parse();

        if p.is_err() {
            println!("parse err!\n{}", p.err().unwrap());
        } else {
            println!("statements:");
            for stmt in p.clone().unwrap() {
                println!("{:?}", stmt);
            }

            let mut interp = piccolo::interp::Interpreter::new();
            println!("\noutput:");
            let i = interp.interpret(&p.unwrap());

            println!();

            if i.is_err() {
                println!("runtime err!\n{}", i.err().unwrap());
            } else {
                println!("huzzah!\n{:?}", interp.env);
            }
        }
    }
}

