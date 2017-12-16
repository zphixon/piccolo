
extern crate piccolo;

fn main() {
    let code =
        //"32 + -4.5 - 3 == 72 * 3 && 4 != 5";
        //"false == false";
        //"\"strang\" == \"string\"";
        //"\"string\" == \"string\"";
        //"prln(32 + 32)\n\nprln(true)\n\n\nprln(\"it is wednesday, my dudes\")\n";
        //"a = 0.1\n\nb=0.2\nprln(a + b == 0.3)\na = 9\nprln(a + b)";
        //"a = 2 b = 3 a b = 4";
        //"prln(a = 2)";
        //"x = \"yes\"\nx or or or or";
        //"a = 1\nb = 1\nc = 1\ndo\n  a = 2\n  b = 2\n  do\n    a = 3\n    prln(a)\n    prln(b)\n    prln(c)\n  end\n  prln(a)\n  prln(b)\n  prln(c)\nend\nprln(a)\nprln(b)\nprln(c)\n";
        //"x = true\nif x do\n  prln(\"hey, not bad\")\nend\n";
        //"x = nil\nif x do\n  prln(\"crepe\")\nelse\n  prln(\"no crepe\")\nend\n";
        //"i = 0\nwhile i < 10 do\n  i = i + 1\n  prln(i)\nend\n";
        //"arr = [8, 6, 7, 5, 3, 0, 9]\nfor num in arr do\n  prln(num)\nend\n";
        //"for i in 2...4 do\n  prln(i)\nend";
        //"for i in 4..5..6 do\n  prln(i)\nend\n";
        //"x = 1...10\n\nfor i in x do\n  prln((i * 29) % 34)\nend\n";
        //"a = 0\nb = 1\n\nwhile a < 10000 do\n  prln(a)\n  tmp = a\n  a = b\n  b = tmp + b\nend\n";
        //"b = 6\nb b b b b";
        //"prln(clock())\n";
        "prln(\"testarino\")";

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
            println!("ast:");
            println!("{}", piccolo::AstPrinter.print(&p.as_ref().unwrap()));
            //println!("statements:");
            //for stmt in p.clone().unwrap() {
            //    println!("{:?}", stmt);
            //}

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

