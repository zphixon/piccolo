extern crate piccolo;

use piccolo::foreign::{Foreign, Test};

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
        //"prln(\"testarino\")";
        //"fn something(x, y) do\n  prln(x * y)\nend\n\nsomething(3, 4)\n";
        //"fn fibonacci(n) is\n  if n <= 1 do\n    retn n\n  end\n  retn fibonacci(n - 2) + fibonacci(n - 1)\nend\nprln(fibonacci(9))\nassert(fibonacci(9) == 34)\n";
        //"if true do\n  prln(\"crepe\")\nend";
        //"for num in 1..999 do\n  if 79 % num == 32 do\n    panic(\"it's 32: \" + str(num))\n  end\nend";
        //"z = [1, 2, 3]\nprln(z[1])\nz[0] = \"yes\"\nprln(z)";
        //"";
        "x = arr()\nio.prln(x[0])\nx[0] = 7\nio.prln(x[0])\n";
    //"data has_arr is\n  pub arr = [1, 2, 3]\nend\n\nx = new has_arr\n\nx.arr[2] = 9\n";
    //"x = [1, 2, 3]\nx[0] = 99\nio.prln(x)\n";
    //        r#"data counter is
    //  i = 0
    //  fn count() is
    //    me.i = me.i + 1
    //    retn me.i
    //  end
    //end
    //
    //data on_the_wall is
    //  pub what = "bottles of beer"
    //  counter = new counter
    //
    //  fn sing() is
    //    while (i = me.counter.count()) <= 99 do
    //      prln(str(i) + " " + me.what + " on the wall")
    //    end
    //  end
    //
    //  fn reset() is
    //    me.counter = new counter
    //  end
    //end
    //
    //gp = new on_the_wall(what = "tubs of grey poupon")
    //gp.sing()
    //"#;

    //let x = Box::new(Someg)
    //println!("{:?}", "a".partial_cmp("b"));
    //let x = ::std::rc::Rc::new(Test {
    //    inner: "hi".into(),
    //});
    //y(x);
    //<Box<Something> as Box<Foreign>>::clone(x);
    //Box::new(x).<Box<piccolo::foreign::Something> as Box<Foreign>>::clone();

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
            println!("parse err!");
            for err in p.err().unwrap() {
                println!("{}", err);
            }
        //println!("parse err!\n{}", p.err().unwrap());
        } else {
            println!("ast:");
            println!("{}", piccolo::AstPrinter.print(p.as_ref().unwrap()));
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
                println!("huzzah!\n{}", interp.env);
            }
        }
    }
}
