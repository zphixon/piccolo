#![allow(unknown_lints)]

extern crate piccolo;

#[test]
#[allow(approx_constant)]
fn scan_correctly() {
    use piccolo::scanner::Scanner;
    use piccolo::token::TokenKind;
    use piccolo::token::TokenKind::*;
    let tk: Vec<TokenKind> = Scanner::new(
        "do end fn if else while for in data is pub me new err retn \
         [ ] ( ) , . .. ... = \n ! + - * / % && || & | ^ == != < > \
         <= >= ident \"string\" 3.14 23"
            .into(),
    ).scan_tokens()
        .unwrap()
        .iter()
        .map(|t| t.kind)
        .collect();
    assert_eq!(
        tk,
        vec![
            Do,
            End,
            Fn,
            If,
            Else,
            While,
            For,
            In,
            Data,
            Is,
            Pub,
            Me,
            New,
            Err,
            Retn,
            LBracket,
            RBracket,
            LParen,
            RParen,
            Comma,
            Dot,
            ERange,
            IRange,
            Assign,
            Newline,
            Not,
            Plus,
            Minus,
            Star,
            Divide,
            Mod,
            And,
            Or,
            BAnd,
            BOr,
            BXor,
            Equals,
            NotEquals,
            LessThan,
            GreaterThan,
            LessThanEquals,
            GreaterThanEquals,
            Ident,
            String,
            Double(3.14),
            Integer(23),
            Eof,
        ]
    );
}

#[test]
fn test_file() {
    println!("{:?}", piccolo::parse_file("test.pc").unwrap());
}

#[test]
fn equal_truthy() {
    use piccolo::value::{is_equal, is_truthy, Value};
    assert!(is_equal(&Value::Nil, &Value::Nil));
    assert!(!is_truthy(&Value::Nil));
    assert!(is_equal(
        &Value::String("a".into()),
        &Value::String("a".into())
    ));
    assert!(!is_equal(
        &Value::String("a".into()),
        &Value::String("b".into())
    ));
    assert!(!is_equal(&Value::Float(3.0), &Value::Integer(3)));
    assert!(is_equal(&Value::Integer(3), &Value::Integer(3)));
    assert!(is_truthy(&Value::String("".into())));
}

#[test]
fn list_progs() {
    let progs = vec![
     // (pass,  program)
        (true,  "32 + -4.5 - 3 == 72 * 3 && 4 != 5"),
        (true,  "false == false"),
        (true,  "\"strang\" == \"string\""),
        (true,  "\"string\" == \"string\""),
        (true,  "io.prln(32 + 32)\n\nio.prln(true)\n\n\nio.prln(\"it is wednesday, my dudes\")\n"),
        (true,  "a = 0.1\n\nb=0.2\nio.prln(a + b == 0.3)\na = 9\nio.prln(a + b)"),
        (true,  "a = 2 b = 3 a b = 4"),
        (true,  "io.prln(a = 2)"),
        (false, "x = \"yes\"\nx or or or or"),
        (true,  "a = 1\nb = 1\nc = 1\ndo\n  a = 2\n  b = 2\n  do\n    a = 3\n    io.prln(a)\n    io.prln(b)\n    io.prln(c)\n  end\n  io.prln(a)\n  io.prln(b)\n  io.prln(c)\nend\nio.prln(a)\nio.prln(b)\nio.prln(c)\n"),
        (true,  "x = true\nif x do\n  io.prln(\"hey, not bad\")\nend\n"),
        (true,  "x = nil\nif x do\n  io.prln(\"crepe\")\nelse\n  io.prln(\"no crepe\")\nend\n"),
        (true,  "i = 0\nwhile i < 10 do\n  i = i + 1\n  io.prln(i)\nend\n"),
        (true,  "arr = [8, 6, 7, 5, 3, 0, 9]\nfor num in arr do\n  io.prln(num)\nend\n"),
        (true,  "for i in 2...4 do\n  io.prln(i)\nend"),
        (false, "for i in 4..5..6 do\n  io.prln(i)\nend\n"),
        (true,  "x = 1...10\n\nfor i in x do\n  io.prln((i * 29) % 34)\nend\n"),
        (true,  "a = 0\nb = 1\n\nwhile a < 10000 do\n  io.prln(a)\n  tmp = a\n  a = b\n  b = tmp + b\nend\n"),
        (true,  "b = 6\nb b b b b"),
        (true,  "io.prln(sys.clock())\n"),
        (true,  "fn something(x, y) is\n  io.prln(x * y)\nend\n\nsomething(3, 4)\n"),
        (true,  "fn fibonacci(n) is\n  if n <= 1 do\n    retn n\n  end\n  retn fibonacci(n - 2) + fibonacci(n - 1)\nend\n\nsys.assert(fibonacci(9) == 34)\n"),
    ];

    for (should_pass, prog) in progs {
        println!("program:");
        for (k, v) in prog.lines().enumerate() {
            println!("{}\t{}", k + 1, v);
        }
        println!();

        let s = piccolo::scanner::Scanner::new(prog.into()).scan_tokens();

        if s.is_err() {
            if should_pass {
                panic!("scan err!\n{}", s.err().unwrap());
            }
        } else {
            println!("tokens:");
            for tok in s.clone().unwrap() {
                println!("{:?}", tok);
            }
            println!();

            let p = piccolo::parser::Parser::new(s.unwrap()).parse();

            if p.is_err() {
                if should_pass {
                    for err in p.err().unwrap() {
                        eprintln!("{}", err);
                    }
                    panic!("parse err");
                }
            } else {
                println!("ast:");
                println!("{}", piccolo::AstPrinter.print(p.as_ref().unwrap()));

                let mut interp = piccolo::interp::Interpreter::new();
                println!("\noutput:");
                let i = interp.interpret(&p.unwrap());

                println!();

                if i.is_err() {
                    if should_pass {
                        panic!("runtime err!\n{}", i.err().unwrap());
                    }
                } else {
                    println!("huzzah!\n{:?}", interp.env);
                }
            }
        }
    }
}
