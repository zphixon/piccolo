use piccolo::{compiler::parser::parse, Environment};

#[allow(unused_assignments)]
fn main() {
    #[cfg(feature = "logging")]
    my_log::init();

    let ast1 = parse("x=:3").unwrap();
    let ast2 = parse("assert x == 3").unwrap();
    let ast3 = parse("fn z(a) do\n  print(\"a is\", a)\nend\n").unwrap();
    let ast4 = parse("z(x)").unwrap();
    let ast5 = parse(
        "fn b(a) do\n  if a == x do\n    return true\n  else\n    return false\n  end\nend\n",
    )
    .unwrap();
    let ast6 = parse("assert b(3)").unwrap();
    let ast7 = parse("print(b(4))").unwrap();

    let mut env = Environment::new();

    env.compile(&ast1).unwrap();

    println!("run 1");
    env.interpret_compiled().unwrap();

    env.compile(&ast2).unwrap();
    println!("run 2");
    env.interpret_compiled().unwrap();

    env.compile(&ast3).unwrap();
    println!("run 3");
    env.interpret_compiled().unwrap();

    env.compile(&ast4).unwrap();
    println!("run 4");
    env.interpret_compiled().unwrap();

    env.compile(&ast5).unwrap();
    println!("run 5");
    env.interpret_compiled().unwrap();

    env.compile(&ast6).unwrap();
    println!("run 6");
    env.interpret_compiled().unwrap();

    env.compile(&ast7).unwrap();
    println!("run 7");
    env.interpret_compiled().unwrap();
}
