use piccolo::{
    compiler::{emitter::compile_with, parser::parse},
    Environment,
};

#[allow(unused_assignments)]
fn main() {
    #[cfg(feature = "log")]
    my_log::init();

    let ast1 = parse("x=:3").unwrap();
    let ast2 = parse("assert x == 3").unwrap();
    let ast3 = parse("fn z(a) do\n  print(\"a is\", a)\nend\n").unwrap();
    let ast4 = parse("z(x)").unwrap();
    let ast5 =
        parse("fn b(a) do\n  if a == x do\n    retn true\n  else\n    retn false\n  end\nend\n")
            .unwrap();
    let ast6 = parse("assert b(3)").unwrap();
    let ast7 = parse("print(b(4))").unwrap();
    let mut env = Environment::new();

    compile_with(&mut env.emitter, &ast1).unwrap();

    println!("run 1");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();

    compile_with(&mut env.emitter, &ast2).unwrap();
    println!("run 2");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();

    compile_with(&mut env.emitter, &ast3).unwrap();
    println!("run 3");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();

    compile_with(&mut env.emitter, &ast4).unwrap();
    println!("run 4");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();

    compile_with(&mut env.emitter, &ast5).unwrap();
    println!("run 5");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();

    compile_with(&mut env.emitter, &ast6).unwrap();
    println!("run 6");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();

    compile_with(&mut env.emitter, &ast7).unwrap();
    println!("run 7");
    env.vm
        .interpret_continue(&mut env.heap, env.emitter.module())
        .unwrap();
}
