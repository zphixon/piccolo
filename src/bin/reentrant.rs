use piccolo::{
    compiler::{
        emitter::{compile_with, Emitter},
        parser::parse,
    },
    runtime::{memory::Heap, vm::Machine},
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
    let mut emitter = Emitter::new();

    compile_with(&mut emitter, &ast1).unwrap();

    let mut heap = Heap::default();
    let mut vm = Machine::new(&mut heap);
    println!("run 1");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();

    compile_with(&mut emitter, &ast2).unwrap();
    println!("run 2");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();

    compile_with(&mut emitter, &ast3).unwrap();
    println!("run 3");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();

    compile_with(&mut emitter, &ast4).unwrap();
    println!("run 4");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();

    compile_with(&mut emitter, &ast5).unwrap();
    println!("run 5");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();

    compile_with(&mut emitter, &ast6).unwrap();
    println!("run 6");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();

    compile_with(&mut emitter, &ast7).unwrap();
    println!("run 7");
    vm.interpret_continue(&mut heap, emitter.module()).unwrap();
}
