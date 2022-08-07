#![allow(dead_code)]

fn main() {
    let ast = piccolo::compiler::parser::parse("a =: 1 + 2 b =: a + 3").unwrap();
    let (mut program, _) = piccolo::v2::compiler::compile(&ast);
    let mut state = piccolo::v2::State::default();
    program.run_with(&mut state).unwrap();
}
