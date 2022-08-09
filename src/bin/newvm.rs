#![allow(dead_code)]

fn main() {
    #[cfg(feature = "logging")]
    my_log::init();
    let ast = piccolo::compiler::parser::parse("a =: 1 + 2 b =: a + 3").unwrap();
    //let ast = piccolo::compiler::parser::parse("print('a')").unwrap();
    let (mut state, mut program) = piccolo::v2::compiler::compile(&ast).unwrap();
    program.run_with(&mut state).unwrap();
}
