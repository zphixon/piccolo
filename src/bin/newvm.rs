#![allow(dead_code)]

fn main() {
    #[cfg(feature = "logging")]
    my_log::init();
    let ast = piccolo::compiler::parser::parse("a =: 1 + 2 b =: a + 3").unwrap();
    let (mut emitter, _) = piccolo::v2::compiler::compile(&ast);
    let mut state = piccolo::v2::State::default();
    emitter.program.run_with(&mut state).unwrap();
}
