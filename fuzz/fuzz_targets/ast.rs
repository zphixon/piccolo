#![no_main]
use libfuzzer_sys::fuzz_target;

use piccolo::compiler::{ast, emitter};

fuzz_target!(|data: ast::Stmt| {
    panic!("{data:?}");
    let _ = emitter::compile(&[data]);
});
