
set shell := ["powershell"]

r:
    cargo run --features "pc-debug" --example main

tf:
    cargo run --example test_files

f:
    cargo run --example fuzzer --features "fuzzer" -- 

c:
    cargo clean
    cargo clippy

t:
    cargo test --features "pc-debug"

idk:
    cargo test --package piccolo --lib ast::parser::idk --features "pc-debug" -- --exact

