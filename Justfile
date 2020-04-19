
set shell := ["powershell"]

r:
    cargo run --features "pc-debug" --example main

tf:
    cargo run --example test_files

f:
    cargo run --example fuzzer --features "fuzzer" --

c:
    cargo check

cc:
    cargo clean
    cargo clippy

t:
    cargo test --features "pc-debug"

vl:
    cargo test --package piccolo --lib tests::very_long -- --exact --ignored
