
set shell := ["powershell"]

r:
    cargo run --features "pc-debug" --example main

rr:
    cargo run --example main --release

c:
    cargo check

cc:
    cargo clean
    cargo clippy

ccc:
    cargo check
    cargo check --features "pc-debug"
    cargo check --features "fuzzer"
    cargo test --no-run --all-targets
    cargo test --no-run --all-targets --features "pc-debug"
    cargo test --no-run --all-targets --features "fuzzer"

b:
    cargo build --features "pc-debug" --example main

t:
    cargo test --features "pc-debug"

tt:
    cargo run --example test_files

do:
    cargo doc --no-deps --open

f:
    cargo run --example fuzzer --features "fuzzer" --

vl:
    cargo test --package piccolo --lib tests::very_long -- --exact --ignored
