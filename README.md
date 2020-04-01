
# Piccolo

Piccolo is a small, light, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

This branch is a bytecode interpreter rewrite of the previous AST-walking version.

## Design decisions that have been made
* Flesh out iterators (e.g. Lua's ipairs)
* Explicit constructors
* Rust interop
* Algol-style loop constructs (`for i from 0 to 10 by 1 while condition do end`)

## Design decisions still to be made
* Implicit vs explicit variable declaration
* Strict or weak typing with math/logic operators
* String indexing (at the very least EGC indexing)
* Lua-style userdata
