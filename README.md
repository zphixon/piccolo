
# Piccolo

Piccolo is a small, light, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

This branch is a bytecode interpreter rewrite of the previous AST-walking version.

## Design decisions that have been made
* Explicit constructors using the old syntax
* Rust interop
* Algol-style loop constructs
    * `for i from 0 to 10 by 1 while condition do end`
    * `for item in iterator do end`
    * `for index, item in iterator do end`
* String indexing
    * `.len` property that returns byte length
    * `.egc_len()` method that returns extended grapheme cluster length

## Design decisions still to be made
* Implicit vs explicit variable declaration
* Strict or weak typing with math/logic operators
* Lua-style userdata
* Split chunk constants and the Value type
