
# Piccolo

Piccolo is a small, lightweight, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

This branch is a bytecode interpreter rewrite of the previous AST-walking version.

## Design decisions that have been made
* Explicit constructors using the old syntax
    * `foo =: new Foo(x=3, y=4)`
* Algol-style loop constructs
    * `for i from 0 to 10 by 1 while condition do end`
    * `for item in iterator do end`
    * `for index, item in iterator do end`
* String indexing
    * `.len` property that returns byte length
    * `.egc_len()` method that returns extended grapheme cluster length
* Explicit variable declaration
    * This avoids some nasty problems solved in Lua and Python by the disgusting
      `local`, `global`, and `nonlocal` keywords that I'd rather not have to deal with
* Weak numeric types
    * Integer arithmetic existing alongside float propagation

## Design decisions still to be made
* How inter-operation will exist
    * Rc<RefCell> is gross, you will probably just have to move your data
      into your program rather than having shared references
    * `Object` trait needs to be fleshed out some more, they will likely
      need access to the `Machine` instance

[❤](http://craftinginterpreters.com/)
