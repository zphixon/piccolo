
# Piccolo

Piccolo is a small, lightweight, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

This branch is a bytecode interpreter rewrite of the previous AST-walking version.

## Notes

* Function pointers can't be serialized. How would that even work?
    * In `Emitter`, map each function name to a number or index and create actual function pointer values
      at runtime.
* `Object` trait objects can't be serialized either, meaning we probably need a third value type to be able
  to return user data from the vm. `Constant` would then exist specifically for the binary format.
* `Gc` is an internal-use-only type that unfortunately we have to expose to library users.
  It is possible to introduce use-after-free errors with `Gc` if the `Heap` is dropped, since its
  lifetime is not bound in any way to `Heap.`
    * We could possibly solve this with a `PhantomData` to bind `Gc`/`Root`/`UniqueRoot` to the
      lifetime of `Heap`? I would need to do more reading/thinking/testing to make sure this would work.
      But, a `PhantomData` would spread a lifetime all around the codebase, reducing ergonomics.
* We need a way to allow users to put objects in and take objects out of the interpreter
  in a way that doesn't trigger this use-after-free.
    * Add a lifetime parameter for `Constant`? Again it would spread a lifetime parameter around
      the codebase, but possibly less so than adding one to `Gc`.

### Design decisions that have been made
* Explicit constructors using slightly modified syntax
    * `foo =: Foo(x=3, y=4)`
    * Don't use the `new` keyword?
* String indexing
    * `.byte_len()` method that returns byte length
    * `.egc_len()` method that returns extended grapheme cluster length
* Explicit variable declaration
    * This avoids some nasty problems solved in Lua and Python by the disgusting
      `local`, `global`, and `nonlocal` keywords that I'd rather not have to deal with
* Weak numeric types
    * Integer arithmetic existing alongside float propagation
* Coroutines
    * `yield` keyword

### Design decisions still to be made
* Multithreading
* How inter-operation will exist
    * `Object` trait needs to be fleshed out some more, they will likely
      need access to the `Machine` instance
* Labels on break/continue and goto
    * Break and continue should at least exist, but labels kind of imply
      the existence of goto, which I'm kind of on the fence about.

[❤](http://craftinginterpreters.com/)
