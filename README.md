
# Piccolo

Piccolo is a small, lightweight, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

## Notes

Users should be able to implement `Object` for their own types, so they can be
traced and garbage-collected. We could do this a number of ways:

1. Continue using the mark-and-sweep implementation from
   https://github.com/Darksecond/lox with its unsoundness issues (see
   `runtime::memory::test::use_after_free`)
2. Just use `Rc<RefCell<dyn Object>>`
3. Heap is a `SlotMap` of `Box<dyn Object>`

## Todo

- Type system? Would be fun
  - Parametric/subtype polymorphism
  - Dynamic dispatch, monomorphization
  - Algebraic data types and pattern matching
  - Type inference
- Compiler
  - https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
  - https://lib.rs/crates/inkwell

[❤](http://craftinginterpreters.com/)
