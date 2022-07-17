
# Piccolo

Piccolo is a small, lightweight, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

## Notes

### GC

Users should be able to implement `Object` for their own types, so they can be
traced and garbage-collected. We could do this a number of ways:

1. ~~Continue using the mark-and-sweep implementation from
   https://github.com/Darksecond/lox with its unsoundness issues (see
   `runtime::memory::test::use_after_free` in previous commits)~~
2. ~~Just use `Rc<RefCell<dyn Object>>`~~
3. **Heap is a `SlotMap` of `Box<dyn Object>`**

Small issue - We want objects to be able to mutate themselves, so the heap must
be able to give out mutable references to its objects. But the objects'
mutation potentially requires heap manipulation, however, we cannot allow
mutable references to the heap to exist at the same time as mutable references
to the object.

1. We could use a channel (`std::sync::mpsc::channel`) and have the `Object`s
   submit change requests to the `Heap` which will only carry them out when
   there can no longer be any `Box<dyn Objects>` in free space.
2. Do `unsafe` funny business to get around the problem with the limitation
   that only way to get a reference to an `Object` is if the heap still exists,
   or to `take` it from the heap entirely. This could be spicy since we'd have
   to uphold the safety invariants of `SlotMap` and potentially guarantee
   implementors of `Object` would also do so.

### Closures

Fun stuff that we probably want to have, but they present a few
interesting implementation challenges. A closure needs to:

1. Understand that local variables it refers to actually exist. The compiler
   needs to check the previous `EmitterContext`s for local variables that it
   captures, and also make sure it's not picking up the dummy variable so that
   functions can be recursive.
2. Put captured variables in the correct place on the stack.

### Tests

We have the test suite which is not great, but it works fine for now. The
actual lib tests were megagarbage and honestly should only target the
high-level APIs in the top-level module and the module-specific things, as well
as the experimental stuff.

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
