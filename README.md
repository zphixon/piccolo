
# Piccolo

Piccolo is a small, lightweight, high-pitched scripting language (eventually) intended
for embedding in Rust projects.

This branch is a bytecode interpreter rewrite of the previous AST-walking version.

## Notes

The design of Piccolo has some interesting lifetime challenges. For example, the
program below has a complex interweaving of lifetimes that isn't statically verifiable
by the Rust borrow checker. There must exist some value type that can represent a
pointer to either the chunk's constant table or the virtual machine's heap.

```rust
fn main() {
    // lifetime 'src

    piccolo::interpret(r#"
        # lifetime 'prog

        function closes() do
            # let x: &'src str = "hello "; // a.k.a 'static
            # let y: &'src str = "world!"; // a.k.a 'static
            x =: "hello "
            y =: "world!"

            # let z: &'prog str = heap.insert(format!("{}{}", x, y));
            z =: x + y

            # "print", "z" -> &'src str (reference to String in chunk)
            # "hello world!" -> &'prog str (reference to String in heap)
            # `z` on the stack should be a reference to its contents
            print(z)

            # let a: &'src str = "whee";
            # `a` is a reference to a value that exists in the chunk
            a =: "whee"

            # `closure` is a reference to a function object stored in the heap
            # closing over `a` should copy the value into the heap and add it
            # to the `upvalues` list of `closure`
            fn closure() do
                a = a + "!"
                retn a
            end

            retn closure
        end
    "#);
}
```

### Design decisions that have been made
* Explicit constructors using slightly modified
    * `foo =: Foo(x=3, y=4)`
    * Don't use the `new` keyword
* Algol-style loop constructs
    * `for i from 0 to 10 by 1 while condition do end`
    * `for item in iterator do end`
    * `for index, item in iterator do end`
* String indexing
    * `.len()` method that returns byte length
    * `.egc_len()` method that returns extended grapheme cluster length
* Explicit variable declaration
    * This avoids some nasty problems solved in Lua and Python by the disgusting
      `local`, `global`, and `nonlocal` keywords that I'd rather not have to deal with
* Weak numeric types
    * Integer arithmetic existing alongside float propagation

### Design decisions still to be made
* How inter-operation will exist
    * `Object` trait needs to be fleshed out some more, they will likely
      need access to the `Machine` instance

[❤](http://craftinginterpreters.com/)
