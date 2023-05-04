# Neut Programming Language

Neut is a *dependently-typed* programming language with *static memory management*.

The following three key features should make it interesting:

- 🌟 Full λ-calculus without restrictions
- 🌟 Static memory management (i.e. no explicit malloc/free, no GC)
- 🌟 Both of the above come without annotations to its type system

I believe the last one is especially interesting. In that sense, Neut is an attempt to find memory predictability *inside* the usual λ-calculus.

<!-- The "dependently-typed" part means that the language is based on a *typed λ-calculus*, in its highly generalized form. -->

<!-- The "static" part means that its *memory behavior is predictable at compile-time*. -->

<!-- The interesting point is that both of the features come *without extra annotations* to the type system. In this sense, Neut is an attempt to find memory-predictability "inside" the usual λ-calculus. -->

<!-- What follows is a brief overview of Neut. It also explains the structure of this book. -->

## How Does it Basically Look Like?

Skim this:

```neut
// algebraic data types
variant my-list(a: tau) {
- Nil
- Cons(a, my-list(a))
}

// a recursive function with pattern matching
define noisy-length[a](xs: my-list(a)): i64 {
  match xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    print("hey\n")
    add-i64(1, noisy-length(ys))
  }
}
```

<!-- ## How is it Interesting? -->

<!-- The following three key features should make it interesting: -->

<!-- - 🌟 Full λ-calculus without restrictions -->
<!-- - 🌟 Static memory management (i.e. no explicit malloc/free, no GC) -->
<!-- - 🌟 Both of the above come without extra annotations to the type system -->

## Static Memory Management — But How?

*Neut translates a type into a function* that knows how to copy/discard the values of the type. Using those functions, every variable is copied/discarded so that it is used exactly once.

For example, if a variable is used twice, a translation like the following will happen:

```neut
let xs: list(A) = create-some-list()
foo(xs, xs) // `xs` is used twice

// ↓

let xs: list(A) = create-some-list()
let xs-copy = copy-term-of-type-list-A(xs)
foo(xs-copy, xs) // now `xs` is used once (ignoring the call above to copy it)
```

If you need more, see [the Chapter 2 (Main Ideas)](./main-ideas.md).

---

Your brain might be whispering now, *"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"*. This topic is covered in [the Section 2.4 (Noetic Optimization)](./noetic-optimization.md). It might sound fishy, but you'll find that we can actually save the world. The idea is adding a new type `&A`, the noema of `A`, which is basically the same as `A` except that it isn't consumed even after used, and utilize it like a reference in the great ST monad.

## Quickstart?

An example scenario:

```sh
# installation (choose one)
$ wget http://github.com/vekatze/.../neut-mac-x86_64 ~/.local/bin/
$ wget http://github.com/vekatze/.../neut-linux-x86_64 ~/.local/bin/


# create a project
$ neut create hello && tree hello && cd hello
# => hello/
#    ├── source/
#    │  └── hello.nt
#    └── module.ens

# the mandatory hello world
$ tee source/hello.nt << END
define main(): i64 {
  print("Hello, world!\n")
  0
}
END

# build the project and execute it
$ neut build --execute
# => Hello, world!
```

To learn more about how to use the language, follow [the Chapter 3 (Language Tour)](./language-tour.md).

## List of Other Basic Characteristics?

- A compiled language
- Call by Value
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, or binary
  - The typesystem ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - (That is, the usual one in functional programming, but a bit generalized)

## Anything Else?

You might also find the module system of Neut interesting. *It distinguishes modules using the checksums of tarballs*, and defines module identities using version information. Although this is not the main point of Neut (and I'm ready to retract it immediately if I found a critical flaw), it still might be of interest. For more, see [the Chapter 4 (Module System)](./module-system.md).
