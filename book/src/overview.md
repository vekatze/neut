# Neut Programming Language

Neut is a dependently-typed programming language with static memory management.

An experiment to reconcile *λ-calculus* with *static memory management*.

## How is it Interesting?

The following two key features should make it interesting:

- Static memory management 🌟
- Full λ-calculus without restrictions 🌟

You'll have the power of the full λ-calculus without annotations to the type system, without losing predecitability of the memory behavior of your product. You'll have full control over your code.

## How Does the Language Generally Look Like?

```neut
// algebraic data types (not GADT)
variant my-item(a: tau) {
- Nil
- Cons(a, my-item(a))
}

// a nonsense function just to show how the language look like
define foo[a](xs: my-item(a)): i64 {
  let s = some-function(foo)
  let other-function =
    lambda (x: A, y: B, z: c) {
      do-something(x, y, z)
    }
  print("hello") // => Unit; this language is impure
  match xs {
  - Nil =>
    0
  - Cons(y, ys) =>
    add-i64(y, foo(ys))
  }
}

// &my-item(A) is a "noetic" type. This is an optimization stuff.
define tail[a](xs: &my-item(a)): option(&my-item(a)) {
  case xs {
  - Nil() =>
    none()
  - Cons(y, ys) =>
    some(ys)
  }
}
```

## Static Memory Management, but How?

*Neut translates a type into a function* that knows how to copy/discard the values of the type. Using those functions, every variable is copied/discarded so that it is used exactly once. For example, if a variable is used twice, conceptually, a translation like the following will happen:

```neut
let xs = create-some-list()
foo(xs)
bar(xs)

// ↓

let xs = create-some-list()
let xs-copy = copy-term-of-type-list-A(xs) // assuming x: list(A)
foo(xs-copy)
bar(xs)
```

If you need more, see [the chapter on the internals of neut](/chapter_1.html).

A sharp-minded reader might be thinking now, *"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"*. Then you can also check this chapter. You'll find that we can actually save the world.

(Spoiler: Neut's approach is twofold. It firstly finds a way to handle the full λ-calculus in a static and consistent manner, and then finds certain fragments in which we can apply "cheats")

## And Practically What Does it Mean?

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

# add module dependencies
$ neut add core https://github.com/vekatze/neut-core/raw/main/release/0.2.0.4.tar.zst
# => note: added a dependency: core (jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=)

# the mandatory hello world
$ tee some-file << END
define main(): i64 {
  print("Hello, world!\n")
}
END

# build the project and execute it
$ neut build --execute
# => Hello, world!

# copy the resulting binary to PATH and execute it
$ cp .build/amd64-darwin/0.2.0.0/executable/hello ~/.local/bin && hello
# => Hello, world!

# create a tarball to release this project as a module
$ neut release 0.1.0.0 && tree
# => ./
#    ├── release/
#    │  └── 0.1.0.0.tar.zst (← you can commit and push this tarball to publish it)
#    ├── source/
#    │  └── hello.nt
#    └── module.ens
```

To learn more about how to use it, follow [the language tour](./language-tour.md).

## List of Other Basic Features?

- A compiled language
- Call by Value
- Impure
- *Predictable static memory management without explicit malloc/free or regions*
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, or binary
- The typesystem ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - Currently not GADT but ADT (although this isn't a technical restriction)

## Anything Else?

You might find the module management system of Neut interesting. It distinguishes modules using the checksum of tarball, and defines package identity using module's version information. For more, see here.

## Then... What are Possible Drawbacks? Caveats?

See here.
