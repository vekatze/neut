# Neut Programming Language

Neut is a dependently-typed programming language with static memory management.

The "dependently-typed" part means that the language is based on a *typed λ-calculus*, in its highly generalized, non-restricted form. The "static" part means that its memory behavior is *predictable at compile-time*.

Also, and here comes the interesting point, both of the features come *without extra annotations* to its type system. In this sense, Neut is an attempt to "find" a memory-predictable λ-calculus in an ordinary type system.

What follows is a brief overview of Neut. It also explains the structure of this book.

## How is it Interesting?

The following three key features should make it interesting:

- 🌟 Full λ-calculus without restrictions
- 🌟 Static memory management (i.e. no explicit malloc/free, no GC, no regions)
- 🌟 Both of the above come without extra annotations to the type system

If you need more on its background motivation, see [the Chapter 2 (What Ignited This Language)](./what-ignited-this-language.md).

---

You might also find the module (package) management system of Neut interesting. *It distinguishes modules using the checksum of a tarball*, and defines module identity using semantic version. This is actually not the main point of Neut (and I'm ready to retract it immediately if I found a critical flaw), but still might be of interest. For more, see [the Section 4.3 (Namespaces and Modules)](./namespaces-and-modules.md).

## How Does it Basically Look Like?

Skim this:

```neut
// algebraic data types (not GADT, at least for now)
variant my-item(a: tau) {
- Nil
- Cons(a, my-item(a))
}

// a nonsense function just to show how the language looks like
define foo[a](xs: my-item(a)): i64 {
  let s = some-function(foo)
  let other-function =
    lambda (x: A, y: B, z: c) {
      do-something(x, y, z)
    }
  print("hello\n")
  match xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    add-i64(1, foo(ys))
  }
}
```

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

If you need more, see [the Chapter 3 (Main Ideas)](/chapter_1.html).

---

Your wise brain might be whispering now, *"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"*. If you hear the voice, you can also check [the Section 3.2 (Noetic Optimization)](./noetic-optimization.md). It might sound fishy, but you'll find that we can actually save the world.

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
  0
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

To learn more about how to use the language, follow [the Chapter 4 (Language Tour)](./language-tour.md).

## List of Other Basic Characteristics?

- A compiled language
- Call by Value
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, or binary
- The typesystem ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - (That is, the usual one in functional programming, but a bit generalized)

## What are Possible Drawbacks? Caveats?

The prominent one that come to my mind is that, when you use a polymorphic variant type, its type arguments must be stored in its values. For example, consider something like below:

```neut
// a syntax to define an ADT
variant Foo(a) {
- ConsA(i64, bool) // e.g. ConsA(3, True): Foo(a)
- ConsB(a)
}

// Haskell counterpart:
//   data Foo a
//     = ConsA Int64 Bool
//     | ConsB a
```

In an ordinary language, the internal representation of `ConsA(3, True)` will be something like:

```neut
// `0` is a tag to distinguish constructors
(0, 3, True)
```

However, in Neut, the type information must also be stored:

```neut
// `TYPE` is the address of the type (as function) `a`
(0, TYPE, 3, True)
```

This means that in Neut you must pay additional spaces to use parameterized variant types. So, it might be necessary to use non-parameterized variant types in performance critical situations, like:

```neut
variant FooText {
- ConsA(i64, bool) // e.g. ConsA(3, True): Foo(a)
- ConsB(text)
}
```

In that case, the internal representation will be `(0, 3, True)`. We'll have to be careful about what we have to pay for polymorphism.

To learn more, see [the Chapter 5 (The Good & The Bad)](./the-good-and-the-bad).

## ... But What After All is This Thing?

I've always wanted something like this, but couldn't find one. As usual, according to the noble law of our majestic solar system, I had to make it exist by myself, spending quite a lot of time. Neut is the outcome of the process I had to go through.

—Well, yes, the above is true, but I feel like it doesn't quite capture the whole story. Let me retry.

To tell the truth, this language is actually a painting. A small painting, redrawn again and again, alone, for like 7 years or longer, seeking my own understanding of beauty™, that happened to take the form of a programming language. Of course, this isn't a heroic thing or whatever, but rather a symptom, if I name it. This painting is entirely dedicated to my conceited obsession. Still, I now believe that the resulting language has something sparkling, and also I don't have any reason to keep it secret in my atelier.

I'd be happy if you were inspired by skimming this book over this weekend for example, or even happier if you chose to try it on your PC. Such a chain of reactions is a little lucky and lovely accident, which I believe is the fundamental element that colors our world.

A lot of pieces of mirrors here, reflecting each other.

The painting is also a mirror now.
