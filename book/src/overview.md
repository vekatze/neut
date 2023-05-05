# Neut Programming Language

Neut is a *dependently-typed* programming language with *static memory management*.

The key features include:

<ul class="star-list">
  <li>Full λ-calculus</li>
  <li>Static memory management</li>
  <li><em>The absence of annotations to the type system</em> when achieving both of the above</li>
</ul>

I believe the last one is especially interesting, as it means Neut found memory predictability *inside* the usual λ-calculus. The predictability has been there from the beginning.

## How Does it Basically Look Like?

Like below:

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

## Static Memory Management — But How?

*Neut translates a type into a function* that knows how to copy/discard the values of the type. Using those functions, every variable is copied/discarded so that it is used exactly once.

For example, if a variable is used twice, a translation like below will happen:

```neut
let xs: list(a) = [value-1, value-2]
foo(xs, xs) // `xs` is used twice

// ↓

let xs: list(a) = [value-1, value-2]
let xs-copy = copy-term-of-type-list-A(xs)
foo(xs-copy, xs) // now `xs` is used once (ignoring the "copying" call)
```

If you need more, see [Chapter 2 (Main Ideas)](./main-ideas.md).

---

Your brain might be whispering now, *"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"*. This topic is covered in [Section 2.4 (Noetic Optimization)](./noetic-optimization.md). As written there, those redundant copyings can be avoided. The idea is to add a new type `&A`, the noema type of `A`, which is the same as `A` except that it isn't consumed even after used, and to utilize it like a reference in the great ST monad.

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

To learn more about how to use the language, follow [Chapter 3 (Language Tour)](./language-tour.md).

## List of Other Basic Characteristics?

- A compiled language
- Call by Value
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - That is, the usual one in functional programming, but a bit generalized

## Anything Else?

You might also find the module system of Neut interesting. *It distinguishes modules using the checksums of tarballs* and defines module identities using version information. Although this is not the main point of Neut (and I'm ready to retract it immediately if necessary), it still might be of interest. For more, see [Chapter 4 (Module System)](./module-system.md).

## ... But What After All is This Thing?

I've always wanted something like this, but couldn't find one. As usual, by the noble law of our solar system, I had to make it exist by myself, spending quite a lot of time. Neut is the outcome of the process I had to go through.

—Well, yes, the above is true, but I feel like it doesn't quite capture the whole story. Let me retry.

To tell the truth, this language is actually a painting. A small painting, redrawn again and again, alone, for like 7 years or longer, seeking my own understanding of beauty™, that happened to take the form of a programming language. Of course, this isn't a heroic thing or whatever, but rather a symptom, if I name it. This painting is entirely dedicated to my conceited obsession. Still, I now believe that the resulting language has something sparkling in its concept, and also I don't have any reason to keep it secret in my atelier.

I'd be happy if you were inspired by skimming this book over this weekend for example, or even happier if you chose to try it on your PC. Such a chain of reactions is a little lucky and lovely accident, which I believe is the fundamental element that colors our world.

A lot of pieces of mirrors here, reflecting each other.

The painting is now a mirror.
