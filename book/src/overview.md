# Neut Programming Language

Neut is a dependently-typed programming language with _static memory management_.

Its key features include:

<ul class="star-list">
  <li>Full λ-calculus support</li>
  <li>Static memory management</li>
  <li><em>The absence of annotations to the type system</em> when achieving both of the above</li>
</ul>

I believe the last one is particularly interesting, as it means Neut found memory predictability _inside_ the usual λ-calculus.

## How Does it Look?

Like the below:

```neut
// the obligated hello world
define hello(): unit {
  print("Hello, world!\n")
}

// algebraic data types (tau = the type of types)
data my-list(a: tau) {
- Nil
- Cons(a, my-list(a))
}

// a recursive function with pattern matching
define noisy-length(a: tau, xs: my-list(a)): int {
  match xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    let my-message = "hey\n" in
    print(my-message);
    add-int(1, noisy-length(a, ys))
  }
}
```

## Static Memory Management — But How?

_Neut translates a type into a function_ that knows how to copy/discard the values of the type. Using those functions, every variable is copied/discarded so that it is used exactly once.

For example, if a variable is used twice, conceptually, a translation like below will happen:

```neut
let xs: list(a) = [value-1, value-2] in
some-func(xs, xs) // `xs` is used twice

// ↓

let xs: list(a) = [value-1, value-2] in
let (xs1, xs2) = copy-list-a(xs) in // now `xs` is used exactly once
some-func(xs1, xs2)
```

If you need more, see [Chapter 2 (Main Ideas)](./main-ideas.md).

---

You may be wondering: _"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"_. This topic is covered in [Section 2.4 (Noetic Optimization)](./noetic-optimization.md). As written there, those redundant copyings can be avoided. The idea is to add a new type `&a`, the noema type of `a`, which is the same as `a` except that it isn't copied/discarded, and to utilize it like a reference of the great ST monad.

## List of Other Basic Characteristics?

- Call by value (i.e. non-lazy)
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - That is, the usual one in functional programming, but a bit generalized
- Built-in LSP support

## Anything Else?

You might also find the module system of Neut interesting. _It distinguishes modules using the digests (checksums) of tarballs_ and defines module identities using version information. Although this is not the main point of Neut (and I'm ready to retract it immediately if necessary), it still might be of interest. For more, see [Chapter 4 (Module System)](./module-system.md).

Also, Neut includes a preliminary LSP server, which provides things like code completion, error reporting on save, etc. See [Chapter 5 (Development Environment)](./development-environment.md) for more.
