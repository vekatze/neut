# Neut Programming Language

Neut is a functional programming language with static memory management.

Its key features include:

<ul class="star-list">
  <li>Full λ-calculus support</li>
  <li>Static memory management</li>
  <li><em>The absence of annotations to the type system</em> when achieving both of the above</li>
</ul>

I believe the last one is particularly interesting, as it seems to mean that Neut is leveraging memory predictability which resides _in_ the usual λ-calculus.

Practically, these mean that you now have a functional programming language without GC. The language doesn't need lifetime annotations, too.

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

_Neut translates a type into a function_ that knows how to copy/discard the values of the type. By using those functions, every variable is copied/discarded so that it is used exactly once.

For example, if a variable is used twice, a translation like the below will happen:

```neut
let xs: list(a) = [value-1, value-2] in
some-func(xs, xs) // `xs` is used twice

// ↓

let xs: list(a) = [value-1, value-2] in
let (xs1, xs2) = copy-list-a(xs) in // now `xs` is used exactly once
some-func(xs1, xs2)
```

If you need more, see [How to Execute Types](./main-ideas.md).

---

You may be wondering: _"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"_. This topic is covered in [Static Memory Management](./noetic-optimization.md). As written there, those redundant copyings can be avoided via something like borrowing in Rust. The idea is to add a new type `&a`, the noema type of `a`, which is the same as `a` except that it isn't copied/discarded, and to utilize it like a reference of the great ST monad.

## How Fast is This?

[Not so bad](./benchmarks.md).

## List of Other Basic Characteristics?

- Call by value (i.e. non-lazy)
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + (fix) - (universe hierarchy)
  - That is, the usual one in functional programming, but a bit generalized
- Built-in [LSP support](./lsp-support.md)
- Built-in [rapid prototyping experience](./rapid-prototyping.md) like scripting languages
- Built-in formatter like Go

## Anything Else?

You might also find the module system of Neut interesting. _It distinguishes modules using the digests (checksums) of tarballs_ and defines module identities using version information. Although this is not the main point of Neut (and I'm ready to retract it immediately if necessary), it still might be of interest. This topic is covered in the [tutorial](./functional-programming.md).

Also, Neut includes a preliminary LSP server, which provides things like code completion, error reporting on save, etc. See [Chapter 5 (Development Environment)](./development-environment.md) for more.

---

You can press the "→" key to go to the next page.
