# Neut Programming Language

Neut is a functional programming language with static memory management.

Its key features include:

<ul class="star-list">
  <li>Full λ-calculus support</li>
  <li>Predictable automatic memory management</li>
  <li><em>The absence of annotations to the type system</em> when achieving both of the above</li>
</ul>

Neut doesn't use GCs or regions. Instead, it takes a _type-directed approach_ to handle resources.

## How Does it Look?

Like below:

```neut
// the obligated hello world
define hello(): unit {
  print("Hello, world!\n")
}

// algebraic data types
data my-list(a) {
| Nil
| Cons(a, my-list(a))
}

// a recursive function with pattern matching
define noisy-length(a: type, xs: my-list(a)): int {
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    let my-message = "hey\n" in
    print(my-message);
    add-int(1, noisy-length(a, ys))
  }
}
```

## Static Memory Management — But How?

_Neut translates a type into a function_ that can discard/copy the values of the type. By using those functions, the compiler translates programs so that every variable is used exactly once.

For example, if a variable is used twice, a translation like the following will happen:

```neut
// (before)
let xs: list(a) = [value-1, value-2] in
some-func(xs, xs) // `xs` is used twice

// ↓

// (after)
let xs: list(a) = [value-1, value-2] in
let (xs1, xs2) = copy-list-a(xs) in  // `xs` is used once
some-func(xs1, xs2)
```

If you need more, see [How to Execute Types](./how-to-execute-types.md).

You may wonder: _"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"_. This topic is covered in [Static Memory Management](./static-memory-management.md). As written there, Neut avoids such copyings by using the _T-necessity operator_ in modal logic to achieve something like borrowing in Rust.

## How Fast is This?

[Please see the benchmarks](./benchmarks.md).

## List of Other Basic Characteristics?

- Call by value
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html) and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + (T-necessity) + (fix) - (universe hierarchy)
  - That is, the usual one in functional programming, but a bit generalized
- Built-in [LSP support](./lovely-lsp-showcase.md)
- Built-in [rapid prototyping experience](./rapid-prototyping.md) like scripting languages
- Built-in formatter like Go

## Anything Else?

You might also find Neut's module system interesting. _It distinguishes modules using the digests (checksums) of tarballs_ and defines module identities using version information. Although this is not the main point of the language, it still might be of interest. This topic is covered in the [tutorial](./hello-external-world.md).

Also, Neut includes an LSP server, which provides things like code completion, error reporting on save, etc. See [Lovely LSP Showcase](./lovely-lsp-showcase.md) to see it in action.

---

You can press the "→" key to go to the next page.
