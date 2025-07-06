# Neut Programming Language

Neut is a functional programming language with static memory management.

Its key features include:

<ul class="star-list">
  <li>Full λ-calculus support</li>
  <li>Predictable automatic memory management</li>
  <li>The ability to achieve both of the above without additional type annotations</li>
</ul>

Neut doesn't use a GC. Instead, it takes a _type-directed approach_ for memory management.

## What Does it Look Like?

Like the following:

```neut
// the obligatory hello world
define hello(): unit {
  print("Hello, world!\n")
}

// an algebraic data type
data my-list(a) {
| Nil
| Cons(a, my-list(a))
}

// a recursive function with pattern matching
define noisy-length<a>(xs: my-list(a)): int {
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    let my-message = "hey\n";
    print(my-message);
    add-int(1, noisy-length(ys))
  }
}
```

## Static Memory Management — But How?

_Neut translates a type into a function_ that can discard/copy the values of the type. By using those functions, the compiler translates programs so that every variable is used exactly once.

For example, if a variable is used twice, a translation like the following happens:

```neut
// (before)
let xs: list(a) = List[value-1, value-2];
some-func(xs, xs) // `xs` is used twice

// ↓

// (after)
let xs: list(a) = List[value-1, value-2];
let (xs1, xs2) = copy-list-a(xs);  // `xs` is used once
some-func(xs1, xs2)
```

If you need more information, see [How to Execute Types](./how-to-execute-types.md).

You might wonder: _"So do I have to, for example, copy an entire list just to get its length? Isn't that a tragedy?"_. This topic is covered in [Static Memory Management](./static-memory-management.md) and [Modality and Memory](./modality-and-memory.md). As written there, Neut avoids such copy operations by using the _box modality_, achieving something like borrowing in Rust.

## Quick List of Other Features

- Call by value
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html) and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + (recursion) + (T-necessity) - (universe hierarchy)
  - That is, the usual one in functional programming, but a bit generalized
- Built-in [LSP support](./lovely-lsp-showcase.md)
- Built-in [rapid prototyping experience](./rapid-prototyping.md) like scripting languages
- Built-in formatter like Go

---

You can press the "→" key to go to the next page.
