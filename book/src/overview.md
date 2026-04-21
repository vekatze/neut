# Neut Programming Language

Neut is a functional programming language with static memory management.

Its key features include:

- Full λ-calculus support
- Predictable automatic memory management
- The ability to achieve both of the above without extra type annotations

Neut doesn't use a GC. Instead, it takes a type-directed approach to memory management.

## Code Example

A typical program looks as follows:

```neut
// the obligatory hello world
define hello() -> unit {
  print("Hello, world!\n")
}

// an algebraic data type
data my-list(a) {
| Nil
| Cons(a, my-list(a))
}

// a recursive function with pattern matching
define foo<a>(xs: my-list(a)) -> int {
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    let my-message = "hey\n";
    print(my-message);
    add-int(1, foo(ys))
  }
}
```

## Static Memory Management

Neut translates a type into a function that can discard/copy the values of the type. By using those functions, the compiler translates programs so that every variable is used exactly once.

For example, if a variable is used twice, a translation like the following happens:

```neut
// (before)
let xs: list(a) = make-list();
some-func(xs, xs) // `xs` is used twice

// ↓

// (after)
let xs: list(a) = make-list();
let (xs1, xs2) = copy-list-a(xs);  // `xs` is used once
some-func(xs1, xs2)
```

If you need more information, see [On Executing Types](./on-executing-types.md).

At this point, one concern is whether operations such as taking the length of a list require copying the entire value. Neut avoids such copy operations by using the _box modality_, achieving something like borrowing. See [Static Memory Management](./static-memory-management.md) and [Modality and Memory](./modality-and-memory.md) for details.

## Quick List of Other Features

- Call-by-value
- Impure
- Compiles to LLVM IR and native binaries
- The type system ≈ System Fω + ADT + recursion + box modality
  - That is, the usual one in functional programming, but a bit generalized
- Built-in [LSP support](./lsp-showcase.md)
- Built-in [rapid prototyping support](./rapid-prototyping.md) as found in scripting languages
- Built-in formatter

---

You can press the "→" key to go to the next page.
