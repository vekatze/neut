# Modality and Memory

Here, we'll see how to interact with the box modality `meta`, which enables borrowing in Neut. We'll then see that both `on` and `*e` can be understood as syntactic sugar over this modality.

## Table of Contents

- [Layers and the Box Modality](#layers-and-the-box-modality)
- [More Tools for Boxes](#more-tools-for-boxes)
- [Desugaring the Two Operations](#desugaring-the-two-operations)
- [Additional Notes](#additional-notes)

## Layers and the Box Modality

In Neut, each type `a` has a corresponding type `meta a`. This type provides a way to work with *layers*, which are similar to lifetimes in other languages.

Below, we’ll first introduce the concept of layers, and then see how to use `meta a`.

### Layers and Variables

Every term in Neut has an integer called layer. Conceptually, a layer can be seen as the level at which a piece of data lives.

The body of a `define` starts at layer 0:

```neut
define foo(): () -> unit {
  // here is layer 0
  function () {
    // here is also layer 0
    Unit
  }
}
```

A variable defined at layer n can only be used at the same layer. For example, the following code is invalid because the variable `x` is defined at layer 0 but used at layer 3:

```neut
define bar(): unit {
  // here is layer 0
  let x = Unit; // ← `x` is defined at layer 0

  ... // ← some layer operations here

  // layer 3 (for example)
  let v2 =
    x; // ← Error: `x` is used at layer 3 (≠ 0)
  ...
}

```

Only modality-related operations can change layers, as we'll see below.

### Creating Boxes

To create a term of type `meta a`, use `box`:

```neut
define use-box(x: &int, y: &bool, z: &text): meta pair(int, bool) {
  // here is layer 0
  // free variables:
  // - x: &int
  // - y: &bool
  // - z: &text
  box x, y {
    // here is layer -1 (== layer(outer) - 1)
    // free variables:
    // - x: int
    // - y: bool
    // - (z is unavailable here because of layer mismatch)
    Pair(x, y)
  }
}
```

Some notes on `box`:

- The type of `xi` in `box x1, ..., xn {e}` must be of the form `&ai`.
- Given `xi: &ai`, the type of `xi` in the body of `box` is `ai`.

`box` behaves as follows:

```neut
box x1, ..., xn { e }

↓ // (compile)

let x1 = COPY(a1, x1);
...
let xn = COPY(an, xn);
e
```

You can omit the sequence `x1, ..., xn` entirely if no variables need to be copied.

### Using Boxes

To use a term of type `meta a`, use `letbox`:

```neut
define use-letbox(x: int, y: bool, z: text): int {
  // here is layer 0
  // free variables:
  // - x: int
  // - y: bool
  // - z: text
  letbox extracted-value on x, y = {
    // here is layer 1 (== layer(outer) + 1)
    // free variables:
    // - x: &int
    // - y: &bool
    // - (z is unavailable here because of layer mismatch)
    some-func(x, y);
    box {42};
  };
  // here is layer 0
  // free variables:
  // - x: int
  // - y: bool
  // - z: text
  extracted-value // == 42
}
```

Some notes on `letbox`:

- The type of `xi` in `on x1, ..., xn` has no restriction.
- Given `xi: ai`, the type of `xi` inside `letbox` is `&ai`.

`letbox` behaves as follows:

```neut
letbox v on x1, ..., xn = e1;
e2

↓ // (compile)

let x1 = cast(a1, &a1, x1); // cast x1: a1 → &a1
...                         // ...
let xn = cast(an, &an, xn); // cast xn: an → &an

let v  = e1;

let x1 = cast(&a1, a1, x1); // cast x1: &a1 → a1
...                         // ...
let xn = cast(&an, an, xn); // cast xn: &an → an

e2
```

The `on y1, ..., yn` in `letbox` is optional.

## More Tools for Boxes

### Using Boxes Without Changing the Current Layer

Sometimes you want to use a term of type `meta a` without shifting your current layer. For this, Neut provides `letbox-T`, which keeps you in the same layer:

```neut
define use-letbox-T(x: int, y: bool): int {
  // here is layer 0
  letbox-T value on x, y = {
    // here is layer 0 (== layer(outer))
    box {42}
  };
  // here is layer 0
  value // == 42
}
```

`letbox-T` can be used for example to write functions of type `(meta a) -> a` as follows:

```neut
define axiom-T<a>(x: meta a): a {
  letbox-T tmp = x;
  tmp
}
```

If you tried to use `letbox` instead, you’d get an error because it would result in layer mismatch.

### A Shortcut for Creating Boxes

We can, for example, construct a `meta bool` from a `bool` as follows:

```neut
define box-bool(b: bool): meta bool {
  match b {
  | True  => box {True}
  | False => box {False}
  }
}
```

To streamline this kind of mechanical step, Neut provides `quote`:

```neut
define box-bool(b: bool): meta bool {
  quote {b} // `quote` casts `bool` into `meta bool`
}
```

Not all types can be cast using `quote`. Specifically, it can't be used on any type that contains:

- a type of the form `&a`
- a type of the form `(a1, ..., an) -> b`
- a type variable

If you can get `meta t` by quoting `e: t`, you can get the same type using `box` instead. In this sense, `quote` is a shortcut for creating boxes.

## Desugaring the Two Operations

We've seen the two constructs `let-on` and `*e`. Though they might have initially appeared a bit artificial, they are in fact straightforward applications of the box modality.

### Desugar: Borrowing

We can now desugar `let-on` as follows:

```neut
let x on y, z = e1;
e2

↓ // desugar

letbox-T x on y, z = quote {e1};
e2
```

This explains why the result type of a `let-on` had to be restricted to some extent: the restriction is from `quote`.

### Desugar: Embodying

Using the `axiom-T` we defined above, we can also desugar `*e` as follows:

```neut
*e

↓ // desugar

let x = e;
axiom-T(box x {x})
```

## Additional Notes

### Layers and Free Variables

There's one last rule that must be satisfied for memory safety. That is, if a function is defined at layer `n`, then any free variable `x` in the function must satisfy `layer(x) <= n`.

Without this rule, you could do something like the following:

```neut
define joker(): () -> unit {
  // layer 0
  let xs: list(int) = [1, 2, 3];
  letbox f on xs =
    // layer 1
    // xs: &list(int), at 1
    box {
      // layer 0
      function () { // ★
        letbox k = {
          // 1
          let len = length(xs);
          box {Unit}
        };
        Unit
      }
    };
  f
}
```

This example would wrongly allow a function at layer 0 (`★`) to keep a reference to data (`xs`) that, after the outer `letbox` completes, could be deallocated, leading to a use-after-free scenario. Hence, Neut’s layer rules prohibit capturing a higher-layer variable in a lower-layer function.
