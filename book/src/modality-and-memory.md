# Modality and Memory

Here, we'll see how to interact with the box modality, which enables borrowing in Neut. We'll then see that both `on` and `*e` are actually syntactic sugar over this modality.

## Table of Contents

- [Introducing Layers and Boxes](#introducing-layers-and-boxes)
- [Auxiliary Tools for Boxes](#auxiliary-tools-for-boxes)
- [Desugaring Exotic Operations](#desugaring-exotic-operations)
- [Additional Notes](#additional-notes)

## Introducing Layers and Boxes

For every type `a`, Neut has a type `meta a`. This `meta` is often called a box modality in the literature.

In Neut, terms of this type can be created via layer-related syntactic constructs.

Below, we'll firstly introduce the concept of layers, and then use it to show how to use the modality.

### Introducing Layers

For every term in Neut, an integer value called _layer_ is defined. The layer of the body of a `define` is defined to be 0:

```neut
define foo(): () -> unit {
  // here is at layer 0
  function () {
    // here is also at layer 0
    Unit
  }
}
```

A variable defined at layer n can only be used at layer n. For example, the following isn't valid:

```neut
define bar(): unit {
  // here is layer 0
  let x = Unit in // ← `x` is defined at layer 0

  ... // ← some layer operations here

  // layer 3 (for example)
  let v2 =
    x // ← Error: `x` is used at layer 3 (≠ 0)
  in

  ...
}

```

Only modality-related operations can change layers, as we'll see below.

### Creating Boxes

You can use `box` to construct terms of type `meta a`:

```neut
define use-box(x: &int, y: &bool, z: &text): meta pair(int, bool) {
  // here is layer 0
  // free variables:
  // - x: &int
  // - y: &bool
  // - z: &text
  box x, y {
    // here is layer -1
    // free variables:
    // - x: int
    // - y: bool
    // - (z can't be used here because of layer mismatch)
    Pair(x, y)
  }
}
```

Given a term `e: a` and variables `x1: &a1, ..., xn: &an`, the type of `box x1, ..., xn {e}` is `meta a`.

If the layer of a term `e` is n, that of `box x1, ..., xn {e}` is n + 1.

Operationally, `box x1, ..., xn { e }` copies all the `x1, ..., xn` and executes `e`:

```neut
box x1, ..., xn { e }

↓

// psueudo-code
let x1 = COPY(type-1, x1) in
...
let xn = COPY(type-n, xn) in
e
```

The sequence `x1, ..., xn` can be empty.

### Using Boxes

We can extract a value from a box using `letbox`:

```neut
define use-letbox(x: int, y: bool, z: text): int {
  // here is layer 0
  // free variables:
  // - x: int
  // - y: bool
  // - z: text
  letbox extracted-value on x, y =
    // here is layer 1
    // free variables:
    // - x: &int
    // - y: &bool
    // - (z can't be used here because of layer mismatch)
    box {42}
  in
  // here is layer 0
  extracted-value // == 42
}
```

Operationally, `letbox` behaves as follows:

```neut
letbox x on y1, ..., ym = e1 in
e2

↓

let y1 = cast(a1, &a1, y1) in // cast y1: a1 → &a1
...                           // ...
let ym = cast(am, &am, ym) in // cast ym: am → &am
let x = e1 in
let y1 = cast(&a1, a1, y1) in // cast y1: &a1 → a1
...                           // ...
let ym = cast(&am, am, ym) in // cast ym: &am → am
e2
```

The `on y1, ..., yn` part in `letbox` can be omitted.

## Auxiliary Tools for Boxes

### Using Boxes Without Changing the Current Layer

Neut has a variant of `letbox`, called `letbox-T`. The only difference is that `letbox-T` doesn't change layers:

```neut
define use-letbox-T(x: int, y: bool): int {
  // here is layer 0
  // (x: int)
  // (y: bool)
  letbox-T value on x, y =
    // here is layer 0 (not 1!)
    // (x: &int)
    // (y: &bool)
    box {42}
  in
  // here is layer 0
  value // == 42
}
```

Using `letbox-T`, we can for example write functions of type `(meta a) -> a` as follows:

```neut
define axiom-T<a>(x: meta a): a {
  letbox-T tmp = x in
  tmp
}
```

Note that `letbox` can't be used here:

```neut
define axiom-T<a>(x: meta a): a {
  letbox tmp =
    // error: x is defined at layer 0 but used at layer 1
    x
  in
  tmp
}
```

### A Shorthand for Creating Boxes

We can construct a `meta bool` from a `bool` as follows:

```neut
define box-bool(b: bool): meta bool {
  match b {
  | True  => box {True}
  | False => box {False}
  }
}
```

This kind of construction can be mechanically done on "simple" types. To make things less tedious, Neut provides `quote` for this kind of constructions.

Given a term of a "simple" type `a`, `quote` casts it to `meta a`:

```neut
define box-bool(b: bool): meta bool {
  quote {b}
}
```

`quote` cannot be used against types that might contain types of the form `&a` or `(a) -> b`. For example, `quote` cannot be applied on values like the following:

- `&list(int)`
- `(int) -> bool`
- `either(bool, &list(int))`
- `either(bool, (int) -> bool)`

`quote` is after all just a shorthand.

## Desugaring Exotic Operations

### Desugar: Borrowing

Now we can desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓ // desugar

letbox-T x on y, z = quote {e1} in
e2
```

This explains why the type of `e1` must be restricted to some extent; those restrictions are from `quote`.

### Desugar: Embodying

Using the `axiom-T` that we've defined, we can desugar `*e` as follows:

```neut
*e

↓ // desugar

let x = e in
axiom-T(box x {x})
```

## Additional Notes

### Layers and Free Variables

Regarding layers, there's one last condition that must be satisfied. That is, if a function is defined at layer n, the layer of every free variable `x` in the function must satisfy `layer(x) <= n`.

<!-- There's one last condition: for any free variable `x` of a function `f`, `layer(x) <= layer(f)` must hold.  -->

For example, the following isn't well-layered:

```neut
define use-function(x: meta int): meta () -> int {
  // layer 0
  let x = 10 in
  box {
    // layer -1
    function () {
      letbox value = x in // Error: layer(x) = 0 > -1
      value
    }
  }
}
```

Without this condition, the following would be well-typed and well-layered:

```neut
define joker(): () -> unit {
  // layer 0
  let xs: list(int) = [1, 2, 3] in
  letbox f on xs =
    // layer 1
    // xs: at 1
    box {
      // layer 0
      function () {
        letbox k =
          // 1
          let len =
            // using xs@1 in function@0
            length(xs)
          in
          box {Unit}
        in
        Unit
      }
    }
  in
  f
}
```

The inner `function`, which contains `xs: &list(int)`, is bound to `f` after evaluating the outer `letbox`. Thus, we would be able to cause a use-after-free by deallocating `xs` and then calling the function `f`. Thus this restriction.
