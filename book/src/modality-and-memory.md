# Modality and Memory

At first glance, the `let-on` stuff in the previous section might seem a bit artificial.

In fact, however, this `let-on` can be understood as a syntax sugar over the T-necessity operator.

Below, we'll first see how Neut incorporates the necessity modality and then how `let-on` is desugared using the modality.

## Table of Contents

- [Introducing Layers and Boxes](#copying-and-discarding-values)
- [Auxiliary Tools for Boxes](#optimization-avoiding-unnecessary-copies)
- [Desugaring Exotic Operations](#decomposing-on)
- [Additional Notes](#optimization-avoiding-unnecessary-copies)

## Introducing Layers and Boxes

For every type `a`, Neut has a type `meta a`. Terms of this type can be created via layer-related operations, as we'll see below.

### Introducing Layers

For every term in Neut, an integer value called _layer_ is defined. The layer of the body of a `define` is defined to be 0:

```neut
define foo(): unit {
  // here is layer 0
  Unit
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
    x // ← ERROR: 3 ≠ 0
  in

  ...
}

```

Only modality-related operations can change layers, as we'll see below.

### Creating Boxes

You can use `box` to construct terms of type `meta a`:

```neut
define use-box(x: &int, y: &bool): meta pair(int, bool) {
  // here is layer 0
  // x: &int
  // y: &bool
  box x, y {
    // here is layer -1
    // x: int
    // y: bool
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

We can extract values from a box using `letbox`.

```neut
define use-letbox(x: int, y: bool): bool {
  // here is layer 0
  // (x: int)
  // (y: bool)
  letbox value on x, y =
    // here is layer 1
    // (x: &int)
    // (y: &bool)
    box x, y { Pair(x, y) }
  in
  // here is layer 0
  value
}
```

More specifically, assuming `e1: meta t`, the syntax of `letbox` is as follows:

```neut
// here is layer n
// - y1: a1
// - ...
// - ym: am
letbox x on y1, ..., ym =
  // here is layer n+1
  // - y1: &a1
  // - ...
  // - ym: &am
  e1
in
// here is layer n
// - x: t
// - y1: a1
// - ...
// - ym: am
e2
```

Operationally, `letbox` behaves as follows:

```neut
letbox x on y1, ..., ym = e1 in
e2

↓

// pseudo-code
let y1 = cast(a1, &a1, y1) in // cast y1: a1 → &a1
...                           // ...
let ym = cast(am, &am, ym) in // cast ym: am → &am
let x = e1 in
let y1 = cast(&a1, a1, y1) in // cast y1: &a1 → a1
...                           // ...
let ym = cast(&am, am, ym) in // cast ym: &am → am
e2
```

The `on y1, ..., yn` part can be omitted.

## Auxiliary Tools for Boxes

### Using Boxes Without Changing the Current Layer

Neut has a variant of `letbox`, called `letbox-T`. This is basically the same as `letbox`. The only difference is that it doesn't change layers:

```neut
define use-letbox-T(x: int, y: bool): int {
  // here is layer 0
  // (x: int)
  // (y: bool)
  letbox-T value on x, y =
    // here is layer 0 (not 1!)
    // (x: &int)
    // (y: &bool)
    quote {10}
  in
  // here is layer 0
  value
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

Using `box` introduced before, we can turn a `bool` into `meta bool` by doing something like this:

```neut
define box-bool(b: bool): meta bool {
  if b {
    box {True}
  } else {
    box {False}
  }
}
```

This kind of translation can be mechanically done on "simple" types. To make things less tedious, Neut provides a syntactic construct `quote` that bypasses these translations.

Using `quote`, the above `box-bool` can be rewritten as follows:

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

and this is why the type of `e1` must be restricted to some extent. We can see that those restrictions come from `quote`.

### Desugar: Embodying

Using the `axiom-T` that we've defined, we can desugar `*e` as follows:

```neut
*e

↓ // desugar

let x = e in
axiom-T(box x {x})
```

## Additional Notes

### Layer Closedness of Functions

There's one last condition: for any free variable `x` of a function `f`, `layer(x) <= layer(f)` must hold. For example, the following is not a valid term:

```neut
define use-function(x: meta int): meta () -> int {
  // layer 0
  let x = 10 in
  box {
    // layer -1
    function () {
      letbox value = x in
      value
    }
  }
}
```

since `layer(x) = 0 > -1 = layer(f)`, where `f` is the anonymous function.

If it were not for this condition, the following would be well-typed and well-layered:

```neut
define joker(): () -> unit {
  // layer 0
  let xs: list(int) = [1, 2, 3] in
  letbox f on xs =
    // layer 1
    // xs: at 1
    box {
      // layer 0
      // 💫
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

The inner `function`, which contains `xs: &list(int)`, is bound to `f` after evaluating the outer `letbox`. Thus, we would be able to cause the dreaded use-after-free by deallocating `xs` and then calling the function `f`.
