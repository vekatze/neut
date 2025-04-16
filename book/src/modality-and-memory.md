# Modality and Memory

Here, we'll see how to interact with the box modality `meta`, which enables borrowing in Neut. We'll then see that both `on` and `*e` can be understood as syntactic sugar over this modality, even if they might look somewhat artificial at first glance.

## Table of Contents

- [Layers and the Box Modality](#layers-and-the-box-modality)
- [More Tools for Boxes](#auxiliary-tools-for-boxes)
- [Desugaring Exotic Operations](#desugaring-exotic-operations)
- [Additional Notes](#additional-notes)

## Layers and the Box Modality

In Neut, each type `a` has a corresponding type `meta a`. This type provides a way to work with *layers*, which are similar to lifetimes in other languages.

Below, we’ll first introduce the concept of layers, and then see how to use this `meta a`.

### Layers and Variables

Every term in Neut has an integer layer. Conceptually, a layer can be seen as the level at which a piece of data or code lives. The body of a `define` starts at layer 0:

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

To create a term of type `meta a`, use `box`:

```neut
define use-box(x: &int, y: &bool, z: &text): meta pair(int, bool) {
  // here is layer 0
  // free variables:
  // - x: &int
  // - y: &bool
  // - z: &text
  box x, y {
    // here is layer -1 (== 0 - 1)
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

Operationally, `box x1, ..., xn { e }` copies all the `x1, ..., xn` and executes `e`:

```neut
box x1, ..., xn { e }

↓

// pseudo-code
let x1 = COPY(type-1, x1) in
...
let xn = COPY(type-n, xn) in
e
```

You can also omit the sequence `x1, ..., xn` entirely if no variables need to be copied.

### Using Boxes

To use a term of type `meta a`, use `letbox`:

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
    // - (z is unavailable here because of layer mismatch)
    box {42}
  in
  // here is layer 0
  // free variables:
  // - x: int
  // - y: bool
  // - z: text
  extracted-value // == 42
}
```

Some notes on `letbox`:

- The type of `yi` in `on y1, ..., yn` has no restriction.
- Given `yi: ai`, the type of `xi` in the body of `letbox x on y1, ..., yn = e1 in e2` is `&ai`.

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

The `on y1, ..., yn` in `letbox` is optional.

## Auxiliary Tools for Boxes

### Using Boxes Without Changing the Current Layer

<!-- Neut has a variant of `letbox`, called `letbox-T`. The only difference is that `letbox-T` doesn't shift layers: -->

Sometimes you want to use a term of type `meta a` without shifting your current layer. For this, Neut provides `letbox-T`. It keeps you in the same layer:

```neut
define use-letbox-T(x: int, y: bool): int {
  // here is layer 0
    letbox-T value on x, y =
    // here is layer 0 (not layer 1)
    box {42}
  in
  // here is layer 0
  value // == 42
}
```

`letbox-T` can be used for example to write functions of type `(meta a) -> a` as follows:

```neut
define axiom-T<a>(x: meta a): a {
  letbox-T tmp = x in
  tmp
}
```

If you tried to use `letbox` instead, you’d get an error because it would result in layer mismatch.

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

To streamline such mechanical lifting operation, Neut provides `quote`. It casts a term of simple type into its meta form:

```neut
define box-bool(b: bool): meta bool {
  quote {b} // directly constructs `meta bool` from `bool`
}
```

`quote` can't be used on types that might contain one of the following:

- a type of the form `&a`
- a type of the form `(a1, ..., an) -> b`
- a type variable

`quote` is after all a shorthand for simple types.

## Desugaring Exotic Operations

### Desugar: Borrowing

We can now desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓ // desugar

letbox-T x on y, z = quote {e1} in
e2
```

This explains why the type of `e1` must be restricted to some extent; those restrictions are from `quote`.

### Desugar: Embodying

Using the `axiom-T` we defined, we can desugar `*e` as follows:

```neut
*e

↓ // desugar

let x = e in
axiom-T(box x {x})
```

## Additional Notes

### Layers and Free Variables

Regarding layers, there's one last condition that must be satisfied. That is, if a function is defined at layer n, the layer of every free variable `x` in the function must satisfy `layer(x) <= n`.

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

The inner `function`, which contains `xs: &list(int)`, is bound to `f` after evaluating the outer `letbox`. Hence, a use-after-free could occur by deallocating `xs` before calling `f`. This possibility explains the restriction.
