# Modality and Memory

At first glance, the `let-on` stuff in the previous section might seem a bit artificial.

This `let-on` can actually be understood as a syntax sugar over the T-necessity operator. Below, we'll first see how Neut incorporates the necessity modality and then how `let-on` is desugared using the modality.

## Table of Contents

- [Introducing Layers](#copying-and-discarding-values)
- [Basics of the Box Type](#copying-and-discarding-values)
  - [□-Introduction: Putting Values into Boxes](#optimization-reusing-memory)
  - [□-elimination: Extracting Values from Boxes](#optimization-avoiding-unnecessary-copies)
- [Utilities](#optimization-avoiding-unnecessary-copies)
  - [quote](#optimization-reusing-memory)
  - [Axiom T](#optimization-reusing-memory)
- [Decomposing Artificial-Looking Stuff](#decomposing-on)
  - [Quote: A Shorthand for Boxes](#optimization-avoiding-unnecessary-copies)
  - [□-elimination-T: Unboxing within the Current Layer](#optimization-avoiding-unnecessary-copies)
- [Miscs](#optimization-avoiding-unnecessary-copies)
  - [Borrowing via box and letbox](#optimization-avoiding-unnecessary-copies)

## Introducing Layers

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

## The Box Type

For every type `a`, Neut has a type `meta a`. As we will see, this `meta` is a necessity operator, often written as `□` in the literature.

### □-Introduction: Putting Values into Boxes

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

The sequence `x1, ..., xn` can be empty. Therefore, the following is valid:

```neut
define box-unit(): meta unit {
  // here is layer 0
  box {
    // here is layer -1
    Unit
  }
}
```

### □-elimination: Extracting Values from Boxes

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

By combining `box` and `letbox`, we can achieve operations similar to borrowing. See the appendix of this page if you're interested.

## Utilities

### Quote: A Shorthand for Boxes

Using `box` introduced above, we can turn a `bool` into `meta bool` by doing something like this:

```neut
define wrap-bool(b: bool): meta bool {
  if b {
    box {True}
  } else {
    box {False}
  }
}
```

This kind of translation can be mechanically done on "simple" types. To make things less tedious, Neut provides a syntactic construct `quote` that bypasses these translations.

Using `quote`, the above `wrap-bool` can be rewritten as follows:

```neut
define wrap-bool(b: bool): meta bool {
  quote {b}
}
```

`quote` cannot be used against types that might contain types of the form `&a` or `(a) -> b`. For example, `quote` cannot be applied against values of the following types:

- `&list(int)`
- `(int) -> bool`
- `either(bool, &list(int))`
- `either(bool, (int) -> bool)`

`quote` is after all just a shorthand.

### □-elimination-T: Unboxing within the Current Layer

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

Note that the following is not well-layered:

```neut
define axiom-T<a>(x: meta a): a {
  letbox tmp =
    // error: x is defined at layer 0 but used at layer 1
    x
  in
  tmp
}
```

## Desugaring

### Borrowing operations

Now we can desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓

letbox-T x on y, z = quote {e1} in
e2
```

and this is why the type of `e1` must be restricted to some extent. Now we can see that those restrictions come from `quote`.

### Embodying

Using the `axiom-T` that we've defined, we can desugar `*e` as follows:

```neut
define embody<a>(x: &a): a {
  axiom-T(box x {x})
}
```

## Miscs

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

The inner function (💫), which depends on `xs: &list(int)`, is bound to `f` after evaluating the outer `letbox`. Thus, we would be able to cause the dreaded use-after-free by deallocating `xs` and then calling the function `f`.

### The Axiom K in Neut

We can prove the axiom K in the literature using `box` and `letbox`:

```neut
// Axiom K: □(a -> b) -> □a -> □b
define axiom-K<a, b>(f: meta (a) -> b, x: meta a): meta b {
  box {
    letbox f' = f in
    letbox x' = x in
    f'(x')
  }
}
```

In this sense, the `meta` is a necessity operator that satisfies the axiom K.

<div class="info-block">

Don't confuse `meta (a) -> b` with `(meta a) -> b`.

</div>

### Borrowing via box and letbox

Let's take a look at a more "real-world" example (It's funny to say "real-world" when talking about modality). Suppose that we have the following function:

```neut
// returns `True` if and only if the input `xs` is empty.
is-empty: (xs: &list(int)) -> bool
```

You can use this function via `box` and `letbox`:

```neut
define borrow-and-check-if-empty(): unit {
  let xs: list(int) = [1, 2, 3] in
  // layer 0
  // (xs: list(int) @ 0)
  letbox result on xs =
    // layer 1
    // (xs: &list(int) @ 1)
    let b = is-empty(xs) in // ← using the borrowed `xs`
    if b {
      box {True}
    } else {
      box {False}
    }
  in
  // layer 0
  // (xs: list(int) @ 0)
  if result {
    print("xs is empty\n")
  } else {
    print("xs is not empty\n")
  }
}
```

In the above example, the variable `xs: list(int)` is turned into a noema by `letbox`, and then used by `is-empty`. Since `xs` is a noema inside the `letbox`, the `is-empty` doesn't have to consume the list `xs`.
