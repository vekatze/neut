# Modality and Memory

At first glance, the `let-on` stuff in the previous section might seem a bit artificial.

This `let-on` can actually be understood as a syntax sugar over the T-necessity operator. Below, we'll first see how Neut incorporates the necessity modality and then how `let-on` is desugared using the modality.

## Table of Contents

- [Introducing Layers](#copying-and-discarding-values)
- [□-Introduction: Putting Values into Boxes](#optimization-reusing-memory)
- [□-elimination: Extracting Values from Boxes](#optimization-avoiding-unnecessary-copies)
- [Combination of box and letbox](#optimization-avoiding-unnecessary-copies)
- [Quote: A Shorthand for Boxes](#optimization-avoiding-unnecessary-copies)
- [□-elimination-T: Unboxing within the Current Layer](#optimization-avoiding-unnecessary-copies)
- [Layer Closedness of Functions](#optimization-avoiding-unnecessary-copies)


## Introducing Layers

For every type `a`, Neut has a type `meta a`. As we will see, this `meta` is a necessity operator, often written as `□` in the literature.

In Neut, given `e: a`, you can create values of type `meta a` by writing something like `box {e}`. Here, the `e` is not arbitrary since, if so, we must admit propositions like `a -> □a`, making every truth a necessary truth.

In Neut, the condition that `e` must satisfy is described using _layers_. So, before using `box`, let's learn what layers are like.

### The Basics of Layers

For every term in Neut, an integer value called _layer_ is defined.

Let's see how layers are calculated. The layer of the body of a `define` is defined to be 0:

```neut
define foo(): unit {
  // here is layer 0
  Unit
}
```

If you define a variable at layer N, the layer of the variable is also N:

```neut
// here is layer N

let x = Unit in
x // ← `x` is a variable at layer N
```

The layer of (an occurrence of) a constant is defined to be the layer in which the constant is used:

```neut
define my-func(): int {
  10
}

define use-my-func() {
  // layer 0
  let v1 =
    my-func() // ← this `my-func` is at layer 0
  in

  ... // ← some layer operations here

  // layer 3 (for example)
  let v2 =
    my-func() // ← this `my-func` is at layer 3
  in

  ...
}
```

Terms that aren't related to modality won't change layers. For example, the following is the layer structure of `function` and `let`:

```neut
// here is layer N
function (x1: a1, ..., xn: an) {
  // here is layer N
  e
}

// here is layer N
let x =
  // here is layer N
  e1
in
// here is layer N
// (x: a at layer N)
e2
```

As long as you don't use modality-related terms, the layer of a term (and a subterm) is always 0.

### Layers and Variables

In Neut, _a variable defined at layer n can only be used at layer n_. For example, the following is not a valid term:

```neut
define bar(): unit {
  // here is layer 0
  let x = Unit in // ← `x` is defined at layer 0

  ... // ← some layer operations here

  // layer 3 (for example)
  let v2 =
    x // ← ERROR
  in

  ...
}

```

This is because the variable `x` is defined at layer 0 but used at layer 3 (≠ 0).

## □-Introduction: Putting Values into Boxes

Now that we know layers, we can talk about how to interact with values of type `meta a`.

### Syntax

A term of type `meta a` can be created using the syntactic construct `box`.

The syntax of `box` is as follows:

```neut
// here is layer (n+1)
// - x1: &a1 at (n+1)
// - ...
// - xm: &am at (n+1)
box x1, ..., xm {
  // here is layer n
  // - x1: a1 at n
  // - ...
  // - xm: am at n
  e1
}
```

Given a term `e: A`, the type of `box x1, ..., xn {e}` is `meta A`.

Note that the types of `xi`s must be of the form `&A`.

`box` turns `&a1, ..., &an` into `a1, ..., an` inside its body.

### Semantics

Operationally, `box x1, ..., xn { e }` copies all the `x1, ..., xn` and executes `e`:

```neut
box x1, ..., xn { e }

↓

// psueudo-code
let x1 = copy(x1) in
...
let xn = copy(xn) in
e
```

As you can see from the above semantics, terms of type `meta a` have the same forms as `a`. Thus, the type `meta a` is compiled into the same closed function as `a`.

### Example

Let's see some examples. Below is an example of `box`:

```neut
define some-function(x: &int): meta int {
  // here is layer 0
  // (x: &int at 0)
  box x {
    // here is layer -1
    // (x: int at -1)
    x
  }
}
```

The sequence `x1, ..., xn` can be empty. Thus, below is also a valid term:

```neut
define box-unit(): meta unit {
  // here is layer 0
  box {
    // here is layer -1
    Unit
  }
}
```

On the other hand, below isn't a valid term:

```neut
define some-function(x: bool): meta bool {
  // here is layer 0
  box {
    // here is at layer -1
    not(x)
  }
}
```

This is because the variable `x` is defined at layer 0 but used at layer -1.

## □-elimination: Extracting Values from Boxes

We can extract values from a box using `letbox`.

### Syntax

The syntax of `letbox` is as follows:

```neut
letbox x on y1, ..., ym =
  e1
in
e2
```

Or, with a bit verbose comments on layers and types:

```neut
// here is layer n
// - y1: a1 @ n
// - ...
// - ym: am @ n
letbox x on y1, ..., ym =
  // here is layer (n+1)
  // - y1: &a1 @ (n+1)
  // - ...
  // - ym: &am @ (n+1)
  e1
in
// here is layer n
// - y1: a1 @ n
// - ...
// - ym: am @ n
e2
```

Given a boxed term `e1: meta a`, `letbox` binds it to `x: a`, and executes `e2`.

### Semantics

The operational semantics of `letbox` is as follows:

```neut
letbox x on y1, ..., ym = e1 in
e2

↓

// pseudo-code
let y1 = cast(from=a1, to=&a1, y1) in
...
let ym = cast(from=am, to=&am, ym) in
let x = e1 in
let y1 = cast(from=&a1, to=a1, y1) in
...
let ym = cast(from=&am, to=am, ym) in
e2
```

### Examples

Let's see some examples. Below is a simple example of `letbox`:

```neut
define use-letbox(): bool {
  let x = True in
  // here is layer 0
  letbox value =
    // here is layer 1
    box {
      // here is layer 0
      x
    }
  in
  // here is layer 0
  value
}
```

A bit more complex example:

```neut
// helper function
define from-noema(x: &bool): meta bool {
  box x {
    x
  }
}

define use-letbox(): bool {
  let x = True in
  let y = True in
  // here is layer 0
  // - x: bool @ 0
  // - y: bool @ 0
  letbox value on x =
    // here is layer 1
    // - x: &bool @ 1
    // - y:  bool @ 0
    from-noema(x)
  in
  value
}
```

Below isn't a well-layered term:

```neut
define use-letbox(x: meta bool): bool {
  // here is layer 0
  letbox value =
    // here is layer 1
    x // ← error: the layer of `x` is 0 but used at layer 1
  in
  x
}
```

## Combination of `box` and `letbox`

Let's see how `box` and `letbox` work in harmony with each other.

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

### Creating and Embodying a Noema

The following code creates a noema using `letbox` and embodies it using `box`:

```neut
define test-embody(): unit {
  let x: int = 1 in
  letbox result on x = // ← creates a noema
    box x { // ← embodies a noema
      add-int(x, 2)
    }
  in
  print-int(result) // → "3"
}
```

### Borrowing a List

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

## Quote: A Shorthand for Boxes

As in above, we can turn a `bool` into `meta bool` by doing something like this:

```neut
define wrap-bool(b: bool): meta bool {
  if b {
    box {True}
  } else {
    box {False}
  }
}
```

You might find it a bit wordy. Indeed, this translation can be mechanically done on certain "simple" types. For example, we can do the same to `either(bool, unit)`:

```neut
define wrap-either(x: either(bool, unit)): meta either(bool, unit) {
  match x {
  | Left(b) =>
    if b {
      box {Left(True)}
    } else {
      box {Left(False)}
    }
  | Right(u) =>
    box {Right(Unit)}
  }
}
```

We just have to decompose values and reconstruct them with `box` added.

Neut has a syntactic construct `quote` that bypasses these manual translations. Using `quote`, the above two functions can be rewritten into the following functions:

```neut
define wrap-bool(b: bool): meta bool {
  quote {b}
}

define wrap-either(x: either(bool, unit)): meta either(bool, unit) {
  quote {x}
}
```

The example of `is-empty` can now be rewritten as follows:

```neut
define foo(): unit {
  let xs: list(int) = [1, 2, 3] in
  // layer 0
  // (xs: list(int) @ 0)
  letbox result on xs =
    // layer 1
    // (xs: &list(int) @ 1)
    quote {is-empty(xs)}
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

`quote` cannot be used against types that might contain types of the form `&a` or `(a) -> b`. For example, `quote` cannot be applied against values of the following types:

- `&list(int)`
- `(int) -> bool`
- `either(bool, &list(int))`
- `either(bool, (int) -> bool)`

`quote` is after all just a shorthand.

## □-elimination-T: Unboxing within the Current Layer

Remember the example of `is-empty`:

```neut
define borrow-and-check-if-empty(): unit {
  let xs: list(int) = [1, 2, 3] in
  letbox result on xs =
    // here is layer 1
    let b = is-empty(xs) in
    (..)
  in
  (..)
}
```

Although the above term is valid, the term obtained by parameterizing `is-empty` is not valid:

```neut
define borrow-and-check-if-empty(is-empty: (&list(int)) -> bool): unit {
  let xs: list(int) = [1, 2, 3] in
  letbox result on xs =
    // here is layer 1
    let b = is-empty(xs) in // ← error
    (..)
  in
  (..)
}
```

This is because the variable `is-empty` is defined at layer 0 but used at layer 1.

`letbox-T` is the loophole for such situations.

### Syntax

The syntax of `letbox-T` is as follows:

```neut
// here is layer n
// (y1: a1): a variable at layer n
// ...
// (ym: am): a variable at layer n
letbox-T x on y1, ..., yn =
  // here is layer n
  // (y1: &a1): a variable at layer n
  // ...
  // (ym: &am): a variable at layer n
  e1
in
// here is layer n
// (y1: a1): a variable at layer n
// ...
// (ym: am): a variable at layer n
e2
```

That is, `letbox-T` is the same as `letbox` except that it doesn't change the layer structure.

### Semantics

The operational semantics of `letbox-T` is again the same as `letbox`:

```neut
letbox-T x on y1, ..., ym = e1 in
e2

↓

let y1 = unsafe-cast(a1, &a1, y1) in
...
let ym = unsafe-cast(am, &am, ym) in
let x = e1 in
let y1 = unsafe-cast(&a1, a1, y1) in
...
let ym = unsafe-cast(&am, am, ym) in
e2
```

### Example: Parameterizing a Callback

Using `letbox-T`, we can parameterize `is-empty` as follows:

```neut
define borrow-and-check-if-empty(is-empty: (&list(int)) -> bool): unit {
  let xs: list(int) = [1, 2, 3] in
  // layer 0
  // (xs: list(int) @ 0)
  // (is-empty: &list(int) -> bool @ 0)
  letbox-T result on xs =
    // layer 0
    // (xs: &list(int) @ 0)
    // (is-empty: &list(int) -> bool @ 0)
    let b = is-empty(xs) in
    (..)
  in
  // layer 0
  // (xs: list(int) @ 0)
  // (is-empty: &list(int) -> bool @ 0)
  (..)
}
```

Note that the body of `letbox-T` in the example above is not layer 1 but layer 0.

### Example: The Axiom T in Neut

You can prove the axiom T by using `letbox-T`:

```neut
// Axiom T: □a -> a
define axiom-T<a>(x: meta a): a {
  letbox-T tmp = x in
  tmp
}
```

Note that the following is not well-layered:

```neut
define axiom-T<a>(x: meta a): a {
  letbox tmp = x in
  tmp
}
```

since the variable `x` is defined at layer 0 but used at layer 1.

In this sense, the `meta` is a necessity operator that satisfies the axiom T.

(I know this is a bit too informal, but anyway)

### Example: Desugaring let-on Using the T-necessity

Now we can desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓

letbox-T x on y, z = quote {e1} in
e2
```

and this is why the type of `e1` must be restricted to some extent. Now we can see that those restrictions come from `quote`.

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
