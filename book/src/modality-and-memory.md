# Modality and Memory

At first glance, the `let-on` stuff in the previous section might seem a bit artificial.

Interestingly (at least to me and hopefully to you), `let-on` can be understood as a syntax sugar over the T-necessity operator in modal logic. Below, we'll first see how Neut incorporates the necessity modality and then how `let-on` is desugared using the modality.

## What You'll Learn Here

- How layers in Neut are organized
- How to introduce "boxed" terms
- How to use "boxed" terms
- How the borrowing-like operation in Neut is organized using the T-necessity operator

## Introducing the Concept of Layers

For every type `a`, Neut has a type `meta a`. As we will see, this `meta` is a necessity operator, often written as `□` in the literature.

In Neut, given `e: a`, you can create values of type `meta a` by writing something like `box {e}`. Here, the `e` is not arbitrary since, if so, we must admit `a -> □a`, which makes every truth a necessary truth.

Neut uses _layers_ to capture the condition that `e` must satisfy. So, before using `box` or whatever, let's learn what layers are like.

### The Basics of Layers

For every term (and subterm) in Neut, an integer value called _layer_ is defined.

The layer of the body of a `define` is defined to be 0:

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

In Neut, _a variable defined at layer n can only be used at layer n_. For example, the following is not a valid term:

```neut
define bar(): unit {
  // here is layer 0
  let x = Unit in // ← `x` is defined at layer 0

  ... // ← some layer operations here

  // layer 3 (for example)
  let foo =
    x // ← error: `x` is used at layer 3 (≠ 0)
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

Below, we'll see how modal inference rules interact with layers.

## □-Introduction: Putting Values into Boxes

Now that we have layers, we can talk about how to interact with values of type `meta a`.

### Syntax

A term of type `meta a` can be created using the syntactic construct `box`.

The syntax of `box` is as follows:

```neut
// here is layer (n + 1)
// (x1: &a1): a variable at layer (n + 1)
// ...
// (xm: &am): a variable at layer (n + 1)
box x1, ..., xm {
  // here is layer n
  // (x1: a1): a variable at layer n
  // ...
  // (xm: am): a variable at layer n
  e1
}
```

Given a term `e: A`, the type of `box x1, ..., xn {e}` is `meta A`.

Note that the types of `xi`s must be of the form `&A`. `box` turns the types of `xi`s into `A`s inside its body.

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
  // (x: &int): variable at layer 0
  box x {
    // here is layer -1
    // (x: int): variable at layer -1
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
// (y1: a1): a variable at layer n
// ...
// (ym: am): a variable at layer n
letbox x on y1, ..., ym =
  // here is layer (n+1)
  // (y1: &a1): a variable at layer (n+1)
  // ...
  // (ym: &am): a variable at layer (n+1)
  e1
in
// here is layer n
// (y1: a1): a variable at layer n
// ...
// (ym: am): a variable at layer n
e2
```

Given a term `e1: meta a`, `letbox` extracts the value from `e1` and binds to `x` and then executes `e2`.

The variables `y1, ..., yn` in the "outer" layer are temporarily borrowed as noemata in `e1`.

### Semantics

The operational semantics of `letbox` is as follows:

```neut
letbox x on y1, ..., ym = e1 in
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

### Examples

Let's see some examples. Below is an example of `letbox`:

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
define noema-to-meta(x: &bool): meta bool {
  box x {
    x
  }
}

define use-letbox(): bool {
  let x = True in
  let y = True in
  // *here is layer 0*
  // the list of free variables:
  // - x: bool @ 0
  // - y: bool @ 0
  letbox value on x =
    // *here is layer 1*
    // the list of free variables:
    // - x: &bool @ 1
    // - y:  bool @ 0
    noema-to-meta(x)
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

### Create and Embody a Noema

The following code creates a noema using `letbox` and embodies it using `box`:

```neut
define test-embody(): unit {
  let x: int = 1 in
  letbox result on x = // 💫 create a noema
    box x { // ← 💫 embody a noema
      add-int(x, 2)
    }
  in
  print-int(result) // → "3"
}
```

See how the variable `x` is passed through layers.

### Borrowing a List

Let's see a more "real-world" example (It's funny to talk about the real world when talking about modality). Suppose that we have the following function:

```neut
is-empty: (xs: &list(int)) -> bool
```

which returns `True` if and only if the input `xs` is empty. You can use this function via `box` and `letbox`:

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

We can turn a `bool` into `meta bool` by doing something like the below:

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

## □-elimination-T

Remember the example of `is-empty`:

```neut
define borrow-and-check-if-empty(): unit {
  let xs: list(int) = [1, 2, 3] in
  letbox result on xs =
    let b = is-empty(xs) in
    (..)
  in
  (..)
}
```

Although the above term is valid, the term obtained by parameterizing `is-empty` is not valid:

```neut
// not well-layered
define borrow-and-check-if-empty(is-empty: (&list(int)) -> bool): unit {
  let xs: list(int) = [1, 2, 3] in
  letbox result on xs =
    let b = is-empty(xs) in
    (..)
  in
  (..)
}
```

This is because the variable `is-empty` is defined at layer 0 but used at layer 1.

You can use `letbox-T` to bypass such situations.

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

That is, `letbox-T` is the same as `letbox` except that it doesn't alter the layer structure.

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

### Example

Using `letbox-T`, we can parameterize `is-empty` as follows:

```neut
define foo(is-empty: (&list(int)) -> bool): unit {
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

## Combination of `box` and `letbox-T`

### The Axiom T in Neut

You can prove the axiom T in modal logic by using `letbox-T`:

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

### Desugaring let-on Using the T-necessity

Now we can desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓

letbox-T x on y, z = quote {e1} in
e2
```

and this is why the type of `e1` must be restricted to some extent. Now we can see that those restrictions come from `quote`.

<div class="info-block">

If you're interested in the relation between `&a` and `meta a`, see the Note of the rule [box](terms.md#box). Spoiler: from the viewpoint of logic, `&a` is the same as `meta a` except that `&a` can only be used via "structural" rules.

</div>

## Layer Closedness of Functions

There's one last condition to require: every free variable in a `function` must be at the layer of `function`. For example, the following is not a valid term:

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

since the function is at layer -1, but the free variable `x` is at layer 0.

<!-- This restriction prohibits `letbox`es to return value that depends on borrowed noemata. For example, consider the following code that tries to return a function from `letbox`: -->

If it were not for the layer condition on functions, the following is well-typed and well-layered.

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

## What You've Learned Here

- Layer structures of `box`, `letbox`, `letbox-T`
- Using `box` or `quote` to create terms of type `meta {..}`
- Using `letbox` or `letbox-T` to use terms of type `meta {..}`
- Decomposition of `let-on` into `letbox-T` and `quote`
- The layer condition of functions
