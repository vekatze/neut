# Modality and Memory

At first glance, the `let-on` stuff in the previous section might seem a bit artificial.

Interestingly (at least to me and hopefully to you), `let-on` can be understood as a syntax sugar over the T-necessity operator in modal logic. This section is for enjoying this charming formulation.

We'll first see how to use the T-necessity operator in Neut.

## What You'll Learn Here

- How layers in Neut is organized
- How to introduce "boxed" terms
- How to use "boxed" terms
- How the borrowing-like operation in Neut is organized using the T-necessity operator

## Layered Language

### Introducing Layers to the Language

Neut has a syntactic construct `box` that introduces the concept of layer to the language:

```neut
                        // `meta a` is a type of a box
define some-function(): meta bool {
  // here is layer 0
  box {
    // here is layer -1
    not(True)
  }
}
```

`box` is a syntactic construct that receives a term `e` at layer n and creates a term `box {e}` at layer n + 1.

As in the above example, every `define` starts at layer 0. Thus, the term `box {True}` is at layer 0. Since `box` lifts the layer of its argument, the layer of `True` in the above example must be -1.

### Box and Layer

In Neut, a variable defined at layer n can only be used at layer n. For example, the following is not a valid term:

```neut
define some-function(x: bool): meta bool {
  // here is layer 0
  box {
    // here is at layer -1
    not(x)
  }
}
```

since the variable `x` (at layer 0) is used at layer -1.

### Noema and Box

`box` can capture an external variable if the variable is a noema:

```neut
define some-function(x: &bool): meta bool {
  // here is layer 0
  // x: &bool (at layer 0)
  box x {
    // here is at layer -1
    // x: bool (at layer -1)
    not(x)
  }
}
```

The syntax of `box` is like the below:

```neut
box x1, ..., xn { e }
```

You can specify the variables that must be captured by `box` using `x1, ..., xn`. If the type of `xi` is `&ai`, you can use `xi: ai` in `e`.

Operationally, `box x1, ..., xn { e }` copies all the `x1, ..., xn` and executes `e`:

```neut
box x1, ..., xn { e }

↓

let x1 = copy(x1) in
...
let xn = copy(xn) in
e
```

## Using Boxes

We can extract values from a box using `letbox`:

```neut
define use-letbox(): {
  // here is layer 0
  letbox value =
    // here is layer 1
    box {True}
  in
  // here is layer 0
  value
}
```

The layer structure of `letbox` is as follows:

```neut
letbox x = e1 in e2
           ^^    ^^
                 ↑ layer n

           ↑ layer (n + 1)

^^^^^^^^^^^^^^^^^^^
↑ layer n
```

For example, the following is a valid term:

```neut
define use-letbox(x: bool): {
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

since, at this time, the variable `x` is defined and used at layer 0.

### The Axiom K in Neut

Now, we have rules to introduce and eliminate `meta a`. We can prove the axiom K in the literature by using these rules:

```neut
// Axiom K: □(a -> b) -> □a -> □b
define axiom-K<a, b>(f: meta (a) -> b, x: meta a): meta b {
  box {
    let f' = f in
    let x' = x in
    f'(x')
  }
}
```

In this sense, the `meta` is a necessity operator that satisfies the axiom K.

### letbox and `on`

Like `let-on`, `letbox` can take a list of variables:

```neut
define use-letbox(): {
  let x = True in
  let y = False in
  let z = Unit in
  // here is layer 0
  // (x: bool @ 0)
  // (y: bool @ 0)
  // (z: unit @ 0)
  letbox value on x, z = // ← borrowing x and z
    // here is layer 1
    // (x: &bool @ 1)
    // (y:  bool @ 0)
    // (z: &unit @ 1)
    box {True}
  in
  // here is layer 0
  // (x: bool @ 0)
  // (y: bool @ 0)
  // (z: unit @ 0)
  value
}
```

By using `on`, you can create a noema that can be used in `e1`. One may be led to say that you "borrow" variables in `e1`.

## Playing with the Layer Structure

To better understand layers, let's see how `box` and `letbox` work in harmony with each other.

The following code creates a noema using `letbox` and embodies it using `box`:

```neut
define test-embody(): unit {
  let x: int = 1 in
  // layer 0
  // (x: int @ 0)
  letbox result on x =
    // layer 1
    // (x: &int @ 1)
    box x {
      // layer 0
      // (x: int @ 0, obtained by cloning the noema)
      add-int(x, 2)
    }
  in
  // layer 0
  // (x: int)
  print-int(result) // → "3"
}
```

See how the variable `x` is passed through layers.

Let's see one more example, a more "borrowing"-like one. Suppose that we have the following function:

```neut
is-empty: (xs: &list(int)) -> bool
```

which returns `True` if and only if the input `xs` is empty. You can use this function via `box` and `letbox`:

```neut
define foo(): unit {
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

## Quote, Only for Convenience

In the example above, we turned a `bool` into `meta bool` by doing something like the below:

```neut
define wrap-bool(b: bool): meta bool {
  if b {
    box {True}
  } else {
    box {False}
  }
}
```

You might find it a bit wordy. Indeed, this translation can be mechanically done on some "simple" types. For example, we can do the same to `either(bool, unit)`:

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

## Using Boxes (T)

Remember the example of `is-empty`:

```neut
define foo(): unit {
  let xs: list(int) = [1, 2, 3] in
  letbox result on xs =
    let b = is-empty(xs) in
    (..)
  in
  (..)
}
```

Now, observe that the term obtained by parameterizing `is-empty` as follows is not valid:

```neut
define foo(is-empty: (&list(int)) -> bool): unit {
  let xs: list(int) = [1, 2, 3] in
  letbox result on xs =
    let b = is-empty(xs) in
    (..)
  in
  (..)
}
```

because the variable `is-empty` is defined at layer 0, but used at layer 1.

If you find it too restrictive (like me), you can use the additional syntactic construct `letbox-T`. `letbox-T` is the same as `letbox` except that it doesn't alter the layer structure:

```neut
letbox-T x = e1 in e2
             ^^    ^^
                   ↑ layer n

             ↑ layer n

^^^^^^^^^^^^^^^^^^^
↑ layer n
```

Note that `e1`, `e2`, and `letbox-T x = e1 in e2` are at the same layer.

Using `letbox-T`, we can parameterize `is-empty` as follows:

```neut
define foo(is-empty: (&list(int)) -> bool): unit {
  let xs: list(int) = [1, 2, 3] in
  // layer 0
  // (xs: list(int) @ 0)
  // (is-empty: &list(int) -> bool @ 0)
  letbox result on xs =
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

Thus `meta` satisfies the axiom K and the axiom T. `meta` is the T-necessity operator in this sense.

(I know this is a bit too informal, but anyway)

## Desugaring let-on

Now we can desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓

letbox-T x on y, z = quote {e1} in
e2
```

and this is why the type of `e1` has some restrictions. Now we can see that those restrictions come from `quote`.

If you're interested in the relation between `&a` and `meta a`, see the Note of the rule [box](terms.md#box). Spoiler: `&a` is a renamed `meta a` defined through structural rules.

## Layers and Functions

`function` in Neut also has a layer condition. That is, every free variable in a `function` must be at the layer of `function`. For example, the following is not a valid term:

```neut
define use-function(x: meta int): meta () -> int {
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

This restriction prohibits `letbox`es to return value that depends on borrowed noemata. For example, consider the following code that tries to return a function from `letbox`:

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
          let len = length(xs) in
          quote {Unit}
        in
        Unit
      }
    }
  in
  f
}
```

If it were not for the layer condition on functions, the term `joker` is well-typed. The inner function will depend on `xs`, so we would be able to cause the dreaded use-after-free by deallocating `xs` and then calling the function `f`.

However, we can reject this `joker` with the layer condition since the inner function is at layer 0 but uses a free variable `xs` at layer 1.

<div class="info-block">

You may think of layers as something roughly similar to lifetimes. The bigger the layer's level is, the shorter the layer's values live. In our example, the layer of `xs` is bigger than the enclosing function. Thus, `xs` is freed before the function, which causes use-after-free.

</div>

```neut
(start)

↓

[layer 0]

↓ // evaluate inside `e1` of a letbox

[layer 0, layer 1]

↓ // evaluate inside `e1` of a letbox

[layer 0, layer 1, layer 2]

↓ // finish evaluating `e1` of a letbox
  // (all the values at layer 2 are discarded before this point)

[layer 0, layer 1]

↓

...
```

## What You've Learned Here

- Layer structures of `box`, `letbox`, `letbox-T`
- Using `box` or `quote` to create terms of type `meta {..}`
- Using `letbox` or `letbox-T` to use terms of type `meta {..}`
- Decomposition of `let-on` into `letbox-T` and `quote`
