# Modality and Memory

<div class="info-block">

This section requires some familiarity with natural deduction.

</div>

At first glance, the `let-on` stuff in the previous section might seem a bit artificial.

Interestingly (at least to me and hopefully to you), `let-on` can be understood as a syntax sugar over the T-necessity operator in modal logic. This section is for enjoying this charming formulation.

We'll first see how to use the T-necessity operator in Neut.

## What You'll Learn Here

- How layers in Neut is organized
- How to introduce "boxed" terms
- How to use "boxed" terms
- How the borrowing-like operation in Neut is organized using the T-necessity operator

## Layered Language

### Layer

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

### letbox and `on`

Like `let-on`, `letbox` can take a list of variables:

```neut
define use-letbox(): {
  let x = True in
  // here is layer 0
  // x: bool (at layer 0)
  letbox value on x =
    // here is layer 1
    // x: &bool (at layer 1)
    box {True}
  in
  // here is layer 0
  // x: bool (at layer 0)
  value
}
```

By using `on`, you can create a noema that can be used in `e1`. You may be led to say that you can "borrow" variables in `e1`.

### Axiom K

You can prove the axiom K in modal logic by using `box` and `letbox`:

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

Here, the type of `e1` must be of the form `meta {..}`.

## Using Boxes (T)

The syntactic construct `letbox-T` is also available. `letbox-T` is the same as `letbox` except that it doesn't alter the layer structure:

```neut
define use-letbox(): {
  // here is layer 0
  letbox-T value =
    // here is layer 0
    box {True}
  in
  // here is layer 0
  value
}
```

Note that the `e1` part of `letbox-T` is layer 0, not layer 1.

### Axiom T

You can prove the axiom T in modal logic by using `letbox-T`:

```neut
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

## Quote, Only for Convenience

We can turn a value of type `bool` into `meta bool` as follows:

```neut
define wrap-bool(b: bool): meta bool {
  if b {
    box {True}
  } else {
    box {False}
  }
}
```

Also, we can do the same to `either(bool, unit)`:

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

The term `quote` can be used to wrap such "simple" types with `meta {..}`. Using `quote`, the above two functions can be rewritten into:

```neut
define wrap-bool(b: bool): meta bool {
  quote {b}
}

define wrap-either(x: either(bool, unit)): meta either(bool, unit) {
  quote {x}
}
```

### Desugaring let-on

Now we can desugar `let-on` as follows:

```neut
let x on y, z = e1 in
e2

↓

letbox-T x on y, z = quote {e1} in
e2
```

### □-introduction

The T-necessity modality in Neut is written as `meta a` and can be introduced as follows:

```neut
Γ1; ...; Γn; Δ ⊢ e1: a
------------------------------------- (□-intro)
Γ1; ...; Γn, &Δ ⊢ box Δ {e1}: meta a
```

where `Γ1; ...; Γn` is a sequence of contexts.

For example, the following derivation is valid:

```neut
--------------
x: int ⊢ x: int
---------------------------
x: int ⊢ add-int(x, 1): int
--------------------------------------------
x: &int ⊢ box x {add-int(x, 1)}: meta int
-------------------------------------------------------------
⊢ function (x) {box x {add-int(x, 1)}}: (&int) -> meta int
```

Thus, the following is a valid term in Neut:

```neut
define foo(x: &int): meta int {
  box x {
    add-int(x, 1)
  }
}
```

Note that

- the `x` in `box x {..}` must be a noetic type
- the `x` in `box x {..}` is cast to a non-noetic type inside `{..}`

Operationally, the term `box x1, ..., xn { e }` copies all the noemata `x1, ..., xn` and executes e.

By the way, what is the operator `&` from a logical perspective? To understand this, let's firstly see that if we set `Δ = (empty)` in the rule `(□-intro)`, the rule degenerates to the following:

```neut
Γ1; ...; Γn; · ⊢ e1: a
------------------------------------- (□-intro)
Γ1; ...; Γn ⊢ box Δ {e1}: meta a
```

which can be seen in the literature. Also, note that the following is an admissible rule:

```neut
Γ1; ...; Γn; x: b ⊢ e: a
-----------------------------
Γ1; ...; Γn, x: meta b ⊢ e: a
```

Thus, the rule `(□-intro)` is a rule obtained from the following admissible inference rule:

```neut
Γ1; ...; Γn; Δ ⊢ e1: a
---------------------------------------- (□-intro)
Γ1; ...; Γn, meta Δ ⊢ box Δ {e1}: meta a
```

by replacing the `meta Δ` with `&Δ`. That is to say, the operator `&` is a box modality defined structurally (and not by a proper inference rule).

### □-elimination (K)

The `meta a` in Neut can be eliminated (used) as follows:

```neut
Γ1; ...; Γn, &Δ ⊢ e1: meta a
Γ1; ...; Γn; Δ, Δ', x: a ⊢ e2: b
------------------------------------------------ (□-elim-K)
Γ1; ...; Γn; Δ, Δ' ⊢ letbox x on Δ = e1 in e2: b
```

For example:

```neut
------------------
Γ, x: int ⊢ x: int                                (..)
--------------------------------     -------------------------------------
Γ, x: &int ⊢ box x {x}: meta int     Γ; x: int, Δ', y: int ⊢ f(x, y): text
--------------------------------------------------------------------------
    Γ; x: int, Δ' ⊢ letbox y on x = box x {x} in f(x, y): int
```

Thus, the following term is well-typed:

```neut
function (x: int) {
  letbox y on x =
    box x {x}
  in
  add-int(x, y)
}
```

From the code above, you should be able to see that the variable `x: int` is borrowed as `x: &int` inside `letbox`.

### □-elimination (T)

Here, note that the following term is not valid in Neut:

```neut
function (x: meta int) {
  letbox y = x in
  add-int(x, y)
}
```

This is because the "layer" of `x` is not compatible with `letbox`. Remember the inference rule of `letbox`:

```neut
Γ1; ...; Γn, &Δ ⊢ e1: meta a
Γ1; ...; Γn; Δ, Δ', x: a ⊢ e2: b
------------------------------------------------ (□-elim-K)
Γ1; ...; Γn; Δ, Δ' ⊢ letbox x on Δ = e1 in e2: b
```

Note that the length of the context sequence of term `e1` is n, whereas that of `letbox` is n + 1.

...

Here comes the □-elimination of the T modality:

```neut
Γ1; ...; Γn, &Δ ⊢ e1: meta a
Γ1; ...; Γn, Δ, Δ', x: a ⊢ e2: b
-------------------------------------------------- (□-elim-T)
Γ1; ...; Γn, Δ, Δ' ⊢ letbox-T x on Δ = e1 in e2: b
```

Unlike `letbox`, `letbox-T` requires the term `e1` to reside in the same layer as `letbox-T`. Thus the following term is well-typed (and well-layered):

```neut
function (x: meta int) {
  letbox-T y = x in
  add-int(x, y)
}
```

Neut also has a syntactic construct `quote` that wraps a type with `meta`:

```neut
Γ1; ...; Γn ⊢ e1: a
(`a` is constant-like type such as int, bool, list(int), etc.)
---------------------------------------------------------------
Γ1; ...; Γn ⊢ quote {e1}: meta a
```

### let-on via T-necessity

`let-on` is a syntax sugar over `letbox-T`:

```neut
let x on y1, ..., yn =
  e1
in
e2

↓

letbox-T x on y1, ..., yn =
  quote {e1}
in
e2
```

For example, the following are valid derivations:

```neut
-----------------------------
xs: list(int) ⊢ xs: list(int)
--------------------------------------------
xs: &list(int) ⊢ box xs {xs}: meta list(int)
-------------------------------------------------------------
⊢ function (xs) {box xs {xs}}: (&list(int)) -> meta list(int)


```

`let-on` is actually a syntax sugar over the T-necessity operator in modal logic.
