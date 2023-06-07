# Noetic Optimization

## Introducing Noetic Types

Let's introduce a new type `&list(a)`, the noetic variant of `list(a)`. Also, let's call a value a noema if its type is of the form `&a`.

A noema of type `&a` has the same internal representation as a value of type `a`. The difference lies in how the values of this type are copied/discarded; *a noema is treated as an immediate*.

More specifically, a noema is handled using the `exp-immediate` that we saw in the last section:

```neut
define exp-immediate(action-selector, v) {
  if action-selector == 0 {
    0 // do nothing against `v`
  } else {
    v // just use the original one as a copy
  }
}
```

We'll use noemata to deceive the type-oriented resource management system.

## Creating a Noema: Local Cast

Firstly, we need a way to create a noema. The syntax `let-on` is there for that purpose:

```neut
let x: a = (...)       // x: a
let result on x = {
  let y: &a = x        // x: &a
  100                  // x: &a
}
// outside
let z: a = x           // x: a
cont                   // x: a
```

`let-on` locally casts a value to a noema, and then casts back the value to its original type. In other words, `let-on` is essentially just a syntax sugar like the below:

```neut
let len on xs = e
cont

// ↓ desugar

let xs = unsafe-cast(a, &a, xs) // cast: `a` ~> `&a`
let len = e                     // (use `&a`)
let xs = unsafe-cast(&a, a, xs) // uncast: `&a` ~> `a`
cont
```

Note that

- you can't use the original `xs: A` in `e` since the name is shadowed by `xs: &A`, and that
- a new `xs: a` is defined after `let-on` by uncasting a noema, and thus can be used in `cont`.

As you can see from the definition of `let-on`, a noema always has its "source" value. We'll call it the hyle of a noema.

(You might have noticed that the answer type (the type of `result`) of a `let-on` needs to be restricted to some extent. This point is covered later in this section)

## Using a Noema: Viewing Its Content

Now that we can create noemata, we want a way to use them.

Using a noema, for example, the length of a list can be obtained as follows:

```neut
define length-noetic[a](xs: &list(a)): int {
  case xs {
  - [] =>
    0
  - y :: ys =>
    add-int(1, length-noetic(ys))
  }
}
```

Here, `case` does the same as `match` except that:

- it expects noetic ADTs like `&list(a)`, not ordinary ADTs like `list(a)`,
- it doesn't call `free` against its arguments (the `xs` here), and that
- the types of the constructor arguments are casted to be noetic.

The last one means, for example, the type of `y` in the example above is not `a` but `&a`. Similarly, The type of `ys` is not `list(a)`, but `&list(a)`. This ensures that the internal content of `xs` is kept intact. In this sense, you can think of `case` as a read-only variant of `match`.

Note that, although the `y: &a` is unused in the code above, since `&a` is a noetic type, this `y` isn't discarded, which is what we wanted.

## Using a Noema: Incarnation

Also, you can create a value of type `A` from a noema of type `&A`:

```neut
define sum-of-list(xs: &list(int)): int {
  case xs {
  - [] =>
    0
  - y :: ys =>
    add-int(*y, sum-of-list(ys))
  }
}
```

`*e` copies the argument along its type, keeping the original noema intact. In the example above, a term of type `int` is obtained from a term `y: &int`, by writing `*y`.

## Example: Get the Length of a List

Using `let-on` and `length-noetic`, we can obtain the length of a list as follows:

```neut
define test() {
  let xs: list(int) = [1, 2, 3]
  let len on xs = length-noetic(xs) // xs: &list(int)
  do-something(xs, len)
}
```

Note that the list `xs` isn't copied anymore. It might be said that this is something like "borrowing" in other languages.

## Result Type Restriction

A noema doesn't make sense if its hyle is discarded. This means, for example, we can break memory safety if `let-on` can return a noema:

```neut
let xs = [1, 2]
let result on xs = xs  // **CAUTION** the result of let-on is a noema
let _ = xs     // ← Since the variable `_` isn't used,
               // the hyle of `result`, namely `xs: list(int)`, is discarded here
match result { // ... and thus using `result` here is a use-after-free!
- [] =>
  print("hey")
- y :: ys =>
  print("yo")
}
```

Thus, we need to restrict the value `result` so that it can't contain any noemata. For example, types like `list(i64)`, `unit`, or `either(list(int), text)` are allowed. types like `&text`, `list(a)`, `int -> bool` are disallowed.

More specifically, the type of `result` must satisfy all of the followings:

- it doesn't contain any free variables,
- it doesn't contain any noetic types,
- it doesn't contain any function types (since a noema can reside in it), and
- it doesn't contain any "dubious" ADTs.

Here, a "dubious" ADT is an ADT like the below:

```neut
data joker-x {
- HideX(&A) // contains a noetic argument
}

data joker-y {
- HideY(int -> bool) // contains a functional argument
}

data joker-z {
- HideZ(joker-y) // contains a dubious ADT argument
}
```

Indeed, if we were to allow returning these dubious ADTs, we can exploit them to hide a noema:

```neut
let result on xs = HideX(xs) // the type of `result` is `jokerX` (dubious)
let _ = xs                   // `xs` is discarded here
match result {
- HideX(xs) =>
  *xs                        // CRASH: use-after-free!
}
```

This restriction is checked at compile time by the type system of Neut.

## A Possible Alternative: The ST Monadic Approach

Some (including me) might find the above restriction rather ad-hoc. Indeed, there exists an alternative, more seemingly principled approach. The approach essentially uses [the ST monad](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad-ST.html); Every noetic type is now of the form `&s A`, where the `s` corresponds to the `s` in `STRef s a`.

In this approach, `let-on` would internally use `runST: (forall s. ST s a) -> a` to ensure that the result doesn't depend on noetic values. Also, in this approach, the whole language will be made pure so that we can track every use of `*e` and `case`. This approach will be a variation of [Monadic State: Axiomatization and Type Safety](https://dl.acm.org/doi/abs/10.1145/258949.258970), or [Monadic Regions](https://dl.acm.org/doi/abs/10.1145/1016848.1016867).

Then, why Neut didn't take this more "principled" approach? This is because the virtue of noetic types lies in making our experience with certain fragments of the language better. Indeed, we can already do the same thing without noetic stuff by using unsafe casts appropriately. Although we can make the language pure and change the formulation of noemata into `&s A`, pursuing local generality here by modifying the whole language will go against the purpose of noetic types; They are there to provide certain safe patterns of uses of unsafe casts.

The language is already well-generalized. Noetic optimization is gold leaf for the cake.
