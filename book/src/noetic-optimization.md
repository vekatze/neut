# Noetic Optimization

## Introducing Noetic Types

Let's define a new type `&list(a)`, the noetic type of `list(a)`, which isn't consumed *even after using it*. Also, let's call a value a noema if its type is of the form `&A`.

A noema of type `&A` has the same internal representation as a value of type `A`. The difference lies in how the terms of this type are copied/discarded; *a noema isn't copied/discarded*.

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

## Creating a Noema: Local Cast

Firstly, we need a way to create a noema. The syntax `let-on` is for that purpose:

```neut
let x: A = some-func() // x: A
let result on x = {
  let y: &A = x        // x: &A
  100                  // x: &A
}                      // x: &A
// outside
let z: &A = x          // x: A
cont                   // x: A
```

`let-on` locally casts a value to a noema. This is basically just a syntax sugar:

```neut
let len on xs = e
cont

// ↓ desugar

let xs = unsafe-cast(A, &A, xs) // cast `xs` from `A` to `&A`
let len = e                     // use `xs: &A` in `e`
let xs = unsafe-cast(&A, A, xs) // cast back to its original type
cont
```

Note that

- you can't use the original `xs: A` in `e` since the name is shadowed by `xs: &A`, and that
- a new `xs` is defined after `let-on`, and thus can be used in `cont`.

You might have noticed that the type of `len` needs to be restricted to some extent. This point is covered later in this section.

As you can see from the definition of `let-on`, a noema always has its "source" value. We'll call it the hyle of a noema.

## Using a Noema: Viewing Its Content

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

- it expects noetic variant types like `&list(a)`, not variant types like `list(a)`,
- it doesn't deallocate its arguments (the `xs` here), and that
- the types of the constructor arguments are automatically made noetic.

The last one means, for example, the type of `y` in the example above is not `a` but `&a`. Similarly, The type of `ys` is not `list(a)`, but `&list(a)`.

Note that, although the `y: &a` is unused in the code above, since `&a` is a noetic type, this `y` isn't discarded, which is what we wanted.

## Using a Noema: Incarnation

Also, you can create a value of type `A` from a noema of type `&A`:

```neut
define sum-of-list(xs: &list(int)): int {
  case xs {
  - [] =>
    0
  - y :: ys =>
    add-int(!y, sum-of-list(ys))
  }
}
```

`!e` copies the argument along its type, keeping the original noema intact. In the example above, a term of type `int` is obtained from a term `y: &int`, by writing `!y`.

## Example: Get the Length of a List

Using `let-on` and `length-noetic`, we can obtain the length of a list as follows:

```neut
define test() {
  let xs: list(int) = [1, 2, 3]
  let len on xs = length-noetic(xs) // xs: &list(int)
  do-something(xs, len)
}
```

Note that the list `xs` isn't copied anymore.

## Result Type Restriction

A noema doesn't make sense if its hyle is discarded. This means, for example, we can break memory safety if `let-on` can return a noema:

```neut
let xs = [1, 2]
let result on xs = xs // the result of let-on is a noema `xs`
let _ = xs // since the variable `_` isn't used,
           // the source `xs` is discarded here
match result { // ... and thus this results in use-after-free!
- [] =>
  print("hey")
- y :: ys =>
  print("yo")
}
```

Thus, we need to restrict the value `result` so that it can't contain any noemata.

This can be achieved by imposing the following restriction on the type of `result`:

- it can't contain any `&A`s,
- it can't contain function types (since a noema can reside in it), and
- it can't contain any "dubious" variant types.

Here, a "dubious" variant type is a variant type like the below:

```neut
variant joker-x {
- HideX(&A)
}

variant joker-y {
- HideY(int -> bool)
}
```

These must be rejected since they can be exploited to hide a noema:

```neut
let result on xs = hideX(xs) // the type of `result` is `jokerX` (dubious)
let _ = xs                   // `xs` is discarded here
match result {
- HideX(xs) =>
  !xs                        // CRASH: use-after-free!
}
```

This restriction is checked at compile time by the type system of Neut.

## A Possible Alternative: The ST Monadic Approach

Some (including me) might find the above restriction rather ad-hoc. Indeed, there exists an alternative, more general approach. The approach essentially uses the ST monad; Every noetic type is now of the form `&s A`, where the `s` corresponds to the `s` in `STRef s a`.

In this approach, `let-on` would internally use `runST: (forall s. ST s a) -> a` to ensure that the result doesn't depend on noetic values. Also, in this approach, the whole language will be made pure so that we can track every use of `!e` and `case`. This approach will be a variation of [Monadic State: Axiomatization and Type Safety](https://dl.acm.org/doi/abs/10.1145/258949.258970), or [Monadic Regions](https://dl.acm.org/doi/abs/10.1145/1016848.1016867).

Then, why Neut didn't take this more general approach? This is because the virtue of all these noetic stuff lies in making our experience with certain fragments of the language better. Yes, we can make the language pure and change the formulation of noemata into `&s A`, but pursuing local generality here by modifying the whole language will go against the purpose of the optimization.

The language is already well-generalized. Noetic optimization is gold leaf for the cake.
