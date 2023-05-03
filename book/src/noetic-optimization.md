# Noetic Optimization

## Introducing Noetic Types

Let's define a new type `&list(a)`, the noetic type of `list(a)`, which isn't consumed *even after using it*. Also, let's call a value a noema if its type is of the form `&A`.

A noema of type `&A` has the same internal representation as a value of type `A`. The difference lies in how the terms of this type are copied/discarded; *a noema isn't copied/discarded*.

More specifically, a noema is handled using `exp-immediate` that we've seen in the last section:

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

We firstly need a way to create a noema. The syntax `let-on` is for that purpose:

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

As you can see from the definition of `let-on`, a noema always have its "source" value. We'll call it as the hyle of a noema.

## Using a Noema: Viewing Its Content

Using a noema, for example, the length of a list can be obtained as follows:

```neut
define length-noetic[a](xs: &list(a)): i64 {
  case xs {
  - [] =>
    0
  - y :< ys =>
    add-i64(1, length-noetic(ys))
  }
}
```

Here, `case` does the same as `match` except that:

- it expects noetic variant types like `&list(a)`, not variant types like `list(a)`,
- it doesn't deallocate its arguments (the `xs` here), and that
- the types of the constructor arguments are automatically made noetic.

The last one means, for example, the type of `y` in the example above is not `a` but `&a`. Similarly, The type of `ys` is not `list(a)`, but `&list(a)`.

Note that, although the `y: &a` is unused in the code above, since `&a` is a noetic type, `y` isn't actually discarded, which is what we wanted.

## Using a Noema: Incarnation

Also, you can create a value of type `A` from a noema of type `&A`:

```neut
define sum-of-list(xs: &list(i64)): i64 {
  case xs {
  - [] =>
    0
  - y :< ys =>
    add-i64(!y, sum-of-list(ys))
  }
}
```

`!e` copies the argument along its type, keeping the original noema intact. In the example above, a term of type `i64` is obtained from a term `y: &i64`, by writing `!y`.

## Example: Get the Length of a List

Using `let-on` and `length-noetic`, we can obtain the length of a list as follows:

```neut
define test() {
  let xs: list(i64) = [1, 2, 3]
  let len on xs = length-noetic(xs) // xs: &list(i64)
  do-something(xs, len)
}
```

Note that the list `xs` isn't copied anymore.

## Result Type Restriction

A noema doesn't make sense if its hyle is discarded. Because of that, `let-on` cannot return a noema as its result. Indeed, we can break memory safety if `let-on` can return a noema:

```neut
let xs = [1, 2]
let result on xs = xs // the result of let-on is a noema `xs`
let _ = xs // since the variable `_` isn't used,
           // the source `xs` is discarded here
match result { // ... and thus this results in use-after-free!
- [] =>
  print("hey")
- y :< ys =>
  print("yo")
}
```

Thus, we need to impose a restriction on the value `result`: It cannot contain any noemata.

This can be achieved by imposing the following restriction on the type of `result`:

- it cannot contain any `&A`s,
- it cannot contain function types (since a noema can reside in it), and
- it cannot contain any "dubious" variant types.

Also, a "dubious" variant type is a variant type like below:

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
let result on xs = hideX(xs) // result: jokerX
let _ = xs // `xs` is discarded here
match result {
- HideX(xs) =>
  !xs // CRASH: use-after-free!
}
```

This restriction is checked at compile time by the type system of Neut.

## A Possible Alternative: The ST Monadic Approach

Some (including me) might find the above restriction rather ad-hoc. There actually exists an alternative, more general appoarch. The approach essentially uses the ST monad; Every noetic type is now of the form `&s A`, where the `s` corresponds to the `s` in `STRef s a`.

In this approach, `let-on` would internally use `runST: (forall s. ST s a) -> a` to ensure that the result doesn't depend on noetic values. Also, in this approach, the whole language will be made pure so that we can track every use of `!e` and `case`. This approach will be a variation of [Monadic State: Axiomatization and Type Safety](https://dl.acm.org/doi/abs/10.1145/258949.258970), or [Monadic Regions](https://dl.acm.org/doi/abs/10.1145/1016848.1016867).

Then, why Neut didn't take this more general way? This is because the virtue of all these noetic stuff lies in making our experience with certain fragments of the language better. Yes, we can make the language pure and change the formulation of noemata into `&s A`, but pursuing local generality here by complicating the whole language goes against the purpose of the optimization.

The language is already baked and frosted. Noetic optimization is gold leaf for the cake.
