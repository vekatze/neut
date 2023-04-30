# Noetic Optimization

## Problem: Seemingly Wasteful Cloning

The problem we treat here is that, since a variable is copied if it is used multiple times, we need to copy, for example, the whole list just to calculate its length:

```neut
define problematic() {
  let xs: list(i64) = [1, 2, 3]
  let len = length(xs) // the first use of xs
  if cond(len) {
    print("hey")
  } else {
    print("yo")
  }
  bar(xs) // the second use of xs
}

```

where the definition of `length` is:

```neut
 // [a] is implicit argument; automatically inserted by the type system
define length[a](xs: list(a)): i64 {
  // `match xs` inspects `xs` destructs it,
  // deallocating the outer tuple of `xs`
  match xs {
  - [] => // nil
    0
  - y :< ys => // cons
    add-i64(1, length(ys))
  }
}
```

The `xs` in `problematic` is copied and passed to `length`, and `length` consumes this list to calculate its length.

Indeed, you can see that the variable `y: a` isn't used in the code above. Thus the `y` is discarded along the type `a` immediately after it is defined.

This is obviously an unsatisfactory behavior; We don't want this cloning operation to happen here.

## Trying to Recover Linearity

The cause of cloning is to use a variable multiple times. Then, what will happen if the `length` also returns original `xs`? In this case, the function `problematic` can use the variable `xs` linearly:

```neut
define problematic() {
  let xs: list(i64) = [1, 2, 3]
  let tuple(len, xs) = length-modified(xs)
  if cond(len) {
    print("hey")
  } else {
    print("yo")
  }
  bar(xs)
}

```

Note that the variable `xs` is now used linearly; The one at `bar(xs)` is a fresh one returned by `length-modified`.

However, in this case, the definition of `length-modified` must be something that consumes all the xs and reconstruct it:

```neut
define length-modified[a](xs: list(a)): i64 * list(a) {
  match xs { // deallocates outer tuple of xs
  - [] =>
    tuple(0, xs)
  - y :< ys =>
    let tuple(n, ys') = length-modified(ys)
    tuple(add-i64(n, 1), y :< ys') // (B)
  }
}
```

This reformulation of `length` is unsatisfactory in that:

- it must allocate/deallocate tuples during its computation, and that
- it must reconstruct the whole list `xs`.

The whole reconstruction stuff of `xs` shouldn't have been necessary in the first place, because the second element of `length-modified(xs)` is always the same as `xs`. What we need is a "virtual", or "noetic" version of `xs` that isn't consumed even after using it. `match` against such a term shouldn't consume its argument; It must be something that just views the argument's internal structure.

...And we found a "cheatable" fragment. Let's cast a magic spell.

## Introducing Noetic Types

Let's define a new type `&list(a)`, the *noetic type* of `list(a)`, which isn't consumed *even after using it*. Also, let's call a value *a noema* if its type is of the form `&A`.

<!-- —Well, please don't care about the name too much. The implication of the name here is nothing more than "not consumed even after it is used". -->

Noemata of type `&A` has exactly *the same internal representation* as values of `A`. The difference lies in *how the terms of this type are copied/discarded*; no matter what `A` is, a noema of type `&A` is copied/discarded exactly the same as `i64`, `double`, etc. In other words, a noema isn't actually copied/discarded.

More specifically, a noema is copied/discarded using the exponential for immediates that we've seen in the last section:

```neut
define exp-immediate(action-selector, v) {
  if action-selector == 0 {
    0 // do nothing against `v`
  } else {
    v // just use the original one as a copy
  }
}
```

## Using a Noema: Viewing Its Content

Via the noetic way, the `length` function can be now rewritten as follows:

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

which is more or less the same as the original version of `length`. The difference is that we now use `case` instead of `match`. `case` does the same as `match` except that:

- it expects noetic variant types like `&list(a)`, not variant types like `list(a)`,
- it doesn't deallocate its arguments (the `xs` here), and that
- the types of the constructor arguments are also wrapped by `&`.

The last one might need additonal explanation. It means, for example, the type of `y` in the example above is not `a` but `&a`. Similarly, The type of `ys` is not `list(a)`, but `&list(a)`.

Now you can see that, for example, although the `y: &a` is unused in the code above, since `&a` is a noetic type, the "discarding" along `&a` does nothing against `y`. The original `y` is kept intact, which is what we wanted.

## Using a Noema: Incarnating Its Content

Also, you can create a value of type `A` from a noema by writing `!e`:

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

`!e` copies the argument along its type. If `e` is of type `&A`, `!e` copies the `e` along `A` and returns its clone, which is of type `A`, keeping the original noema intact.

In the example above, a term of type `i64` is obtained from a term `y: &i64`, by writing `!y`. It might be worth noting that, since the exponential of an immediate returns itself, the clone of the noema `y` is actually itself.

Using in combination with `case`, you can trace a data structure and clone specific portions if necessary.

## Creating a Noema: Local Cast

Of course, we need a way to create a noema. A new syntax `let-on` is introduced for that purpose:

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

`let-on` is a syntax that locally allow us to use noemata. More specifically,

(1) In the body of `let-on`, the type of `x` is casted into the corresponding noema. That is, if the type of `x` is `A`, the type of `x` inside its body is `&A`.

(2) `let ans on x = e` also defines new variable `x` for its continuation. Remember the code that use `length-modified`:

```neut
let tuple(len, xs) = length-modified(xs)
cont
```

You can easily see that the code above defines a new variable `xs` at `let tuple(len, xs)`, for its continuation `cont`. `let-on` does the same:

```neut
let len on xs = length-noetic(xs)
cont
```

The code above defines a new variable `xs: list(a)` at `let len on xs` for its continuation `cont`.

Incidentally, the internal of `let-on` is something like below:

```neut
let len on xs = e
cont

// ↓ desugar

let xs = unsafe-cast(A, &A, xs) // cast xs from A to &A
let len = e
let xs = unsafe-cast(&A, A, xs) // cast back to its original type
cont
```

Note that you can't use the original `xs: A` in `e` since the name is shadowed by `xs: &A`.

Here, you might notice that the type of `len` needs to be restricted to some extent. This point is covered later.

## Application: Reformulation Using `let-on`

Let's rewrite the `problematic` using `let-on` and `length-noetic`:

<!-- Using new `length-noetic` and `let-on`, the `problematic` is now rewritten as follows: -->

```neut
// actually not problematic anymore
define problematic() {
  let xs: list(i64) = [1, 2, 3]
  let len on xs = length-noetic(xs) // xs: &list(i64)
  if cond(len) {
    print("hey")
  } else {
    print("yo")
  }
  bar(xs) // xs: list(i64)
}
```

Now, we can calculate the length of `xs` without consuming it.

Or, suppose we wrote something like below:

```neut
  let len-double on xs = {
    let len1 = length-noetic(xs)
    let len2 = length-noetic(xs)
    add-i64(len1, len2)
  }
  ...
```

Although the `xs` is used twice, they aren't copied since the type of `xs` in the body of `let-on` is `&list(i64)`.

## Result Type Restriction

A noema is discarded when its "source" is discarded. This means that `let-on` cannot return a noema as its result. Indeed, we can easily break memory safety if `let-on` can return a noema:

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

Thus, we need some restriction on the type of `result`.

When using `let result on x = e`, `result` cannot contain any noema. More specifically, the type of `result`

- cannot contain any `&A`s, and
- cannot contain function types.

The type of `result` cannot contain a function type because the function might be using a noema:

```neut
let result on xs =
  lambda () {
    let cloned-value = !xs
    cloned-value
  }
let _ = xs // `xs` is discarded
result() // CRASH: use-after-free!
```

Also, the type of result cannot contain a variant type like below:

```neut
variant jokerX {
- HideX(&A)
}

variant jokerY {
- HideY(int -> bool)
}
```

since these types can be exploited to hide a noema:

```neut
let result on xs = hideX(xs) // result: jokerX
let _ = xs // `xs` is discarded
match result {
- HideX(xs) =>
  !xs // CRASH: use-after-free!
}
```

All in all, the type of `result` cannot contain any functions or noemata. This condition is checked at compile time by the type system of Neut.

## A Possible Alternative: The ST Monadic Approach

Some might find the above restriction rather ad-hoc. There actually exists an alternative, more general appoarch. The approach essentially uses the ST monad; Every noetic type is now of the form `&s A`, where the `s` corresponds to the `s` in `STRef s a`.

In this approach, `let-on` would internally use `runST: (forall s. ST s a) -> a` to ensure that the result doesn't depend on noetic values. Also, in this approach, the whole language will be made pure so that we can track every use of `!e` and `case`. This approach will be a variation of [Monadic State: Axiomatization and Type Safety](https://dl.acm.org/doi/abs/10.1145/258949.258970), or [Monadic Regions](https://dl.acm.org/doi/abs/10.1145/1016848.1016867).

Then, why Neut didn't take this more general way? This is because the virtue of all these noetic stuff lies in making our experience with certain fragments of the language better. Yes, we can make the language pure and change the formulation of noemata into `&s A`, but pursuing local generality here by complicating the whole language goes against the purpose of the optimization.

The language is already baked and frosted. Noetic optimization is gold leaf for the cake.
