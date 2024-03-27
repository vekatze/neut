# Static Memory Management

Here, we'll see how to write performant programs in Neut.

## What You'll Learn Here

- How memory regions are handled in Neut
- How to bypass copying resources
- How Neut optimizes memory allocations/deallocations

## Linearity and Memory

In Neut, the content of a variable is copied along its type if the variable is used more than once. Consider the following code:

```neut
define foo(xs: list(int)): list(int) {
  let ys = xs in
  let zs = xs in
  some-func(ys);
  other-func(zs);
  xs
}
```

In the above code, the variable `xs` is used three times. Because of that, the content of `xs` is copied twice:

```neut
// pseudo-code
define foo(xs: list(int)): list(int) {
  let xs1 = copy-value-along-type(list(int), xs) in
  let xs2 = copy-value-along-type(list(int), xs) in
  let ys = xs1 in
  let zs = xs2 in
  some-func(ys);
  other-func(zs);
  xs
}
```

Also, the content of a variable is discarded along its type if the variable isn't used. Consider the following code:

```neut
define bar(xs: list(int)): unit {
  Unit
}
```

In the above code, since `xs` isn't used, the content of `xs` is discarded as follows:

```neut
// pseudo-code
define bar(xs: list(int)): unit {
  let xs1 = discard-along-type(list(int), xs) in
  Unit
}
```

Technically speaking, these discarding/copying operations also happen when the variable is an immediate value like an integer:

```neut
define buz(x: int): unit {
  Unit
}

↓


// pseudo-code
define bar(x: int): unit {
  let _ = discard-along-type(int, x) in
  Unit
}
```

In practice, copying/discarding operations on immediate values are optimized away.

If you're interested in how Neut achieves these discarding/copying operations, please see [How to Execute Types](./on-executing-types.md).

## The Problem

In Neut, the content of a variable is copied if the variable is used more than once. Now, suppose we defined a function `length` as follows:

```neut
define length(xs: list(int)): int {
  match xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    add-int(1, length(ys))
  }
}
```

Also, suppose that we used the function as follows:

```neut
define length-then-branch(xs: list(int)): unit {
  let len = length(xs) in
  if eq-int(len, 5) {
    some-function(xs)
  } else {
    other-function(xs)
  }
}
```

Note that the variable `xs` is used more than once. Therefore, the content of `xs` is copied just to calculate its length. That is a disaster. The end of the world. Every wish is crushed into pieces.

Luckily, Neut has a way to overcome this sadness.

## The Solution: Noema Type

We need a way to bypass this copying/discarding behavior. Here come _noema types_.

For any type `t`, Neut has a type `&t`. We'll call this the noema type of `t`. We'll also say that a term is noetic if the type of the term is a noema type.

Unlike ordinary terms, noetic terms aren't copied or discarded even when they are used non-linearly. By using this behavior, we can avoid the disaster we have just seen.

Let's see how it works. We first redefine `length`. If the type `t` is an ADT type, you can inspect its content using `case`:

```neut
define length(xs: &list(int)): int {
  case xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    add-int(1, length(ys))
  }
}
```

The main difference between `case` and `match` lies in the fact that `case` doesn't perform `free` against its arguments. Because of that, this new `length` uses the argument `xs` in a read-only manner. That is, `length` doesn't consume `xs`.

Also, note that the newly-bound variables in `case` are automatically wrapped with `&(_)`. For example, in the above example, the type of `ys` is not `list(int)`, but `&list(int)`.

The `length-then-branch` then becomes as follows:

```neut
define length-then-branch(xs: &list(int)): unit {
  let len = length(xs) in
  if eq-int(len, 5) {
    some-function(xs)
  } else {
    other-function(xs)
  }
}
```

Note that `xs` is used more than once. However, this `xs` isn't copied because the type of the variable is noetic.

## Creating a Noetic Value

The last piece for our running question is how to construct such noetic values. This can be done by `let-on`.

```neut
define create-and-use-noetic-values(): unit {
  let xs: list(int) = [1, 2, 3] in
  // 🌟
  let len on xs =
    // xs: &list(int)
    length(xs)
  in
  // xs: list(int)
  print-int(len)
}
```

You can add `on` to your `let`s. You'll add a comma-separated list of variables to the `on`. Variables specified there are then casted as a noema in the body of the `let`.

We'll call the content of noetic value `xs` a _hyle_. In the example, the hyle of `xs` at `length(xs)` is `[1, 2, 3]`.

`let-on` is essentially the following syntax sugar:

```neut
let result on x = e in
cont

// ↓ desugar

let x = unsafe-cast(a, &a, x) in // cast: `a` ~> `&a`
let result = e in                // (use `&a`)
let x = unsafe-cast(&a, a, x) in // uncast: `&a` ~> `a`
cont
```

The result of `let-on` (that is, `len` in this case) can't include any noetic term. This restriction is required so that a noetic value won't outlive its hyle. Please see the [corresponding part of the language reference](terms.md#result-type-restriction) for more information.

## Allocation Canceling

Let's see another aspect of Neut's memory management. Thanks to its static nature, memory allocation in Neut can sometimes be optimized away. Consider the following code:

```neut
data int-list {
- Nil
- Cons(int, int-list)
}

// [1, 5, 9] => [2, 6, 10]
define increment(xs: int-list): int-list {
  match xs {
  - Nil =>
    Nil
  - Cons(x, rest) =>
    let foo = add-int(x, 1) in
    let bar = increment(rest) in
    Cons(foo, bar)
  }
}
```

The expected behavior of the `Cons` clause in `increment` would be something like the following:

1. obtain `x` and `rest` from `xs`
2. `free` the outer tuple of `xs`
3. calculate `foo (= add-int(x, 1))` and `bar (= increment(rest))`
4. allocate memory region using `malloc` to represent `Cons(foo, bar)`
5. store the calculated values to the pointer and return it

However, the compiler knows the following two facts during compilation:

- The size of `Cons(x, rest)` and `Cons(foo, bar)` are the same
- The outer tuple of `Cons(x, rest)` will never be used after extracting its contents

Thanks to this knowledge, the compiler can optimize away the pair of `free` and `malloc`, as follows:

1. obtain `x` and `rest` from `xs`
2. calculate `foo (= add-int(x, 1))` and `bar (= increment(rest))`
3. store the calculated values to `xs` (overwrite)

The compiler indeed performs this optimization. When a `free` is required, the compiler looks for a `malloc` in the continuation that is the same size and optimizes away such a pair if one exists. The resulting assembly code thus performs in-place updates.

### How Effective Is This Optimization?

The performance benefit obtained by allocation canceling seems to be pretty significant.

Below is the result of benchmarking of a bubble sorting program. This test creates a random list of length `N` and performs bubble sort on the list.

![allocation canceling](./image/graph/allocation-canceling.png "allocation canceling")

This benchmark executes the following `sort` function:

```neut
data int-list {
- My-Nil
- My-Cons(int, int-list)
}

nominal {
- _insert(v: int, xs: int-list): int-list
}

// 🌟
inline _swap-gt(cond: bool, v: int, x: int, xs: int-list): int-list {
  if cond {
    My-Cons(x, _insert(v, xs))
  } else {
    My-Cons(v, My-Cons(x, xs))
  }
}

define _insert(v: int, xs: int-list): int-list {
  match xs {
  - My-Nil =>
    My-Cons(v, My-Nil)
  - My-Cons(y, ys) =>
    _swap-gt(gt-int(v, y), v, y, ys)
  }
}

define sort(xs: int-list, acc: int-list): int-list {
  match xs {
  - My-Nil =>
    acc
  - My-Cons(y, ys) =>
    sort(ys, _insert(y, acc))
  }
}

```

The above is the "faster" implementation of bubble sorting in Neut. The key person is `_swap-gt`. The above code defines `_swap-gt` as an inline function. Therefore, in `_insert`, the definition of `_swap-gt` is expanded, which makes allocation canceling of `My-Cons` in `_insert` possible.

The "slower" implementation can be obtained by replacing `inline` at the 🌟 with `define`. In this implementation, since the definition of `_swap-gt` can't be expanded in `_insert`, allocation canceling of `My-Cons` in `_insert` is not possible.

I also added the result of Haskell just for reference.

Additional notes:

- You can find the source files used in this benchmarking [here](https://github.com/vekatze/neut/placeholder).
- I used my M1 Max MacBook Pro to perform the above benchmarks.

If you're interested in more benchmarking results, please see [Benchmarks](./benchmarks.md).

## What You've Learned Here

- Neut uses noema types to bypass copying resources
- The compiler finds pairs of `malloc/free` that are the same size and optimizes them away

---

Now, you should be able to write programs in Neut.

If you're interested in how Neut achieves this memory management, see [How to Execute Types](./on-executing-types.md).
