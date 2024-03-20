# Static Memory Management

Here, we'll see how to write a performant program in Neut.

## What You'll Learn Here

- How to bypass copying resources
- How Neut optimizes memory allocations/deallocations

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

We need a way to bypass this copying/discarding behavior. Here comes the _noema types_.

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

The newly-bound variables in `case` are automatically wrapped with `&(_)`. For example, in the above example, the type of `ys` is not `list(int)`, but `&list(int)`.

This new `length` uses the argument `xs` in a read-only manner. This `length` doesn't consume `xs`.

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

The last piece is how to construct such noetic values. This can be done by `let-on`.

```neut
define create-and-use-noetic-values(): unit {
  let xs: list(int) = [1, 2, 3] in
  let len on xs =
    // xs: &list(int) ← 🌟 (`xs` is casted to a noetic value)
    length(xs)
  in
  // xs: list(int)
  print-int(len)
}
```

You can add `on` to your `let`s. In this case, you'll add a comma-separated list of variables to the `on`. Variables specified there are then casted as a noema in the body of the `let`.

The result of `let-on` (that is, `len` in this case) can't include any noetic term.

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
    Cons(add-int(x, 1), increment(rest))
  }
}
```

The expected behavior of the `Cons` clause above would be something like the below:

1. obtain `x` and `rest` from `xs`
2. `free` the outer tuple of `xs`
3. calculate `add-int(x, 1)` and `increment(rest)`
4. allocate memory region using `malloc` to return the result
5. store the calculated values to the pointer and return it

However, since

- the size of `Cons(x, rest)` and `Cons(add-int(x, 1), increment(rest))` are known to be the same at compile-time, and
- the outer tuple of `Cons(x, rest)` isn't used after extracting its contents,

the pair of `free` and `malloc` should be able to be optimized away, as follows:

1. obtain `x` and `rest` from `xs`
2. calculate `add-int(x, 1)` and `increment(rest)`
3. store the calculated values to `xs` (overwrite)

Neut performs this optimization. When a `free` is required, Neut looks for a `malloc` that has the same size and optimizes away such a pair if there exists one. The resulting assembly code thus performs in-place updates.

### How Effective Is This Optimization?

The performance benefit obtained by this optimization seems to be pretty significant, at least on my machine. It feels somewhat like tail call optimization.

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

---

Now, you should be able to write programs in Neut.

If you're interested in how Neut achieves this memory management, see [How to Execute Types](./on-executing-types.md).
