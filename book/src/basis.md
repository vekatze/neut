# Basis

## Table of Contents

- [On Executing Types](./basis.md#on-executing-types)
- [Allocation Canceling](./basis.md#allocation-canceling)
- [Compiler Configuration](./basis.md#compiler-configuration)
- [Other Basic Facts](./basis.md#other-basic-facts)

## On Executing Types

A type in Neut is compiled into a pointer to a binary function like the below (pseudo-code):

```neut
define discard-or-copy-value(action-selector, value) {
  if eq-int(action-selector, 0) {
    discard-value(value);
    Unit
  } else {
    let new-value = copy-value(value) in
    new-value
  }
}
```

These functions are then used to discard/copy values when necessary.

### Discarding Values

Let's see how types are executed when discarding values. For example, consider the following code:

```neut
define foo(xs: list(int)): unit {
  Unit
}
```

Note that the variable `xs` isn't used. Because of that, the compiler translates the code above into the below (pseudo-code; won't typecheck):

```neut
define foo(xs: list(int)): unit {
  let f = list(int) in
  f(0, xs); // passing `0` to discard `xs`
  Unit
}
```

Note that the above example executes the type `list(int)` as a function.

### Copying Values

Let's see how types are executed when copying values. For example, consider the following code:

```neut
define foo(xs: list(int)): unit {
  some-func(xs, xs)
}
```

Note that the variable `xs` is used twice. Because of that, the compiler translates the above code into the below (pseudo-code; won't typecheck):

```neut
define foo(xs: list(int)): unit {
  let f = list(int) in
  let xs-clone = f(1, xs) in // passing `1` to copy `xs`
  some-func(xs-clone, xs)
}
```

Note that the above example executes the type `list(int)` as a function.

### On Immediate Values

We don't have to discard immediates like integers or floats because their internal representations don't depend on memory-related operations like `malloc` or `free`. Because of that, "discarding" immediate values does nothing. Also, "copying" immediate values just reuse original values.

More specifically, the type of an immediate is compiled into a pointer to the following function (pseudo-code):

```neut
inline discard-or-copy-immediate(selector, value) {
  if eq-int(selector, 0) {
    0     // discard: we have nothing to do on `value`
  } else {
    value // copy: we can simply reuse the immediate `value`
  }
}
```

These fake discarding/copying are optimized away at compile-time.

Also, this function is internally called `"base.#.imm"`. If you compile your project with:

```sh
neut build --emit llvm --skip-link
```

You'll find this name here and there.

<div class="info-block">

Since every type is translated into a pointer to a function, a type is an immediate value. Thus, the type of the types `tau` is compiled into `base.#.imm`.

</div>

## Allocation Canceling

Thanks to its static nature, memory allocation in Neut can sometimes be optimized away. Consider the following code:

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
  // ↓ the `Cons` clause
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

However, since the size of `Cons(x, rest)` and `Cons(add-int(x, 1), increment(rest))` are known to be the same at compile-time, the pair of `free` and `malloc` should be able to be optimized away, as follows:

1. obtain `x` and `rest` from `xs`
2. calculate `add-int(x, 1)` and `increment(rest)`
3. store the calculated values to `xs` (overwrite)

And Neut does this optimization. When a `free` is required, Neut looks for a `malloc` that has the same size and optimizes away such a pair if there exists one. The resulting assembly code thus performs in-place updates.

### Allocation Canceling and Branching

This optimization "penetrates" branching. For example, consider the below:

```neut
// (an `insert` function in bubble sort)
define insert(v: int, xs: int-list): int-list {
  match xs {
  - Nil =>
    // ...
  - Cons(y, ys) =>           // (X)
    if gt-int(v, y) {
      Cons(y, insert(v, ys)) // (Y)
    } else {
      Cons(v, Cons(y, ys))   // (Z)
    }
  }
}
```

At point `(X)`, `free` against `xs` is required. However, this `free` can be canceled, since `malloc`s of the same size can be found in all the possible branches (here, `(Y)` and `(Z)`). Thus, in the code above, the deallocation of `xs` at `(X)` is removed, and the memory region of `xs` is reused at `(Y)` and `(Z)`, resulting in an in-place update of `xs`.

On the other hand, consider rewriting the code above into something like the below:

```neut
define foo(v: int, xs: int-list): int-list {
  match xs {
  - Nil =>
    // ...
  - Cons(y, ys) =>         // (X')
    if gt-int(v, y) {
      Nil                  // (Y')
    } else {
      Cons(v, Cons(y, ys)) // (Z')
    }
  }
}
```

At this time, the `free` against `xs` at `(X')` can't be optimized away, since there exists a branch (namely, `(Y')`) that doesn't perform `malloc` which is of the same size as `xs`.

### How Effective Is This Optimization?

Please see [here](./resource-management.md#how-effective-is-this-optimization).

## Compiler Configuration

The behavior of the compiler can be adjusted using the following environment variables:

| Environment Variable      | Meaning                           |
| ------------------------- | --------------------------------- |
| `NEUT_CACHE_DIR`          | the directory used to save caches |
| `NEUT_CLANG`              | the command to call `clang`       |
| `NEUT_CORE_MODULE_DIGEST` | the digest of the core module     |
| `NEUT_CORE_MODULE_URL`    | the URL of the core module        |

The default values are as follows:

| Environment Variable      | Default Value                 |
| ------------------------- | ----------------------------- |
| `NEUT_CACHE_DIR`          | `$XDG_CACHE_HOME/neut`        |
| `NEUT_CLANG`              | `clang`                       |
| `NEUT_CORE_MODULE_DIGEST` | (undefined; you must set one) |
| `NEUT_CORE_MODULE_URL`    | (undefined; you must set one) |

## Other Basic Facts

- call-by-value
- impure
- the type of `main` must be `() -> unit`
