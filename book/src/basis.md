# Basis

## Table of Contents

- [On Executing Types](./basis.md#on-executing-types)
- [Allocation Canceling](./basis.md#allocation-canceling)
- [Name Resolution](./basis.md#name-resolution)
- [Leading Bars and Trailing Commas](./basis.md#leading-bars-and-trailing-commas)
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
    let new-value = copy-value(value);
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
  let f = list(int);
  f(0, xs); // passing `0` to discard `xs`
  Unit
}
```

Note that the above example executes the type `list(int)` as a function.

### Copying Values

Let's see how types are executed when copying values. For example, consider the following code:

```neut
define foo(!xs: list(int)): unit {
  some-func(!xs, !xs)
}
```

Note that the variable `!xs` is used twice. Because of that, the compiler translates the above code into the below (pseudo-code; won't typecheck):

```neut
define foo(!xs: list(int)): unit {
  let f = list(int);
  let xs-clone = f(1, !xs); // passing `1` to copy `xs`
  some-func(xs-clone, !xs)
}
```

Note that the above example executes the type `list(int)` as a function.

You must prefix a variable with `!` if the variable needs to be copied. You must also prefix free variables in a term-level `define` with `!` if they cannot be copied for free.

The prefix `!` is unnecessary if the variable can be copied for free.

### On Immediate Values

We don't have to discard immediates like integers or floats because their internal representations don't depend on memory-related operations like `malloc` or `free`. Because of that, "discarding" immediate values does nothing. Also, "copying" immediate values means reusing original values.

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

Also, this function is internally called `"base.#.imm"`. Try compiling your project as follows:

```sh
neut build TARGET --emit llvm --skip-link
```

Then, take a peek at the `build` directory. You'll find the name here and there.

<div class="info-block">

Since every type is translated into a pointer to a function, a type is an immediate value. Thus, `type` is compiled into `base.#.imm`.

</div>

## Allocation Canceling

Thanks to its static nature, memory allocation in Neut can sometimes be optimized away. Consider the following code:

```neut
data int-list {
| Nil
| Cons(int, int-list)
}

// [1, 5, 9] => [2, 6, 10]
define increment(xs: int-list): int-list {
  match xs {
  | Nil =>
    Nil
  // ↓ the `Cons` clause
  | Cons(x, rest) =>
    Cons(add-int(x, 1), increment(rest))
  }
}
```

The expected behavior of the `Cons` clause above would be something like the following:

1. obtain `x` and `rest` from `xs`
2. `free` the outer tuple of `xs`
3. calculate `add-int(x, 1)` and `increment(rest)`
4. allocate memory region using `malloc` to return the result
5. store the calculated values to the pointer and return it

However, since the size of `Cons(x, rest)` and `Cons(add-int(x, 1), increment(rest))` are known to be the same at compile-time, the pair of `free` and `malloc` should be able to be optimized away, as follows:

1. obtain `x` and `rest` from `xs`
2. calculate `add-int(x, 1)` and `increment(rest)`
3. store the calculated values to `xs` (overwrite)

And Neut does this optimization. When a `free` is required, Neut looks for a `malloc` that is the same size and optimizes away such a pair if one exists. The resulting assembly code thus performs in-place updates.

### Allocation Canceling and Branching

This optimization "penetrates" branching. For example, consider the following:

```neut
// (an `insert` function in bubble sort)
define insert(v: int, xs: int-list): int-list {
  match xs {
  | Nil =>
    // ...
  | Cons(y, ys) =>           // (X)
    if gt-int(v, y) {
      Cons(y, insert(v, ys)) // (Y)
    } else {
      Cons(v, Cons(y, ys))   // (Z)
    }
  }
}
```

At point `(X)`, `free` against `xs` is required. However, this `free` can be canceled since `malloc`s of the same size can be found in all the possible branches (here, `(Y)` and `(Z)`). Thus, in the code above, the deallocation of `xs` at `(X)` is removed, and the memory region of `xs` is reused at `(Y)` and `(Z)`, resulting in an in-place update of `xs`.

On the other hand, consider rewriting the code above into something like the following:

```neut
define foo(v: int, xs: int-list): int-list {
  match xs {
  | Nil =>
    // ...
  | Cons(y, ys) =>         // (X')
    if gt-int(v, y) {
      Nil                  // (Y')
    } else {
      Cons(v, Cons(y, ys)) // (Z')
    }
  }
}
```

At this time, the `free` against `xs` at `(X')` can't be optimized away since there exists a branch (namely, `(Y')`) that doesn't perform `malloc` that is of the same size as `xs`.

## Name Resolution

### Resolving Module Aliases

Let's see how the name of a module alias is resolved. Here, the name of a module alias is something like the `core` in `core.text.io.get-line`:

```neut
import {
  core.text.io,
}

define use-external-module-function(): text {
           // 🌟
  let value = core.text.io.get-line();
  ...
}
```

When compiling a module, the compiler reads the field `dependency` in the `module.ens` and adds correspondences like the below to its internal state:

```neut
// alias => (the digest of the library)
core => "jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o"
foo-module => "JEpjuzZ0rlqxiVuCnD000jEKIA_Y6ku1L3J139h3M6Q"
bar-module => "zptXghmyD5druBl8kx2Qrei6O6fDsKCA7z2KoHp1aqA"
...
```

The compiler then resolves aliases like below:

```text
core.text.io.get-line

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=.text.io.get-line

--------------

foo-module.path.to.some.file.my-function

↓

JEpjuzZ0rlqxiVuCnD000jEKIA_Y6ku1L3J139h3M6Q.path.to.some.file.my-function

--------------

...
```

### Resolving `this`

Let's see how `this` is resolved. Here, `this` is a component of a global variables, like the below:

```neut
import {
  this.path.to.file,
}

define use-my-function(): text {
           // 🌟
  let value = this.path.to.file.my-function();
  ...
}
```

The first thing to note here is that every module is marked as "main" or "library" when running compilation. The main module is the module in which `neut build` is executed. Library modules are all the other modules that are necessary for compilation.

All the occurrences of `this` in the main module are kept intact during compilation. Thus, the resulting assembly file contains symbols like `this.foo.bar`.

On the other hand, all the occurrences of `this` in a library module are resolved into their corresponding digests. More specifically, when processing a library module, the compiler adds correspondences like the below:

```neut
// this => (the digest of the library)
this => "jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o"
```

The compiler then resolves `this` like below:

```text
this.text.io.get-line

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=.text.io.get-line
```

Thus, the resulting assembly file contains symbols like the above.

## Leading Bars and Trailing Commas

### Comma-Separated Sequences (And-Sequences)

Every comma-separated sequence like `a, b, c` can have a trailing comma like `a, b, c,`.

If a comma-separated sequence has a trailing comma, the sequence is formatted vertically by the built-in formatter.

### Bar-Separated Sequences (Or-Sequences)

Every bar-separated sequence like `a | b | c` can have a leading bar like `| a | b | c`.

If a bar-separated sequence has a leading bar, the sequence is formatted vertically by the built-in formatter.

## Compiler Configuration

The behavior of the compiler can be adjusted using the following environment variables:

| Environment Variable      | Meaning                       |
| ------------------------- | ----------------------------- |
| `NEUT_CLANG`              | the command to call `clang`   |
| `NEUT_CORE_MODULE_DIGEST` | the digest of the core module |
| `NEUT_CORE_MODULE_URL`    | the URL of the core module    |

The default values are as follows:

| Environment Variable      | Default Value                 |
| ------------------------- | ----------------------------- |
| `NEUT_CLANG`              | `clang`                       |
| `NEUT_CORE_MODULE_DIGEST` | (undefined; you must set one) |
| `NEUT_CORE_MODULE_URL`    | (undefined; you must set one) |

## Other Basic Facts

- Neut is call-by-value
- Neut is impure
- The type of `main` must be `() -> unit`
- A module named `core` is treated specially (treated as the "prelude" library)
  - Syntactic constructs like `[1, 2, 3]` depends on functions in `core`
