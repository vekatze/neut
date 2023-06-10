# Other Built-in Utilities

## Primitive Types and Operations

### Basics

Integers and floats are of course supported in Neut. For example, the factorial function will be written as follows:

```neut
define fact(x: i64): i64 {
  if eq-i64(x, 0) {
    1
  } else {
    mul-i64(x, fact(sub-i64(x, 1)))
  }
}
```

The above uses `i64` as an integer type. Other number types are also available:

- integer types: `iN (= i1, i2, i3, ..., i64)`
- float types: `fN (= f16, f32, f64)`

These types have a lot of primitive operations. These inherit from LLVM IR:

|             | integers                                                      | floats                                                                            |
|             | ----                                                          | ----                                                                              |
| arithmetics | add, sub, mul, div, rem, udiv, urem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                      |
| comparison  | eq, ne, gt, ge, lt, le, ueq, une, ugt, uge, ult, ule          | eq, gt, ge, lt, le, ne, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, all `add-i32`, `neg-f64`, `eq-i64`, and `gt-f32` are available.

Neut transparently uses LLVM's integer types and float types for its primitive types. This means, in particular, that the primitive integer types in Neut are "signless". That is, signedness of an integer type in Neut resides in operators, not in values.

For example, `div-i64` interprets its two arguments as signed integers, and returns its (signed) result. `udiv-i64` interprets its two arguments as unsigned integers, and returns its (unsigned) result. Integer operations prefixed with `u` are for unsigned operations.

The internal representation of the integer types in Neut is the same as that of LLVM. They are therefore based on the two's complement representation. That is why Neut doesn't have something like `uadd-i64`. The `u`-prefixed integer operations are there only when we need different behaviors for different signednesses.

For their detailed behaviors, please refer to [the language reference of LLVM](https://llvm.org/docs/LangRef.html). Also, as usual, please be careful when you compare floats. I can hear a faint voice from deep within my heart saying "I want to rename `eq`s for floats into something like `I-know-what-I-am-doing-and-still-want-to-check-if-two-floats-are-ordered-and-equal`".

### Platform-Dependent Integer Type

There also exists a type `int`. This is a syntax sugar for target-dependent type. That is, for example, if the target architecture is 64 bit, the `int` is an alias of `i64`. If the target is 32 bit, `int` is an alias of `i32`, etc.

Primitives operations for `int` are also available. For example, you can use `div-int`, `eq-int`. These are aliases of the corresponding primitive operations; If `int` is `i64`, the `div-int` is the same as `div-i64`.

### Memory Behavior

The cloning of operation for primitive values returns the original values. The discarding operation for them does nothing.

## Core Types

The core library (something like Prelude in other languages) is imported automatically and provides basic ADTs and functions. Things like the below are defined in the library:

```neut
data void {}

data unit {
- Unit
}

data bool {
- False
- True
}

data list(a) {
- Nil
- Cons(a, list(a))
}

// Additional notes on `list`:
// (1) you can write `x :: xs` instead of `Cons(x, xs)`
// (2) you can write `[x, y]` instead of `Cons(x, Cons(y, Nil))`

data option(a) {
- None
- Some(a)
}

data either(a, b) {
- Left(a)
- Right(b)
}

// Additional notes on `either`:
// (1) you can write `option(a)` instead of `either(unit, a)`
// (2) you can write `option(a)` as `?a`.
// (3) you can write `None` instead of `Left(Unit)`
// (4) you can write `Some(e)` instead of `Right(e)

data both(a ,b) {
- Both(left: a, right: b)
}
```

Basic operations for those types are also defined in the library. For more, please see [the source of the core library](https://github.com/vekatze/neut-core/tree/main/source).

## Other Language Features

### Inline Functions

You can define inline functions as follows:

```neut
inline increment(x: int): int {
  add-int(x, 1)
}
```

Inline functions are reduced at compile time.

You'll have to use `inline` when you want to define an alias of a type. Consider the following two definitions of `my-list`:

```neut
// (A)
inline my-list(a: tau): tau {
  list(a)
}

// (B)
define my-list(a: tau): tau {
  list(a)
}
```

If you write a definition like `(B)`, `my-list` is defined to be opaque. That is, the type inference algorithm won't know that `my-list(a)` is the same as `list(a)`. Therefore, for example, the following code won't pass type checking:

```neut
define foo(x: list(int)): my-list(int) {
  x
}
```

To tell this equivalence, you'll have to use `inline` instead.

### Type Alias

You can define a type alias as follows:

```neut
alias my-int {
  i64
}
```

The above is essentially the same as below:

```neut
inline my-int(): tau {
  i64
}
```

The difference lies in the fact that you don't have to "call" the type if you use `alias`:

```neut
// when you use `alias`
define use-my-int(x: my-int) {
  ...
}

// when you use `inline`
define use-my-int(x: my-int()) {
  ...
}
```

which relieves code cluttering.

### Tail Call Optimization

Neut optimizes all the tail calls. Thus, calculating length can be done faster using the following way:

```neut
define length-of-my-list[a](xs: my-list(a)): int {
  let helper =
    // (you can also define a recursive function in a function)
    mu get-length(ys: my-list(a), acc: int): int {
      match ys {
      - Nil =>
        acc
      - Cons(_, zs) =>
        get-length(zs, add-int(1, acc)) // tail call of `get-length`
      }
    }
  helper(xs, 0)
}
```
