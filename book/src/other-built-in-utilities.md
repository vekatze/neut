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

The above uses `i64` for an integer type. Other number types are also available:

- integer types: `iN (= i1, i2, i3, ..., i64)`
- float types: `fN (= f16, f32, f64)`

These types have a lot of primitive operations. These inherit from LLVM IR:

|             | integers                                                      | floats                                                                            |
|             | ----                                                          | ----                                                                              |
| arithmetics | add, sub, mul, div, rem, udiv, urem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                      |
| comparison  | eq, ne, gt, ge, lt, le, ueq, une, ugt, uge, ult, ule          | eq, gt, ge, lt, le, ne, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, all `add-i32`, `neg-f64`, `eq-i64`, and `gt-f32` are available.

Neut uses LLVM's integer types and float types for its primitive types. This means, in particular, that the primitive integer types in Neut are "signless". That is, signedness of an integer type in Neut resides in operators, not in values.

For example, `div-i64` interprets its two arguments as signed integers, and returns its (signed) result. `udiv-i64` interprets its two arguments as unsigned integers, and returns its (unsigned) result. Integer operations prefixed with `u` are for unsigned operations.

The internal representation of integer types in Neut is (of course) the same as that of LLVM. They are therefore based on the two's complement representation. That why Neut doesn't have `uadd-i64` or alike. The `u`-prefixed integer operations are there only when we need different behaviors for different signednesses.

For their detailed behaviors, please refer to [the language reference of LLVM](https://llvm.org/docs/LangRef.html). Also, as usual, please be careful when you compare floats. I can hear a faint voice from deep within my heart saying "I want to rename `oeq` into `I-know-what-I-am-doing-and-still-want-to-check-if-two-floats-are-ordered-and-equal` or something like that".

### Platform-Dependent Integer Type

There also exists a type `int`. This is a target-dependent type. That is, for example, if the target architecture is 64 bit, the `int` is an alias of `i64`. If the target is 32 bit, `int` is an alias of `i32`, etc.

Primitives operations for `int` are also available. For example, you can use `div-int`, `srem-int`. These are aliases of the corresponding primitive operations; If `int` is `i64`, the `div-int` is the same as `div-i64`.

### Memory Behavior

A primitive value is stored as it is. Its copy is itself, and won't be discarded.

## Core Types

The core library (something like Prelude in other languages) is imported automatically and provides basic variant types and functions. Things like the below are defined in the library:

```neut
variant bottom {}

variant top {
- Unit
}

variant bool {
- False
- True
}

variant list(a) {
- Nil
- Cons(a, list(a))   // `Cons(x, xs)` can also be written as `x :: xs`
                     // `Cons(x, Cons(y, Nil))` can also be written as `[x, y]`
}

// you can write `option(a)` as `?a`
variant option(a) {
- None
- Some(a)
}

variant sum(a, b) {
- Left(a)
- Right(b)
}

struct product(a, b) by Product {
- left: a
- right: b
}
```

Basic operations for those types are also defined in the library. For more, see the library definition (FIXME: insert a link here).

## Other Language Features

### Inline Functions

You can define inline functions as follows:

```neut
inline increment(x: int): int {
  add-int(x, 1)
}
```

Inline functions are reduced at compile time.

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
