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

The above used `i64` for an integer type. There are other number types in Neut;:

- signed integer types: `iN (= i1, i2, i3, ..., i64)`
- unsigned integer types: `uN (= u1, u2, u3, ..., u64)`
- float types: `fN (= f16, f32, f64)`

These types have a lot of primitive operations. These inherit from LLVM IR:

|             | integers                                          | floats                                                                            |
|             | ----                                              | ----                                                                              |
| arithmetics | add, sub, mul, div, rem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                      |
| comparison  | eq, ne, gt, ge, lt, le                            | oeq, ogt, oge, olt, ole, one, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, all of `add-u32`, `neg-f64`, `eq-i64`, and `oeq-f32` are available.


For the detailed behaviors of them, please refer to [the language reference of LLVM](https://llvm.org/docs/LangRef.html). Also, as usual, please be careful when you compare floats. I can actually hear a faint voice from deep within my heart saying "I want to rename `oeq` into `I-know-what-I-am-doing-and-still-want-to-check-if-two-floats-are-ordered-and-equal` or something like that".

By the way, LLVM IR in itself doesn't distinguish signed integer types with unsigned integer types. Rather, it has operations for signed/unsigned integers. For example, LLVM has an operation `udiv` that calculates the quotient of two operands, regarding both of them as unsigned integers. Neut, on the other hand, distinguishes these two types, and provides operations for both of them. The `udiv` in LLVM is `div-uN` in Neut. The `div` in LLVM is `div-iN`. And so on.

### Memory Behavior

Primitive values are stored as it is, copied by using the original value, and won't be discarded.
<!-- The value is stored in the stack. Primitive values aren't actually copied or discarded even if they are used non-linearly. -->

## Core Types

The core library (something like Prelude in other languages) is imported automatically and provides basic variant types and functions. Things like below are defined in the library:

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
- Cons(a, list(a))   // `Cons(x, xs)` can also be written as `x :< xs`
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

You can define an inline functions as follows:

```neut
define-inline increment(x: i64): i64 {
  add-i64(x, 1)
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
define-inline my-int(): tau {
  i64
}
```

The difference lies in the fact that you don't have to "call" the type if you use `alias`:

```neut
// when you use `alias`
define use-my-int(x: my-int) {
  ...
}

// when you use `define-inline`
define use-my-int(x: my-int()) {
  ...
}
```

which relieves code cluttering.

### Tail Call Optimization

Neut optimizes all the tail calls. Thus, calculating length can be done faster using the following way:

```neut
define length-of-my-list[a](xs: my-list(a)): i64 {
  let helper =
    // (you can also define a recursive function in a function)
    define get-length(ys: my-list(a), acc: i64): i64 {
      match ys {
      - Nil =>
        acc
      - Cons(_, zs) =>
        get-length(zs, add-i64(1, acc)) // tail call of `get-length`
      }
    }
  helper(xs, 0)
}
```
