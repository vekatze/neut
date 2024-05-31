# Primitives

## Table of Contents

- [Primitive Types and Functions](#primitive-types-and-functions)
- [Core Types and Functions](#core-types-and-functions)

## Primitive Types and Functions

### Primitive Types

Neut supports integers and floats. More specifically, the following types are supported:

- integer types: `intN (= int1, int2, int3, ..., int64)`
- float types: `floatN (= float16, float32, float64)`

You can also use `int` and `float`. These are a syntax sugar for target-dependent types. For example, if the target architecture is 64-bit, the `int` is an alias of `int64`. If the target is 32-bit, `int` is an alias of `int32`, etc.

### Primitive Functions

These primitive types have a lot of primitive functions from LLVM:

|             | integers                                                      | floats                                                                      |
| ----------- | ------------------------------------------------------------- | --------------------------------------------------------------------------- |
| arithmetics | add, sub, mul, div, rem, udiv, urem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                |
| comparison  | eq, ne, gt, ge, lt, le, ugt, uge, ult, ule                    | eq, gt, ge, lt, le, ne, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, all `add-int32`, `neg-float64`, `eq-int64`, and `gt-float32` are available.

Neut uses LLVM's integer types and float types for its primitive types. Thus, the primitive integer types in Neut are signless. More specifically, the signedness of an integer type in Neut resides in functions, not values.

For example, `div-int64` interprets its two arguments as signed integers and returns its (signed) result. `udiv-int64` interprets its two arguments as unsigned integers and returns its (unsigned) result. Integer functions prefixed with `u` are for unsigned functions.

The internal representation of the integer types in Neut is the same as that of LLVM. Therefore, they are based on the two's complement representation. That is why Neut doesn't have something like `uadd-int64`. The `u`-prefixed integer functions are there only when we need different behaviors for different signednesses.

For their detailed behaviors, please refer to [the LLVM language reference](https://llvm.org/docs/LangRef.html).

<div class="info-block">

Primitive functions for `int` and `float` are also available. For example, you can use `div-int`, `eq-int`, `add-float`, etc. These are aliases of the corresponding primitive functions. For example, if `int` is `int64`, the `div-int` is the same as `div-int64`.

</div>

## Core Types and Functions

The [preset](./modules.md#preset) of the core library (something like Prelude in other languages) is imported automatically and provides some types and functions. These types and functions can also be used as primitives. Things like the below are defined in the library:

```neut
data void {}

data unit {
| Unit
}

data bool {
| False
| True
}

data list(a) {
| Nil
| Cons(a, list(a))
}

// you can also write `?a` instead of `either(unit, a)`
data either(a, b) {
| Left(a)
| Right(b)
}

data pair(a, b) {
| Pair(left: a, right: b)
}
```

Functions for those types are also defined in the library. For more, please see [the source of the core library](https://github.com/vekatze/neut-core/tree/main/source).
