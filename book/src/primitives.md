# Primitives

## Table of Contents

- [Primitive Types and Functions](#primitive-types-and-functions)
- [Core Types and Functions](#core-types-and-functions)

## Primitive Types and Functions

### Primitive Types

Neut supports integers and floats. More specifically, the following types are supported:

- integer types: `intN (= int1, int2, int3, ..., int64)`
- float types: `floatN (= float16, float32, float64)`

You can also use `int` and `float`. These are just syntax sugar for `int64` and `float64`, respectively.

### Primitive Functions

These primitive types have a lot of primitive functions from LLVM:

|             | integers                                                      | floats                                                                      |
| ----------- | ------------------------------------------------------------- | --------------------------------------------------------------------------- |
| arithmetics | add, sub, mul, div, rem, udiv, urem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                |
| comparison  | eq, ne, gt, ge, lt, le, ugt, uge, ult, ule                    | eq, gt, ge, lt, le, ne, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, `add-int32`, `neg-float64`, `eq-int64`, and `gt-float32` are available.

Neut uses LLVM's integer types and float types for its primitive types. Thus, the primitive integer types in Neut are signless. More specifically, the signedness of an integer type in Neut resides in functions, not values.

For example, `div-int64` interprets its two arguments as signed integers and returns its (signed) result. `udiv-int64` interprets its two arguments as unsigned integers and returns its (unsigned) result. Integer functions prefixed with `u` are for unsigned functions.

The internal representation of the integer types in Neut is the same as that of LLVM. Therefore, they are based on the two's complement representation. That is why Neut doesn't have something like `uadd-int64`. The `u`-prefixed integer functions are there only when we need different behaviors for different signednesses.

The following conversion functions are also available: `trunc`, `zext`, `sext`, `fptrunc`, `fpext`, `fptoui`, `fptosi`, `uitofp`, and `sitofp`.

For example, `trunc-int64-int32`, `zext-int8-int32`, `fptoui-float32-int64`, and `uitofp-int64-float64` are available.

For their detailed behaviors, please refer to [the LLVM language reference](https://llvm.org/docs/LangRef.html).

<div class="info-block">

Primitive functions for `int` and `float` are also available. For example, you can use `div-int`, `eq-int`, `add-float`, etc.

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

// you can also write `?a` instead of `except(unit, a)`
data except(e, a) {
| Error(e)
| OK(a)
}

data pair(a, b) {
| Pair(left: a, right: b)
}
```

Functions for those types are also defined in the library. For more, please see [the source of the core library](https://github.com/vekatze/neut-core/tree/main/source).
