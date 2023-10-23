# Other Built-in Utilities

## Primitive Types and Operations

### Basics

Integers and floats are of course supported in Neut. For example, the factorial function will be written as follows:

```neut
define fact(x: int64): int64 {
  if eq-int64(x, 0) {
    1
  } else {
    mul-int64(x, fact(sub-int64(x, 1)))
  }
}
```

The above uses `int64` as an integer type. Other number types are also available:

- integer types: `intN (= int1, int2, int3, ..., int64)`
- float types: `floatN (= float16, float32, float64)`

These types have a lot of primitive operations. These inherit from LLVM IR:

|             | integers                                                      | floats                                                                            |
|             | ----                                                          | ----                                                                              |
| arithmetics | add, sub, mul, div, rem, udiv, urem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                      |
| comparison  | eq, ne, gt, ge, lt, le, ueq, une, ugt, uge, ult, ule          | eq, gt, ge, lt, le, ne, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, all `add-int32`, `neg-float64`, `eq-int64`, and `gt-float32` are available.

Neut transparently uses LLVM's integer types and float types for its primitive types. This means, in particular, that the primitive integer types in Neut are "signless". That is, signedness of an integer type in Neut resides in operators, not in values.

For example, `div-int64` interprets its two arguments as signed integers, and returns its (signed) result. `udiv-int64` interprets its two arguments as unsigned integers, and returns its (unsigned) result. Integer operations prefixed with `u` are for unsigned operations.

The internal representation of the integer types in Neut is the same as that of LLVM. They are therefore based on the two's complement representation. That is why Neut doesn't have something like `uadd-int64`. The `u`-prefixed integer operations are there only when we need different behaviors for different signednesses.

For their detailed behaviors, please refer to [the language reference of LLVM](https://llvm.org/docs/LangRef.html). Also, as usual, please be careful when you compare floats. I can hear a faint voice from deep within my heart saying "I want to rename `eq`s for floats into something like `I-know-what-I-am-doing-and-still-want-to-check-if-two-floats-are-ordered-and-equal`".

### Platform-Dependent Primitive Types

You can also use `int`. This is a syntax sugar for target-dependent type. That is, for example, if the target architecture is 64 bit, the `int` is an alias of `int64`. If the target is 32 bit, `int` is an alias of `int32`, etc.

Primitives operations for `int` are also available. For example, you can use `div-int`, `eq-int`. These are aliases of the corresponding primitive operations; If `int` is `int64`, the `div-int` is the same as `div-int64`.

The type `float` is also available. This is the floating point version of `int`, and thus resolved into `float64` if the target is 64 bit. You can of course use primitive operations like `add-float`, `gt-float`, etc.

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

data except(a, b) {
- Fail(a)
- Pass(b)
}

// Additional notes on `except`:
// (1) you can write `option(a)` instead of `except(unit, a)`
// (2) you can write `option(a)` as `?a`.
// (3) you can write `None` instead of `Left(Unit)`
// (4) you can write `Some(e)` instead of `Right(e)

data pair(a, b) {
- Pair(left: a, right: b)
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
  int64
}
```

The above is essentially the same as below:

```neut
inline my-int(): tau {
  int64
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

### A Notation for (Not Necessarily) Monadic Binds

You can use Neut's `with` notation as something like the `do` notation in other languages:

```neut
// play with the `with` notation
define test(): except(&text, int) {
  with except-bind {
    bind x: bool = Pass(True) in   // x == True
    bind y: tau = Fail("error") in // returns `Fail("error"): except(&text, int)`
    Pass(10) // not executed
  }
}
```

where the `except-bind` is the bind operator of the except monad:

```neut
// monadic bind
define except-bind[e, a, b](x: except(e, a), k: a -> except(e, b)): except(e, b) {
  match x {
  - Fail(err) => Fail(err)
  - Pass(value) => k(value)
  }
}
```

Although the `binder` in `with binder { .. }` is a monadic bind in the case above, it can be any term as long as it typechecks.

Let's take a look at a slightly more complex example:

```neut
define test(): except(&text, int) {
  with except-bind {
    let _ = tau in   // `let` can be used in `with` as usual
    print("hey");    // `e1; e2` can also be used in `with` as usual
    bind _: bool =
      bind _: bool = Pass(True) in // `bind` is nestable
      Fail("hello")
    in
    Pass(10)
  }
}
```

### Tail Call Optimization

Neut optimizes all the tail calls. Thus, for example, the following function is optimized into a loop:

```neut
define length-of-my-list(a: tau, xs: my-list(a)): int {
  let helper =
    mu get-length(ys: my-list(a), acc: int): int {
      match ys {
      - Nil =>
        acc
      - Cons(_, zs) =>
        get-length(zs, add-int(1, acc)) // a tail call of `get-length`
      }
    }
  in
  helper(xs, 0)
}
```
