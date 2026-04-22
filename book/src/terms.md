# Terms

## Table of Contents

### Basics

- [type](#type)
- [Local Variables](#local-variables)
- [Top-Level Variables](#top-level-variables)
- [let](#let)

### Primitive Values

- [Integers](#integers)
- [Floats](#floats)
- [Runes](#runes)
- [Strings](#strings)

### Functions

- [(x1: a1, ..., xn: an) -> b](#x1-a1--xn-an---b)
- [(x1: a1, ..., xn: an) => { e }](#x1-a1--xn-an---e-)
- [define f(x1: a1, ..., xn: an) -> c { e }](#define-fx1-a1--xn-an---c--e-)
- [inline f(x1: a1, ..., xn: an) -> c { e }](#inline-fx1-a1--xn-an---c--e-)
- [e(e1, ..., en)](#ee1--en)
- [e{x1 := e1, ..., xn := en}](#ex1--e1--xn--en)
- [exact e](#exact-e)

### ADT

- [ADT Formation](#adt-formation)
- [Constructors](#constructors-adt-introduction)
- [match](#match)

### Necessity and Noema

- [+a](#a)
- [&a](#a-1)
- [box](#box)
- [letbox](#letbox)
- [letbox-T](#letbox-t)
- [case](#case)

### Metaprogramming

- ['a](#a-2)
- [quote](#quote)
- [unquote](#unquote)
- [promote](#promote)

### Miscellaneous

- [{e}](#e)
- [lift](#lift)
- [pack-type](#pack-type)
- [unpack-type](#unpack-type)
- [magic](#magic)
- [introspect](#introspect)
- [static](#static)
- [admit](#admit)
- [assert](#assert)
- [\_](#_)

### Syntactic Sugar

- [let x on y1, ..., yn = e1; e2](#on)
- [\*e](#e-1)
- [e::(e1, ..., en)](#ee1--en-1)
- [name[x1, ..., xn]](#namex1--xn)
- [if](#if)
- [when cond { e }](#when-cond--e-)
- [e1; e2](#e1-e2)
- [try x = e1; e2](#try-x--e1-e2)
- [tie x = e1; e2](#tie-x--e1-e2)
- [pin x = e1; e2](#pin-x--e1-e2)
- [?t](#t)

## `type`

`type` is the type of types.

### Example

```neut
define identity<a: type>(x: a) -> a {
  x
}
```

### Syntax

```neut
type
```

### Semantics

`type` is compiled into a pointer to `base.#.imm`.

### Type

```neut
(Γ is a context)
----------------
Γ ⊢ type: type
```

## Local Variables

### Example

```neut
define sample() -> unit {
  // defining/using various local variables
  let x = Unit;
  let foo = x;
  let _h-e-l-l-o = foo;
  let αβγ = _h-e-l-l-o;
  let theSpreadingWideMyNarrowHandsToGatherParadise = αβγ;
  let 冥きより冥き道にぞ入りぬべきはるかに照らせ山の端の月 = Unit;
  let _ = Unit;

  // shadowing (not reassignment)
  let x = Unit;
  let x =
    (x: bool) => {
      x // x: bool
    };
  Unit
}
```

### Syntax

The name of a local variable must satisfy the following conditions:

- It doesn't contain any of ``=()' `\"\n\t:;,<>[]{}/*|&?``
- It doesn't start with `A, B, .., Z` (uppercase letters)

### Semantics

If the content of a variable `x` is an immediate value, `x` is compiled into the name of a register that stores the immediate. Otherwise, `x` is compiled into the name of a register that stores a pointer to the content.

### Type

```neut
Γ ⊢ t: type
---------------- (tvar)
Γ, α: t ⊢ α: t


Γ ⊢ t: type
---------------- (var)
Γ, x: t ⊢ x: t
```

### Notes

- The compiler reports unused variables. You can use the name `_` to suppress those.
- Variables in Neut are immutable. You'll need `core.cell` to achieve mutability.

## Top-Level Variables

### Example

```neut
import {
  core.bool {and},
}

define sample() -> unit {
  // using top-level variables
  let _ = and; // using an imported top-level name
  let _ = core.bool.and; // using the fully qualified name `core.bool.and`
  Unit
}
```

### Syntax

The name of a top-level variable is a (possibly) dot-separated sequence of symbols, where each symbol must satisfy the following conditions:

- It doesn't contain any of ``=()' `\"\n\t:;,<>[]{}/*|&?``

### Semantics

A top-level variable `f` is compiled into the following 3-word tuple:

```
(base.#.imm, 0, POINTER_TO_FUNCTION(f))
```

See the Note below for a more detailed explanation.

### Type

```neut
(Γ is a context)
(c: a is defined at the top-level)
----------------------------------
Γ ⊢ c: a
```

### Note

Let's see how top-level variables are compiled. Consider the following top-level definitions:

```neut
// (source-dir)/sample.nt

// defining a top-level function `increment`
define increment(x: int) -> int {
  add-int(x, 1)
}

define get-increment() -> (int) -> int {
  increment // using a top-level variable `increment`
}
```

`increment` and `get-increment` are compiled into LLVM functions like the following:

```llvm
; (build-dir)/path/to/sample.ll

define fastcc ptr @"this.sample.increment"(ptr %_1) {
  %_2 = ptrtoint ptr %_1 to i64
  %_3 = add i64 %_2, 1
  %_4 = inttoptr i64 %_3 to ptr
  ret ptr %_4
}

define fastcc ptr @"this.sample.get-increment"() {
  ; `increment` in `get-increment` is lowered to the following code:

  ; calculate the size of 3-word tuples
  %_1 = getelementptr ptr, ptr null, i32 3
  %_2 = ptrtoint ptr %_1 to i64
  ; allocate memory
  %_3 = call fastcc ptr @malloc(i64 %_2)
  ; store contents
  %_4 = getelementptr [3 x ptr], ptr %_3, i32 0, i32 0
  %_5 = getelementptr [3 x ptr], ptr %_3, i32 0, i32 1
  %_6 = getelementptr [3 x ptr], ptr %_3, i32 0, i32 2
  store ptr @"base.#.imm", ptr %_4            ; tuple[0] = `base.#.imm`
  store ptr null, ptr %_5                     ; tuple[1] = null
  store ptr @"this.sample.increment", ptr %_6 ; tuple[2] = (function pointer)
  ; return the pointer to the tuple
  ret ptr %_3
}
```

Incidentally, these 3-word tuples are optimized away as long as top-level variables (functions) are called directly with arguments.

## `let`

### Example

```neut
define use-let() -> unit {
  // `let`
  let t = "test";
  print(t)
}

define use-let() -> unit {
  let bar = {
    // nested `let`
    let foo = some-func();
    other-func(foo)
  };
  do-something(bar)
}

define use-let() -> unit {
  // `let` with a type annotation
  let t: &string = "test";
  print(t)
}

```

`let` can be used to destructure an ADT value:

```neut
data item {
| Item(i: int, b: bool)
}

define use-item(x: item) -> int {
  // use `let` with a pattern
  let Item(i, _) = x; // ← here
  i
}

define use-item-2(x: item) -> int {
  // use `let` with a named-field pattern
  let Item{i} = x;
  i
}
```

### Syntax

```neut
let p = e1; e2

let p: t = e1; e2
```

### Semantics

If `p` is a variable `x`, then `let x = e1; e2` binds the result of `e1` to `x` and then evaluates `e2`.

If `p` is a non-variable pattern, `let p = e1; e2` is the following syntactic sugar:

```neut
let p = e1;
e2

↓

match e1 {
| p =>
  e2
}
```

### Type

If `p` is a variable `x`, then:

```neut
Γ ⊢ e1: a
Γ, x: a ⊢ e2: b
---------------------
Γ ⊢ let x = e1; e2: b
```

If `p` is a non-variable pattern, the type is derived from the desugared form.

## Integers

### Example

```neut
define foo() -> unit {
  let _: int = 100;
  let _: int16 = -16;
  let _: int = +1_000_000;
  let _: int = 0b1010_1010;
  let _: int = 0o755;
  let _: int = 0xDEAD_BEEF;
  Unit
}

```

### Syntax

Underscores in integer literals are ignored.

After removing all `_` characters, an integer literal must have one of the following forms:

```text
[+-]?[0-9]+
[+-]?0b[01]+
[+-]?0o[0-7]+
[+-]?0x[0-9A-F]+
```

So, for example, `3`, `-16`, `+1_000_000`, `0b1010_1010`, `0o755`, and `0xDEAD_BEEF` are valid integer literals.

### Semantics

The same as LLVM integers.

### Type

The type of an integer is unknown in itself. It must be inferred to be one of the following types:

- `int1`
- `int2`
- `int4`
- `int8`
- `int16`
- `int32`
- `int64`

It can also be inferred as an ADT with no type parameters and exactly one constructor with one field, as long as tracing back from that type eventually reaches an integer type. For example, given

```neut
data inner {
| Inner(int)
}

data wrapper {
| Wrapper(inner)
}
```

Then `42: wrapper` holds.

### Note

- The type `int` is also available. For more, see [Primitives](./primitives.md#primitive-types).

## Floats

### Example

```neut
define foo() -> unit {
  let _: float = 3.8;
  let _: float32 = -0.2329;
  let _: float = +1_234.5e-2;
  let _: float = 6.0e23;
  let _: float = 0x1.Ap2;
  let _: float = 0x1.8;
  Unit
}

```

### Syntax

Underscores in float literals are ignored.

After removing all `_` characters, a decimal floating-point literal must match:

```text
[+-]?[0-9]+\.[0-9]+(e[+-]?[0-9]+)?
```

After removing all `_` characters, a hexadecimal floating-point literal must match:

```text
[+-]?0x[0-9A-F]+\.[0-9A-F]+(p[+-]?[0-9]+)?
```

So, for example, `3.8`, `-0.2329`, `+1_234.5e-2`, `6.0e23`, `0x1.Ap2`, and `0x1.8` are valid float literals.

In hexadecimal floating-point literals, the `p` exponent is base-2.

### Semantics

The same as LLVM floats.

### Type

The type of a float is unknown in itself. It must be inferred to be one of the following types:

- `float16`
- `float32`
- `float64`

It can also be inferred as an ADT with no type parameters and exactly one constructor with one field, as long as tracing back from that type eventually reaches a float type. For example, given

```neut
data inner {
| Inner(float)
}

data wrapper {
| Wrapper(inner)
}
```

Then `3.14: wrapper` holds.

### Note

- The type `float` is also available. For more, see [Primitives](./primitives.md#primitive-types).

## Runes

### Example

```neut
define foo() -> unit {
  let _: rune = `A`;
  //            ^^^
  let _: rune = `\n`;
  //            ^^^
  let _: rune = `\u{123}`;
  //            ^^^^^^^^^
  Unit
}

```

### Syntax

`` `A` ``, `` `\n` ``, `` `\u{123}` ``, etc.

The available escape sequences in rune literals are the same as those of the literal form of [`static`](./terms.md#static).

### Semantics

The value of a rune literal is represented as its UTF-8 byte sequence packed into an `int32`.

### Type

```neut
(Γ is a context)
(c is a rune literal)
---------------------
Γ ⊢ c: rune
```

### Note

(1) You can write `` `\u{1234}` ``, for example, to represent U+1234 (`` `ሴ` ``).

(2) We have the following equalities, for example:

```neut
`A` == magic cast(int32, rune, 0x41)
`Γ` == magic cast(int32, rune, 0xCE93)
`あ` == magic cast(int32, rune, 0xE38182)
`⭐` == magic cast(int32, rune, 0xE2AD90)
```

You can see this by calling the following function:

```neut
define print-star() -> unit {
  // prints "⭐"
  pin t = core.string.singleton(magic cast(int32, rune, 0xe2ad90));
  print-line(t)
}
```

## Strings

### Example

```neut
define foo() -> unit {
  let _: &string = "test";
  //               ^^^^^^
  Unit
}

```

### Syntax

The syntax of a string literal is the same as that of the literal form of [`static`](#static).

### Semantics

If `t` is a string literal, then `t` is shorthand for `magic cast(text, &string, static t)`.

### Type

```neut
(Γ is a context)
(t is a string literal)
-----------------------
Γ ⊢ t: &string
```

### Note

- For the exact syntax and internal representation of the literal part, see [`static`](#static).

## `(x1: a1, ..., xn: an) -> b`

`(x1: a1, ..., xn: an) -> b` is the type of ordinary functions. Replacing `->` with `->>` yields the type of destination-passing functions. A function type can also have a bracketed default-argument part after the ordinary parameters, such as `(value: int)[step: int] -> int`.

### Example

```neut
// a function that accepts ints and returns bools
(value: int) -> bool

// a destination-passing function that returns `either(int, bool)`
(value: int) ->> either(int, bool)

// this is equivalent to `(_: int) -> bool`:
(int) -> bool

// using a type variable
<a: type>(x: a) -> a

// this is equivalent to `<a: _>(x: a) -> a`
<a>(x: a) -> a

// a function with a default argument named `step`
(value: int)[step: int] -> int
```

### Syntax

```neut
<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c

<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm)[z1: c1, ..., zk: ck] -> c

<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) ->> c

<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm)[z1: c1, ..., zk: ck] ->> c
```

The following abbreviations are available:

```neut
(y1: b1, ..., ym: bm) -> c

// ↓
// <>(y1: b1, ..., ym: bm) -> c


(b1, ..., bm) -> c

// ↓
// (_: b1, ..., _: bm) -> c


<a1, ..., an>(y1: b1, ..., ym: bm) -> c

// ↓
// <a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c
```

The same abbreviations are available when `->` is replaced with `->>`.

The bracketed part may be omitted, and `[]` is also accepted. This default-argument part is included in the function type, so both the keys and their order must match during type checking.

### Semantics

A function type is compiled into a pointer to `base.#.cls`. For more, please see [On Executing Types](./on-executing-types.md).

### Type

```neut
Γ, α1: s1, ..., αn: sn, x1: t1, ..., xm: tm, z1: u1, ..., zk: uk ⊢ v: type
--------------------------------------------------------------------------------
Γ ⊢ <α1: s1, ..., αn: sn>(x1: t1, ..., xm: tm)[z1: u1, ..., zk: uk] -> v: type
```

Omitting the bracketed part means `k = 0`. The same rule applies to `->>`.

## `(x1: a1, ..., xn: an) => { e }`

`=>` can be used to create an anonymous function. Replacing `=>` with `=>>` yields a destination-passing anonymous function. You can also insert a default-argument list between the ordinary parameter list and the arrow.

### Example

```neut
define use-function() -> int {
  let f =
    (x: int, y: int) => {
      let z = add-int(x, y);
      mul-int(z, z)
    };
  let id =
    <a>(x: a) => {
      x
    };
  let step =
    (x: int) =>> {
      Pair(x, add-int(x, 1))
    };
  let result = f(10, 20);
  let _ = id(42);
  let Pair(a, b) = step(10);
  add-int(result, add-int(a, b))
}
```

### Syntax

```neut
(x1: a1, ..., xn: an)[y1: b1 := d1, ..., ym: bm := dm] => {
  e
}

(x1: a1, ..., xn: an) => {
  e
}

(x1: a1, ..., xn: an)[y1: b1 := d1, ..., ym: bm := dm] =>> {
  e
}

(x1: a1, ..., xn: an) =>> {
  e
}

// You can omit type annotations
(x1, ..., xn) => {
  e
}
```

The following abbreviation is available:

```neut
(x1, ..., xn) => { e }

// ↓
// (x1: _, ..., xn: _) => { e }
```

The same abbreviation is available when `=>` is replaced with `=>>`.

Type annotations inside the default-argument list can also be omitted when they can be inferred. For example,

```neut
(x: int)[step := 1] => {
  add-int(x, step)
}
```

has type `(x: int)[step: int] -> int`.

If an anonymous function is defined at layer `n`, then any free variable `x` in the function must satisfy `layer(x) <= n`. For example, the following is not a valid term:

```neut
define return-int(x: +int) -> +() -> int {
  // here is layer 0
  box {
    // here is layer -1
    () => {
      letbox result =
        // here is layer 0
        x; // ← error
      result
    }
  }
}
```

because the free variable `x` in the anonymous function is at layer 0, whereas the anonymous function is at layer -1, so the condition `layer(x) <= n` is not satisfied.

For more on layers, please see the section on [box](#box), [letbox](#letbox), and [letbox-T](#letbox-t).

### Semantics

Anonymous functions are compiled into three-word closures. For more, please see [On Executing Types](./on-executing-types.md#advanced-function-types).

When `=>>` is used, the closure uses destination-passing style when applied. The same destination-passing scheme is used for `->>` as well. The source-level calling syntax remains the usual one, but the caller passes the result destination to the callee.

This is useful when combined with malloc-free canceling. For example, if a function returns an ADT value using the ordinary arrow `->`, then the function itself has to allocate that result. With `->>`, the allocation choice moves to the caller, so temporary heap allocations can often be removed.

For example, consider the following definitions:

```neut
define foo(x: int) ->> either(int, bool) {
  if eq-int(x, 0) {
    Left(42)
  } else {
    Right(True)
  }
}

define use-foo() -> unit {
  match foo(10) {
  | Left(x) =>
    cont1
  | Right(y) =>
    cont2
  }
}
```

These behave roughly as follows after compilation:

```neut
// pseudocode
define foo(dest: pointer, x: int) -> void {
  if eq-int(x, 0) {
    let tmp = malloc(..);
    // initialize `tmp := Left(42)`
    copy(dest, tmp);
    free(tmp)
  } else {
    let tmp = malloc(..);
    // initialize `tmp := Right(True)`
    copy(dest, tmp);
    free(tmp)
  }
}

define use-foo() -> unit {
  let buf = malloc(..);
  foo(buf, 10);
  match tag(buf) {
  | 0 =>
    let x = extract-from-left(buf);
    free(buf);
    cont1
  | _ =>
    let y = extract-from-right(buf);
    free(buf);
    cont2
  }
}
```

After malloc-free canceling, this can be simplified further:

```neut
// pseudocode
define foo(dest: pointer, x: int) -> void {
  if eq-int(x, 0) {
    let tmp = alloca(..);
    // initialize `tmp := Left(42)`
    copy(dest, tmp)
  } else {
    let tmp = alloca(..);
    // initialize `tmp := Right(True)`
    copy(dest, tmp)
  }
}

define use-foo() -> unit {
  let buf = alloca(..);
  foo(buf, 10);
  match tag(buf) {
  | 0 =>
    let x = extract-from-left(buf);
    cont1
  | _ =>
    let y = extract-from-right(buf);
    cont2
  }
}
```

The size of the destination is determined by the value returned by `magic call-type(t, 2, ...)`. When the size is non-negative, the caller prepares a destination of that size. Otherwise, the caller uses a one-word temporary slot and passes that to the callee instead.

### Type

```neut
Γ, α1: s1, ..., αn: sn, x1: t1, ..., xm: tm ⊢ e: u
------------------------------------------------------------------------------------------------------
Γ ⊢ <α1: s1, ..., αn: sn>(x1: t1, ..., xm: tm) => {e}: <α1: s1, ..., αn: sn>(x1: t1, ..., xm: tm) -> u

```

Replacing `=>` with `=>>` changes the resulting type from `-> u` to `->> u`.

When default arguments are present, the resulting type additionally contains the bracketed default-argument part, and those binders are also available in the body.

### Note

- Anonymous functions are reduced at compile time when possible. If you would like to avoid this behavior, consider using `define`.

## `define f(x1: a1, ..., xn: an) -> c { e }`

`define` (at the term-level) can be used to create a function with possible recursion. Replacing `->` with `->>` yields a destination-passing function. As with anonymous functions, you can insert a default-argument list between the ordinary parameter list and the arrow.

### Example

```neut
define use-define() -> int {
  let c = 10;
  let f =
    // term-level `define` with a free variable `c`
    define some-recursive-func(x: int) -> int {
      if eq-int(x, 0) {
        0
      } else {
        add-int(c, some-recursive-func(sub-int(x, 1)))
      }
    };
  f(100)
}
```

### Syntax

```neut
define name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c {
  e
}

define name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm)[z1: c1 := e1, ..., zk: ck := ek] -> c {
  e
}

define name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) ->> c {
  e
}

define name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm)[z1: c1 := e1, ..., zk: ck := ek] ->> c {
  e
}
```

The following abbreviations are available:

```neut
define name(y1: b1, ..., ym: bm) -> c {e}

// ↓
// define name<>(y1: b1, ..., ym: bm) -> c {e}


define name<a1, ..., an>(y1: b1, ..., ym: bm) -> c {e}

// ↓
// define name<a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c {e}
```

The same abbreviations are available when `->` is replaced with `->>`.

Type annotations inside the default-argument list can be omitted when they can be inferred, as in `define add(x: int)[step := 1] -> int { add-int(x, step) }`.

If a term-level `define` is at layer `n`, then any free variable `x` in it must satisfy `layer(x) <= n`.

### Semantics

A term-level `define` is lifted to a top-level definition using lambda lifting. For example, consider the following example:

```neut
define use-define() -> int {
  let c = 10;
  let f =
    // term-level `define` with a free variable `c`
    define some-recursive-func(x: int) -> int {
      if eq-int(x, 0) {
        0
      } else {
        add-int(c, some-recursive-func(sub-int(x, 1)))
      }
    };
  f(100)
}
```

The code above is compiled into something like the following:

```neut
// the free variable `c` is now a parameter
define some-recursive-func(c: int, x: int) -> int {
  if eq-int(x, 0) {
    0
  } else {
    let f =
      (x: int) => {
        some-recursive-func(c, x)
      };
    add-int(c, f(sub-int(x, 1)))
  }
}

define use-define() -> int {
  let c = 10;
  let f =
    (x: int) => {
      some-recursive-func(c, x)
    };
  f(100)
}
```

When `->>` is used, the lifted function and the resulting closure use destination-passing style. The function is still called as usual. For the details of this behavior, please see the section on [anonymous functions](#x1-a1--xn-an---e-).

### Type

```neut
Γ, x1: a1, ..., xn: an, f: (x1: a1, ..., xn: an) -> t ⊢ e: t
----------------------------------------------------------------------
Γ ⊢ define f(x1: a1, ..., xn: an) -> t {e}: (x1: a1, ..., xn: an) -> t
```

Replacing `->` with `->>` changes the resulting type from:

```neut
(x1: a1, ..., xn: an) -> t
```

to:

```neut
(x1: a1, ..., xn: an) ->> t
```

When default arguments are present, the resulting type additionally contains the bracketed default-argument part.

### Note

- Functions defined by term-level `define` aren't inlined at compile time, even if they contain no recursion.

## `inline f(x1: a1, ..., xn: an) -> c { e }`

`inline` (at the term-level) can be used to create an inline function. Replacing `->` with `->>` yields a destination-passing inline function. The same default-argument syntax as `define` is available here as well.

### Example

```neut
define use-inline() -> int {
  let f =
    inline add-and-double(x: int, y: int) -> int {
      let z = add-int(x, y);
      mul-int(z, z)
    };
  let result = f(10, 20);
  result
}
```

### Syntax

```neut
inline name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c {
  e
}

inline name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm)[z1: c1 := e1, ..., zk: ck := ek] -> c {
  e
}

inline name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) ->> c {
  e
}

inline name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm)[z1: c1 := e1, ..., zk: ck := ek] ->> c {
  e
}
```

The following abbreviations are available:

```neut
inline name(y1: b1, ..., ym: bm) -> c {e}

// ↓
// inline name<>(y1: b1, ..., ym: bm) -> c {e}


inline name<a1, ..., an>(y1: b1, ..., ym: bm) -> c {e}

// ↓
// inline name<a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c {e}
```

The same abbreviations are available when `->` is replaced with `->>`.

You can also insert a default-argument list between the ordinary parameter list and the arrow, as in:

```neut
inline add(x: int)[step: int := 1] -> int {
  add-int(x, step)
}
```

If a term-level `inline` is at layer `n`, then any free variable `x` in it must satisfy `layer(x) <= n`.

### Semantics

A term-level `inline` is the same as a term-level `define`, except that the resulting function is always expanded at compile time. When `->>` is used, the inlined function uses destination-passing style. For the details of this behavior, please see the section on [anonymous functions](#x1-a1--xn-an---e-).

### Type

```neut
Γ, x1: a1, ..., xn: an, f: (x1: a1, ..., xn: an) -> t ⊢ e: t
----------------------------------------------------------------------
Γ ⊢ inline f(x1: a1, ..., xn: an) -> t {e}: (x1: a1, ..., xn: an) -> t
```

Replacing `->` with `->>` changes the resulting type from:

```neut
(x1: a1, ..., xn: an) -> t
```

to:

```neut
(x1: a1, ..., xn: an) ->> t
```


When default arguments are present, the resulting type additionally contains the bracketed default-argument part.

### Note

- Functions defined by term-level `inline` are always inlined at compile time. If you would like to avoid this behavior, consider using `define`.

## `e(e1, ..., en)`

Given a function `e` and arguments `e1, ..., en`, we can write `e(e1, ..., en)` to denote a function application. If `e` has default arguments, the application may be followed by a bracketed list of overrides.

### Example

```neut
define use-function() -> unit {
  let _ = foo();
  //      ^^^^^
  let _ = bar(1);
  //      ^^^^^^
  let _ = baz("hello", True);
  //      ^^^^^^^^^^^^^^^^^^
  Unit
}
```

### Syntax

```neut
e(e1, ..., en)

e<t1, ..., tm>(e1, ..., en)

e(e1, ..., en)[x1 := d1, ..., xk := dk]

e<t1, ..., tm>(e1, ..., en)[x1 := d1, ..., xk := dk]
```

If `e` has implicit parameters, you can specify them explicitly using the latter form. For example, if `e: <a>(a) -> unit`, then `e<int>(v)` specifies the implicit parameter `a` as `int`.

If `e` has default arguments, you can override some or all of them by writing

```neut
[x1 := d1, ..., xk := dk]
```

after the ordinary argument list. These overrides are matched by key.

### Semantics

Given a function application `e(e1, ..., en)`, if `e` has an ordinary function type, it is evaluated as follows:

1. Computes `e`, `e1`, ..., `en` into values `v`, `v1`, ..., `vn`
2. Extracts the contents from the closure `v`, obtaining the tuple of its free variables and a function label
3. Deallocates the tuple of the closure `v`
4. Calls the function label with the tuple and `v1`, ..., `vn` as arguments

If `e` has a noetic function type such as `&(a1, ..., an) -> b`, it is evaluated as follows:

1. Computes `e`, `e1`, ..., `en` into values `v`, `v1`, ..., `vn`
2. Reads the contents of the closure `v`, obtaining the tuple of its free variables and a function label
3. Copies all the free variables captured by `e`
4. Executes the function body using those copies and `v1`, ..., `vn`

If `e` has a destination-passing function type, it is evaluated as follows:

1. Prepares the result destination
2. Computes `e`, `e1`, ..., `en` into values
3. Performs the corresponding call above, passing the destination as an extra argument
4. Reads the result from that destination

When the size of the result type is non-negative, the destination has that size. Otherwise, the caller uses a one-word temporary slot that stores a pointer to the result.

When a default argument is omitted, its default expression is evaluated at the time of the call. In particular, each call that omits the argument computes a fresh value rather than reusing one from the function definition.

### Type

```neut
Γ ⊢ e: <α1: a1, .., αn: an>(y1: b1, .., ym: bm) -> c    Γ ⊢ e1: b1  ..   Γ ⊢ em: bm
---------------------------------------------------------------------------------------
Γ ⊢ e(e1, .., em): c[α1 := ?M1, .., αn := ?Mn, y1 := e1, .., ym := em]
```

The `?Mi`s in the above rule are metavariables that must be inferred by the compiler.

The same rule also applies when `e` has type:

- `<α1: a1, .., αn: an>(y1: b1, .., ym: bm) ->> c`
- `&<α1: a1, .., αn: an>(y1: b1, .., ym: bm) -> c`
- `&<α1: a1, .., αn: an>(y1: b1, .., ym: bm) ->> c`

When `e` has a default-argument part such as `[z1: c1, .., zk: ck]`, the application may additionally provide `[zi := di]`, and omitted keys use the defaults declared by the function.

## `e{x1 := e1, ..., xn := en}`

`e{x1 := e1, ..., xn := en}` is an alternative notation for applying a function.

### Example

```neut
define foo(x: int, y: bool, some-path: &string) -> unit {
  // ...
}

define use-foo() -> unit {
  foo{
    x := 10,
    y := True,
    some-path := "/path/to/file",
  }
}
```

### Syntax

```neut
e{x1 := e1, ..., xn := en}
```

### Semantics

The same as `e(e1, ..., en)`.

### Type

The same as `e(e1, ..., en)`.

### Note

This notation might be useful when used in combination with ADTs:

```neut
data config {
| Config(
    count: int,
    path: &string,
    colorize: bool,
  )
}

constant some-config: config {
  Config{
    count := 10,
    colorize := True,
    path := "/path/to/file", // you can reorder arguments
  }
}
```

If the argument is a variable that has the same name as the parameter, you can use a shorthand notation:

```neut
define use-foo() -> unit {
  let x = 10;
  let y = True;
  let some-path = "/path/to/file";
  foo{x, y, some-path} // == foo{x := x, y := y, some-path := some-path}
}
```

## `exact e`

Given a function `e`, `exact e` supplies all the implicit parameters of `e` by inserting holes.

### Example

```neut
define id<a>(x: a) -> a {
  x
}

define use-id() -> unit {
  let g: (x: int) -> int = exact id;
  Unit
}
```

Note that the following won't type-check:

```neut
define id<a>(x: a) -> a {
  x
}

define use-id() -> unit {
  let g: (x: int) -> int = id;
  Unit
}
```

This is because the type of `id` is `<a>(x: a) -> a`, not `(x: ?M) -> ?M`.

### Syntax

```neut
exact e
```

### Semantics

Given a term `e` of type `<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c`,

```neut
exact e
```

is translated into the following:

```neut
(y1: b1, ..., ym: bm) => {
  e<_, ..., _>(y1, ..., ym)
}
```

### Type

```neut
Γ ⊢ e: <α1: a1, ..., αn: an>(y1: b1, ..., ym: bm) -> c
--------------------------------------------------------------------
Γ ⊢ exact e: {(y1: b1, ..., ym: bm) -> c}[α1 := ?M1, ..., αn := ?Mn]
```

Here, `?Mi`s are metavariables that must be inferred by the type checker.

### Note

As you can see from its semantics, an `exact` is just a shorthand for a "hole-application" that fills in implicit parameters.

## ADT Formation

After defining an ADT using the statement `data`, you can use the ADT.

### Example

```neut
data my-nat {
| Zero
| Succ(my-nat)
}

define use-nat-type() -> my-nat {
  Zero
}
```

### Syntax

The same as that of top-level variables.

### Semantics

The same as that of top-level variables.

### Type

If an ADT `some-adt` is nullary, the type of `some-adt` is `type`.

Otherwise, suppose that an ADT `some-adt` is defined as follows:

```neut
data some-adt(x1: a1, ..., xn: an) {..}
```

In this case, the type of `some-adt` is `(x1: a1, ..., xn: an) -> type`.

## Constructors (ADT Introduction)

After defining an ADT using the statement `data`, you can use the constructors to construct values of the ADT.

### Example

```neut
data my-nat {
| Zero
| Succ(my-nat)
}

define create-nat() -> my-nat {
  // `Succ` and `Zero` are constructors
  Succ(Succ(Zero))
}
```

### Syntax

The same as that of top-level variables, except that constructors must be capitalized.

### Semantics

The same as that of top-level variables.

### Type

If a constructor `c` is nullary, the type of `c` is the ADT type. For example, consider the following code:

```neut
data some-adt {
| C1
}

data other-adt(a: type) {
| C2
}
```

In this case,

- the type of `C1` is `some-adt`, and
- the type of `C2` is `other-adt(?M)`, where the `?M` must be inferred by the compiler.

If a constructor `c` isn't nullary, the type of `c` is the function type that takes specified arguments and turns them into the ADT type. For example, consider the following code:

```neut
data some-adt {
| C1(foo: int)
}

data other-adt(a: type) {
| C2(bar: bool, baz: other-adt(a))
}
```

In this case,

- the type of `C1` is `(foo: int) -> some-adt`, and
- the type of `C2` is `(bar: bool, baz: other-adt(?M)) -> other-adt(?M)`.

## `match`

You can use `match` to destructure ADT values or integers.

### Example

```neut
data my-nat {
| Zero
| Succ(my-nat)
}

define foo(n: my-nat) -> int {
  match n {
  | Zero =>
    100
  | Succ(m) =>
    foo(m)
  }
}

define bar(n: my-nat) -> int {
  // You can use nested patterns
  match n {
  | Zero =>
    100
  | Succ(Succ(m)) => // ← a nested pattern
    200
  | Succ(m) =>
    foo(m)
  }
}

define eq-nat(n1: my-nat, n2: my-nat) -> bool {
  // `match` can handle multiple values
  match n1, n2 {
  | Zero, Zero =>
    True
  | Succ(m1), Succ(m2) =>
    eq-nat(m1, m2)
  | _, _ =>
    False
  }
}

define literal-match(x: int) -> int {
  // You can use `match` against integers
  match x {
  | 3 =>
    30
  | 5 =>
    50
  | _ =>
    add-int(x, 10)
  }
}
```

### Syntax

```neut
match e1, ..., en {
| pattern-1 =>
  body-1
  ...
| pattern-m =>
  body-m
}
```

### Semantics

The semantics of `match` is the same as the semantics of ordinary pattern matching, except that ADT values are _consumed_ after branching.

For example, let's see how `my-nat` in the following code is used in `match`:

```neut
data my-nat {
| Zero
| Succ(my-nat)
}
```

The internal representation of `n: my-nat` is something like the following:

```neut
Zero:
  (0) // 1-word tuple
Succ:
  (1, pointer-to-m) // 2-word tuple
```

When evaluating `match`, the runtime inspects the first element of the "tuple" `n`.

```neut
define foo(n: my-nat) -> int {
  // inspects the first element of `n` here
  match n {
  | Zero =>
    100
  | Succ(m) =>
    foo(m)
  }
}
```

If the first element is `0`, which means that we found an ADT value of `Zero`, the runtime _frees_ the outer tuple of `(0)`, and then evaluates `100`.

If the first element is `1`, which means that we found an ADT value of `Succ`, the runtime gets the pointer to the second element of `n`, binds it to `m`, _frees_ the outer tuple of `(1, pointer-to-m)`, and then evaluates `foo(m)`.

### Type

```neut
Γ ⊢ e1: a1
...
Γ ⊢ en: an

Γ ⊢ patterns_1 ⇐ (a1, ..., an) ⇒ Δ_1
Γ, Δ_1 ⊢ body-1: b

...

Γ ⊢ patterns_m ⇐ (a1, ..., an) ⇒ Δ_m
Γ, Δ_m ⊢ body-m: b

Exhaustive((a1, ..., an), [patterns_1, ..., patterns_m])
------------------------------------------------------------------------------
Γ ⊢ match e1, ..., en {
    | patterns_1 => body-1
    ...
    | patterns_m => body-m
    }: b
```

where:

- `Γ ⊢ patterns ⇐ types ⇒ Δ` means that the pattern sequence `patterns` matches values of types `types`, introducing the variable context `Δ`.
- `Exhaustive(types, clauses)` means that the list of pattern sequences `clauses` is exhaustive for the types `types`.
- `patterns_i := (pat_{i,1}, ..., pat_{i,n})`

### Note

An example of the application of the typing rule of `match`:

```neut
Γ ⊢ n: my-nat

Γ ⊢ Zero ⇐ my-nat ⇒ ∅ // = Δ_1
Γ ⊢ 100: int          // body-1

Γ ⊢ Succ(m) ⇐ my-nat ⇒ (m: my-nat) // = Δ_2
Γ, m: my-nat ⊢ foo(m): int         // body-2

Exhaustive((my-nat), [Zero, Succ(m)])
------------------------------------------------------------------------------
Γ ⊢ match n {
    | Zero => 100
    | Succ(m) => foo(m)
    }: int
```

## `+a`

Given a type `a: type`, `+a` is the type of `a` in the "outer" layer.

### Example

```neut
define axiom-T<a>(x: +a) -> a {
  letbox-T result = x;
  result
}
```

### Syntax

```neut
+a
```

### Semantics

Operationally, `+a` has the same runtime representation as `a`.

### Type

```neut
Γ ⊢ t: type
----------------
Γ ⊢ +t: type
```

### Note

`+` is the T-necessity operator in that we can construct terms of the following types:

- `((a) -> b, +a) -> +b` (Axiom K)
- `(+a) -> a` (Axiom T)

Note that `+(a) -> b` and `(+a) -> b` are different types.

## `&a`

Given a type `a: type`, `&a` is the type of noemata over `a`.

### Example

```neut
data my-nat {
| Zero
| Succ(my-nat)
}

define foo-noetic(n: &my-nat) -> int {
  case n {
  | Zero =>
    100
  | Succ(m) =>
    foo-noetic(m)
  }
}
```

### Syntax

```neut
&t
```

### Semantics

For every type `a`, `&a` is compiled into `base.#.imm`.

### Type

```neut
Γ ⊢ t: type
-----------
Γ ⊢ &t: type
```

### Note

- Values of type `&a` can be created using `on`.
- Values of type `&a` are expected to be used in combination with `case` or `*e`.
- Since `&a` is compiled into `base.#.imm`, values of type `&a` aren't discarded or copied even when used non-linearly.
- See the Note of [box](#box) to see the relation between `&a` and `+a`

## `box`

`box {e}` can be used to "lift" the layer of `e`.

### Example

```neut
define use-noetic<a>(x: &a, y: &a) -> +a {
  // layer 0
  // - x: &a at layer 0
  // - y: &a at layer 0
  box x {
    // layer -1
    // x:  a at layer -1
    // y: &a at layer 0 (cannot be used here; causes a layer error)
    x
  }
}
```

### Syntax

```neut
box x1, ..., xn { e } // n >= 0
```

We say that this `box` captures the variables `x1, ..., xn`.

### Semantics

Given noetic variables `x1: &a1, ..., xn: &an`, the term `box x1, ..., xn { e }` copies all the `xi`s and executes `e`:

```neut
box x1, ..., xn { e }

↓

let x1 = copy-noema(x1);
...
let xn = copy-noema(xn);
e
```

### Type

```neut
Γ, Δ ⊢ⁱ e1: a
------------------------- (□-intro)
Γ, &Δ ⊢ⁱ⁺¹ box Δ {e1}: +a
```

where:

- every variable in `Δ` is at layer `i`.
- every variable in `&Δ` is at layer `i+1`.

### Notes on Layers

The body of `define` is at layer 0:

```neut
define some-function(x: int) -> int {
  // here is layer 0
  // `x: int` is a variable at layer 0
  add-int(x, 1)
}
```

Since `box {e}` lifts the layer of `e`, if we use `box` at layer 0, the layer of `e` will become -1:

```neut
define use-box(x: int) -> +int {
  // here is layer 0
  box {
    // here is layer -1
    10
  }
}
```

In layer n, we can only use variables at the same layer. Thus, the following is not a valid term:

```neut
define use-box-error(x: int) -> +int {
  // here is layer 0
  box {
    // here is layer -1
    add-int(x, 1) // error: use of a variable at layer 0 (≠ -1)
  }
}
```

We can incorporate variables outside `box` by capturing them:

```neut
define use-box-with-noema(x: &int) -> +int {
  // here is layer 0
  // x: &int at layer 0
  box x {
    // here is layer -1
    // x: int at layer -1
    add-int(x, 1) // ok
  }
}
```

## `letbox`

You can use `letbox` to "unlift" terms.

### Example

```neut
define roundtrip<a>(x: +a) -> +a {
  // here is layer 0
  box {
    // here is layer -1
    letbox tmp =
      // here is layer 0
      x;
    tmp
  }
}

define try-borrowing(x: int) -> unit {
  // here is layer 0
  // x: int (at layer 0)
  letbox tmp =
    // here is layer 1
    // (thus `x` is not available here)
    some-func(x);
  // here is layer 0
  // x: int (at layer 0)
  Unit
}
```

### Syntax

```neut
letbox result = e1;
e2
```

### Semantics

```neut
letbox result = e1;
e2

↓

let result = e1;
e2
```

### Type

```neut
Γ ⊢ⁱ⁺¹ e1: +a
Γ, x: a ⊢ⁱ e2: b
------------------------- (□-elim-K)
Γ ⊢ⁱ letbox x = e1; e2: b
```

where `x` is a variable at layer `i`.

### Note

Given a term `e1` at layer n + 1, `letbox x = e1; e2` is at layer n:

```neut
define roundtrip<a>(x: +a) -> +a {
  box {
    // here is layer -1 (= n)
    letbox tmp =
      // here is layer 0 (= n + 1)
      x;
    // here is layer -1 (= n)
    tmp
  }
}
```

In layer n, we can only use variables at the same layer. Thus, the following is not a valid term:

```neut
define use-letbox-error(x: +int) -> int {
  // here is layer 0
  // x: +int (at layer 0)
  letbox tmp =
    // here is layer 1
    x; // error: use of a variable at layer 0 (≠ 1)
  // here is layer 0
  tmp
}
```

## `letbox-T`

You can use `letbox-T` to get values from terms of type `+a` without changing layers.

### Example

```neut
define extract-value-from-meta(x: +int) -> int {
  // here is layer 0
  // x: +int (at layer 0)
  letbox-T tmp =
    // here is layer 0
    x; // ok
  // here is layer 0
  tmp
}
```

### Syntax

```neut
letbox-T result = e1;
e2

letbox-T result on x1, ..., xn = e1;
e2
```

### Semantics

```neut
letbox-T result on x1, ..., xn = e1;
e2

↓

let x1 = cast<a1, &a1>(x1);
...
let xn = cast<an, &an>(xn);
let result = e1;
let x1 = cast<&a1, a1>(x1);
...
let xn = cast<&an, an>(xn);
e2
```

### Type

```neut
Γ, &Δ ⊢ⁱ e1: +a
Γ, Δ, x: a ⊢ⁱ e2: b
----------------------------------- (□-elim-T)
Γ, Δ ⊢ⁱ letbox-T x on Δ = e1; e2: b
```

where every variable in `&Δ` and `Δ` is at layer `i`.

### Note

`letbox-T` doesn't alter layers:

```neut
define extract-value-from-meta(x: +int) -> int {
  // here is layer 0
  letbox-T tmp =
    // here is layer 0
    x;
  // here is layer 0
  tmp
}
```

`on` doesn't alter variable layers either:

```neut
define extract-value-from-meta(x: int) -> +int {
  // here is layer 0
  // x: int (at layer 0)
  letbox-T tmp on x =
    // here is layer 0
    // x: &int (at layer 0)
    box x {x};
  // here is layer 0
  // x: int (at layer 0)
  tmp
}
```

## `case`

You can use `case` to inspect noetic ADT values or integers.

### Example

```neut
data my-nat {
| Zero
| Succ(my-nat)
}

define foo-noetic(n: &my-nat) -> int {
  case n {
  | Zero =>
    100
  | Succ(m) =>
    foo-noetic(m)
  }
}
```

### Syntax

```neut
case e1, ..., en {
| pattern-1 =>
  body-1
  ...
| pattern-m =>
  body-m
}
```

### Semantics

The semantics of `case` is the same as `match`, except that `case` doesn't consume ADT values.

### Type

```neut
Γ ⊢ e1: a1
...
Γ ⊢ en: an

Γ ⊢ patterns_1 ⇐ (a1, ..., an) ⇒ Δ_1
Γ, &Δ_1 ⊢ body-1: b

...

Γ ⊢ patterns_m ⇐ (a1, ..., an) ⇒ Δ_m
Γ, &Δ_m ⊢ body-m: b

Exhaustive((a1, ..., an), [patterns_1, ..., patterns_m])
------------------------------------------------------------------------------
Γ ⊢ case e1, ..., en {
    | patterns_1 => body-1
    ...
    | patterns_m => body-m
    }: b
```

where:

- `Γ ⊢ patterns ⇐ types ⇒ Δ` means that the pattern sequence `patterns` matches values of types `types`, introducing the variable context `Δ`.
- `Exhaustive(types, clauses)` means that the list of pattern sequences `clauses` is exhaustive for the types `types`.
- `patterns_i := (pat_{i,1}, ..., pat_{i,n})`
- if `Δ` is `x1: t1, ..., xk: tk`, then `&Δ` means `x1: &t1, ..., xk: &tk`.

### Note

An example of the application of the typing rule of `case`:

```neut
Γ ⊢ n: &my-nat

Γ ⊢ Zero ⇐ my-nat ⇒ ∅ // = Δ_1
Γ ⊢ 100: int          // body-1

Γ ⊢ Succ(m) ⇐ my-nat ⇒ (m: my-nat) // = Δ_2
Γ, m: &my-nat ⊢ foo-noetic(m): int // body-2

Exhaustive((my-nat), [Zero, Succ(m)])
------------------------------------------------------------------------------
Γ ⊢ case n {
    | Zero => 100
    | Succ(m) => foo-noetic(m)
    }: int
```

## `'a`

Given a type `a: type`, `'a` is the type of code that evaluates to a term of type `a`.

### Example

```neut
define duplicate-code(x: 'int) -> 'pair(int, int) {
  quote {
    let y = unquote {x};
    Pair(y, y)
  }
}
```

### Syntax

```neut
'a
```

### Semantics

For every type `a`, `'a` is compiled into the same term as `a`.

### Type

```neut
Γ ⊢ t: type
------------
Γ ⊢ 't: type
```

### Note

- Values of type `'a` are expected to be used in combination with `quote`, `unquote`, or `promote`.

## `quote`

You can use `quote` to create code.

### Example

```neut
define duplicate-code(x: 'int) -> 'pair(int, int) {
  quote {
    let y = unquote {x};
    Pair(y, y)
  }
}
```

### Syntax

```neut
quote {
  e
}
```

### Semantics

`quote` is compiled into the same term as its body.

### Type

```neut
Γ ⊢ⁱ e: a
--------------------
Γ ⊢ⁱ⁺¹ quote {e}: 'a
```

### Note

The body of `quote` is at one stage lower than the outer context:

```neut
define make-code() -> 'int {
  // here is stage 0
  quote {
    // here is stage -1
    10
  }
}
```

## `unquote`

You can use `unquote` to use code.

### Example

```neut
define use-code() -> int {
  unquote {
    quote {10}
  }
}
```

### Syntax

```neut
unquote {
  e
}
```

### Semantics

`unquote` is compiled into the same term as its body.

### Type

```neut
Γ ⊢ⁱ⁺¹ e: 'a
-------------------
Γ ⊢ⁱ unquote {e}: a
```

### Note

Given a term `e` at stage n + 1, `unquote {e}` is at stage n:

```neut
define use-code() -> int {
  // here is stage 0
  unquote {
    // here is stage 1
    quote {10}
  }
}
```

## `promote`

You can use `promote` to create code without changing stages.

### Example

```neut
define-meta make-message<a>() -> 'unit {
  let t = magic show-type(a);
  quote {
    print(unquote {promote {t}});
    Unit
  }
}
```

### Syntax

```neut
promote {
  e
}
```

### Semantics

`promote` is compiled into the same term as its body.

### Type

```neut
Γ ⊢ⁱ e: a
--------------------
Γ ⊢ⁱ promote {e}: 'a
```

### Note

Unlike `quote`, `promote` doesn't alter stages.

## `{e}`

`{e}` is a grouped term.

### Example

```neut
define use-braces() -> int {
  mul-int({add-int(1, 2)}, 3)
}

define use-braces-with-let() -> int {
  let result = {
    let x = add-int(1, 2);
    mul-int(x, 3)
  };
  result
}
```

### Syntax

```neut
{e}
```

### Semantics

`{e}` is the same term as `e`. It only groups the enclosed term.

### Type

```neut
Γ ⊢ e: a
-----------
Γ ⊢ {e}: a
```

### Note

- The same grouping syntax is available in type positions. In other words, `{ t }` can be used to group a type expression.

## `lift`

You can use `lift` to wrap the types of "safe" values by `+`.

### Example

```neut
define lift-int(x: int) -> +int {
  lift {x}
}

define lift-bool(x: bool) -> +bool {
  lift {x}
}

define lift-text(t: text) -> +text {
  lift {t}
}

define lift-function(f: (int) -> bool) -> +(int) -> bool {
  lift {f} // error; won't typecheck
}
```

### Syntax

```neut
lift {e}
```

### Semantics

```neut
lift {e}

↓

e
```

### Type

```neut
Γ ⊢ e: a
(a is an "actual" type)
-----------------------
Γ ⊢ lift {e}: +a
```

Here, an "actual" type is a type that satisfies all the following conditions:

- It doesn't contain any free variables
- It doesn't contain any noetic types
- It doesn't contain any function types
- It doesn't contain any "dubious" ADTs

Here, a "dubious" ADT is something like the following:

```neut
// the type `joker-x` is dubious since it contains a noetic type
data joker-x {
| Joker-X(&list(int))
}

// the type `joker-y` is dubious since it contains a functional type
data joker-y {
| Joker-Y(int -> bool)
}

// the type `joker-z` is dubious since it contains a dubious ADT type
data joker-z {
| Joker-Z(joker-y)
}
```

### Note

(1) Unlike `box`, `lift` doesn't alter layers.

(2) `lift` doesn't add extra expressiveness to the type system. For example, `lift` on `bool` can be replaced with `box` as follows:

```neut
define lift-bool(b: bool) -> +bool {
  lift {b}
}

↓

define lift-bool(b: bool) -> +bool {
  if b {
    box {True}
  } else {
    box {False}
  }
}
```

`lift` on `either(bool, unit)` can also be replaced with `box` as follows:

```neut
define lift-either(x: either(bool, unit)) -> +either(bool, unit) {
  lift {x}
}

↓

define lift-either(x: either(bool, unit)) -> +either(bool, unit) {
  match x {
  | Left(b) =>
    if b {
      box {Left(True)}
    } else {
      box {Left(False)}
    }
  | Right(u) =>
    box {Right(Unit)}
  }
}
```

`lift` is there only for convenience.

One useful example is `text`: since `static` has type `text`, you can lift embedded text directly. If you first convert it to `&string` using `core.string.from-text`, it is no longer liftable.

## `pack-type`

`pack-type {t}` turns a type `t` into a term of type `type`.

### Example

```neut
define get-type<a>(x: a) -> type {
  let tvar = pack-type {a};
  tvar
}
```

### Syntax

```neut
pack-type {t}
```

### Semantics

`pack-type {t}` creates a first-class value from the type `t`.

### Type

```neut
Γ ⊢ t: type
-----------------------
Γ ⊢ pack-type {t}: type
```

### Note

- `pack-type` is useful when used in combination with `magic call-type`.

## `unpack-type`

`unpack-type a = e1; e2` binds the type represented by `e1` to the type variable `a`, and then evaluates `e2`.

### Example

```neut
define use-unpack-type(arg: type) -> type {
  unpack-type a = arg;
  pack-type {list(a)}
}
```

### Syntax

```neut
unpack-type a = e1;
e2
```

### Semantics

`unpack-type a = e1; e2` lets you use a term of type `type` in type positions.

### Type

```neut
Γ ⊢ e1: type
Γ, α: type ⊢ e2: b
----------------------------
Γ ⊢ unpack-type α = e1; e2: b
```

## `magic`

You can use `magic` to perform low-level operations. Using `magic` is unsafe.

### Example

```neut
// empty type
data descriptor {}

// add an element to the empty type
constant stdin: descriptor {
  magic cast(int, descriptor, 0) // ← cast
}

define malloc-then-free() -> unit {
  // allocates a memory region (stack)
  let ptr = magic alloca(int64, 2); // allocates (64 / 8) * 2 = 16 bytes

  // allocates a memory region (heap)
  let size: int = 10;
  let ptr: pointer = magic external malloc(size); // ← external

  // stores a value
  let value: int = 123;
  magic store(int, value, ptr); // ← store

  // loads and prints a value
  let value = magic load(int, ptr); // ← load
  print-int(value); // => 123

  // tells the compiler to treat the content of {..} as a value
  let v =
    magic opaque-value {
      get-some-c-constant-using-FFI()
    };

  // frees the pointer
  magic external free(ptr); // ← external

  // call types as functions
  let t: string = *"hello";
  magic call-type(string, 0, t); // ← call-type (discard)

  Unit
}

```

### Syntax

```neut
magic cast(from-type, to-type, value)

magic store(lowtype, stored-value, address)

magic load(lowtype, address)

magic alloca(lowtype, num-of-elems)

magic calloc(num-of-elems, size)

magic malloc(size)

magic realloc(pointer, size)

magic free(pointer)

magic opaque-value { e }

magic external func-name(e1, ..., en)

magic external func-name(e1, ..., en)(vararg-1: lowtype-1, ..., vararg-n: lowtype-n)

magic call-type(some-type, switch, arg)

magic inspect-type(some-type)

magic eq-type(type-1, type-2)

magic show-type(some-type)

magic string-cons(rune, string)

magic string-uncons(string)

magic compile-error(message)
```

A "lowtype" is a term that reduces to one of the following:

- `int1`, `int2`, `int4`, `int8`, `int16`, `int32`, `int64`
- `float16`, `float32`, `float64`
- `pointer`

You can also use `int` and `float` as a lowtype. These are just syntactic sugar for `int64` and `float64`, respectively.

### Compile-Time Primitives

The forms

- `magic inspect-type(some-type)`
- `magic eq-type(type-1, type-2)`
- `magic show-type(some-type)`
- `magic string-cons(rune, string)`
- `magic string-uncons(string)`
- `magic compile-error(message)`

are compile-time primitives.

These forms can only be used at stage 1 or above. The compiler reports an error if they are used below stage 1. They are resolved during compile-time evaluation and can therefore be used in `inline-meta` or `define-meta`.

### Semantics (cast)

`magic cast(a, b, e)` casts the term `e` from the type `a` to `b`. `cast` does nothing at runtime.

### Semantics (store)

`magic store(lowtype, value, address)` stores a value `value` to `address`. This is the same as `store` [in LLVM](https://llvm.org/docs/LangRef.html#store-instruction).

### Semantics (load)

`magic load(lowtype, address)` loads a value from `address`. This is the same as `load` [in LLVM](https://llvm.org/docs/LangRef.html#load-instruction).

### Semantics (alloca)

`magic alloca(lowtype, num-of-elems)` allocates a memory region on the stack frame. This is the same as `alloca` [in LLVM](https://llvm.org/docs/LangRef.html#alloca-instruction).

### Semantics (calloc)

`magic calloc(num-of-elems, size)` allocates a zero-initialized memory region on the heap.

### Semantics (malloc)

`magic malloc(size)` allocates a memory region on the heap.

### Semantics (realloc)

`magic realloc(pointer, size)` resizes a memory region on the heap.

### Semantics (free)

`magic free(pointer)` deallocates a memory region on the heap.

### Semantics (opaque-value)

`magic opaque-value { e }` tells the compiler to treat the term `e` as a value. You may want to use this in combination with `define` or `inline` that don't have any explicit arguments.

### Semantics (external)

`magic external func(e1, ..., en)` can be used to call foreign functions (or FFI). See [foreign in Statements](./statements.md#foreign) for more information.

`magic external func(e1, ..., en)(e{n+1}: lowtype1, ..., e{n+m}: lowtypem)` can also be used to call variadic foreign functions like `printf` in C.

### Semantics (call-type)

Neut compiles types into functions. The first argument of such a function is usually 0 or 1, but we can actually pass other integers using `call-type`.

`magic call-type(some-type, switch, arg)` treats `some-type` as a function pointer and calls `some-type(switch, arg)`.

`magic call-type(some-type, 0, value)` discards `value`.

`magic call-type(some-type, 1, value)` copies `value` and returns a new value.

`magic call-type(some-type, 2, value)` returns the size of a value in words. This value is used when calling a function in destination-passing style. If it is non-negative, the caller prepares a destination of that size. Otherwise, the caller uses a one-word temporary slot. For `resource` types, this size is given by the third term of the `resource` definition.

The type of the result of `call-type` is inferred from the context.

### Semantics (inspect-type)

`magic inspect-type(some-type)` inspects the given type and returns a structured value of type `type-value`.

### Semantics (eq-type)

`magic eq-type(type-1, type-2)` compares two types at compile time and returns whether they are equal.

### Semantics (show-type)

`magic show-type(some-type)` returns a string representation of the given type.

### Semantics (string-cons)

`magic string-cons(rune, string)` prepends `rune` to `string`.

### Semantics (string-uncons)

`magic string-uncons(string)` decomposes `string` into either the empty case or a pair of its first rune and the remaining string.

### Semantics (compile-error)

`magic compile-error(message)` reports a compile-time error with the given message when evaluated.

### Type

```neut
Γ ⊢ t1: type
Γ ⊢ t2: type
Γ ⊢ e: t1
-----------------------------
Γ ⊢ magic cast(t1, t2, e): t2


(t is a lowtype)
Γ ⊢ stored-value: t
Γ ⊢ address: pointer
------------------------------------------------------
Γ ⊢ magic store(t, stored-value, address): unit


(t is a lowtype)
Γ ⊢ t: type
Γ ⊢ e: pointer // address
------------------------------------------------------
Γ ⊢ magic load(t, e): t


Γ ⊢ num-of-elems: c-size
Γ ⊢ size: c-size
------------------------------------------------------
Γ ⊢ magic calloc(num-of-elems, size): pointer


Γ ⊢ size: c-size
------------------------------------------------------
Γ ⊢ magic malloc(size): pointer


Γ ⊢ pointer: pointer
Γ ⊢ size: c-size
------------------------------------------------------
Γ ⊢ magic realloc(pointer, size): pointer


Γ ⊢ pointer: pointer
------------------------------------------------------
Γ ⊢ magic free(pointer): unit


Γ ⊢ e:t
------------------------------------------------------
Γ ⊢ magic opaque-value { e }: t


Γ ⊢ e1: t1
...
Γ ⊢ en: tn
Γ ⊢ t: type
(t1 is a lowtype)
...
(tn is a lowtype)
(t is a lowtype or void)
(func is a foreign function)
--------------------------------------------------
Γ ⊢ magic external func(e1, ..., en): t


Γ ⊢ e1: t1
...
Γ ⊢ en: tn
Γ ⊢ e{n+1}: t{n+1}
...
Γ ⊢ e{n+m}: t{n+m}
Γ ⊢ t: type
(t1 is a lowtype)
...
(tm is a lowtype)
(t is a lowtype or void)
(func is a foreign function)
---------------------------------------------------------------------------------
Γ ⊢ magic external func(e1, ..., en)(e{n+1}: t{n+1}, ..., e{n+m}: t{n+m}): t


Γ ⊢ t: type
Γ ⊢ switch: int
Γ ⊢ arg: s
------------------------------------------------------
Γ ⊢ magic call-type(t, switch, arg): u


Γ ⊢ t: type
------------------------------------------------------
Γ ⊢ magic inspect-type(t): type-value


Γ ⊢ t1: type
Γ ⊢ t2: type
------------------------------------------------------
Γ ⊢ magic eq-type(t1, t2): bool


Γ ⊢ t: type
------------------------------------------------------
Γ ⊢ magic show-type(t): &string


Γ ⊢ r: rune
Γ ⊢ s: &string
------------------------------------------------------
Γ ⊢ magic string-cons(r, s): &string


Γ ⊢ s: &string
------------------------------------------------------
Γ ⊢ magic string-uncons(s): either(unit, pair(rune, &string))


Γ ⊢ a: type
Γ ⊢ message: &string
------------------------------------------------------
Γ ⊢ magic compile-error(message): a

```

## `introspect`

You can use `introspect key {..}` to introspect the compiler's configuration.

### Example

```neut
define arch-dependent-constant() -> int {
  introspect architecture {
  | arm64 =>
    1
  | amd64 =>
    2
  }
}

define os-dependent-constant() -> int {
  introspect operating-system {
  | linux =>
    1
  | default =>
    // `2` is returned if the target OS isn't Linux
    2
  }
}
```

### Syntax

```neut
introspect key {
| value-1 =>
  e1
  ...
| value-n =>
  en
}
```

You can use the following configuration `key`s and configuration `value`s:

| Configuration Key  | Configuration Value    |
| ------------------ | ---------------------- |
| `architecture`     | `amd64` or `arm64`     |
| `operating-system` | `linux` or `darwin`    |
| `build-mode`       | `develop` or `release` |

You can also use `default` as a configuration value to represent a fallback case.

### Semantics

First, `introspect key {v1 => e1 | ... | vn => en}` looks up the configuration value `v` of the compiler by `key`. Then it reads the configuration values `v1`, ..., `vn` in this order to find `vk` that is equal to `v`. If such a `vk` is found, `introspect` executes the corresponding clause `ek`. If no such `vk` is found, `introspect` reports a compilation error.

The configuration value `default` is equal to any configuration value.

### Type

```neut
(key is a configuration key)

(v1 is a configuration value)
Γ ⊢ e1: a

...

(vn is a configuration value)
Γ ⊢ en: a
------------------------------------------
Γ ⊢ introspect key {
    | v1 => e1
      ...
    | vn => en
    }: a
```

### Note

- The branching of an `introspect` is resolved at compile time.

## `static`

You can use `static` to create a value of type `text`.

### Example

```neut
import {
  core.string {from-text},
  text-file {some-file},
}

define use-some-file() -> unit {
  let t: text = static some-file;
  let s: &string = from-text(t);
  print(s)
}

define use-static-literal() -> unit {
  let t: text = static "hello";
  let s: &string = "hello";
  print(from-text(t));
  print(s)
}
```

### Syntax

```neut
static some-file
static "hello"
static "Hello, world!\n"
static "\u{1f338} ← Cherry Blossom"
```

Below is a list of all escape sequences available in Neut string literals:

| Escape Sequence | Meaning                        |
| --------------- | ------------------------------ |
| `\0`            | U+0000 (null character)        |
| `\t`            | U+0009 (horizontal tab)        |
| `\n`            | U+000A (line feed)             |
| `\r`            | U+000D (carriage return)       |
| `\"`            | U+0022 (double quotation mark) |
| `\\`            | U+005C (backslash)             |
| `` \` ``        | U+0060 (backtick)              |
| `\u{n}`         | U+n                            |

The `n` in `\u{n}` must be a lowercase hexadecimal number.

### Semantics

The compiler expands `static foo` into the content of `foo` at compile time.

If `foo` isn't a key of a UTF-8 text file, `static foo` reports a compilation error.

The expression `static "hello"` creates a value of type `text` from the given string literal.

A static literal is compiled into a pointer to a tuple like the following:

```text
(0, length-of-string, array-of-characters)
```

This tuple is static. More specifically, a global constant like the following is inserted into the resulting IR.

```llvm
@"text-hello" = private unnamed_addr constant {i64, i64, [5 x i8]} {i64 0, i64 5, [5 x i8] c"hello"}
```

And a term like `static "hello": text` is compiled into `ptr @"text-hello"`.

### Type

```neut
(Γ is a context)
(k is a text file's key)
--------------------------------------------
Γ ⊢ static k: text

(Γ is a context)
(s is a string literal)
-------------------------------------------
Γ ⊢ static s: text
```

### Note

- Since `text` is primitive, `static` can be lifted.
- You may also want to read [the section on text files in Modules](modules.md#text-file).

## `admit`

You can use `admit` to suppress the type checker and sketch the structure of your program.

### Example

```neut
define my-complex-function() -> unit {
  admit
}
```

### Syntax

```neut
admit
```

### Semantics

Evaluating `admit` exits the program and displays a message like the following:

```text
admit: /path/to/file.nt:1:2
```

When `admit` exits a program, the exit code is 1.

### Type

```neut
Γ ⊢ t: type
------------
Γ ⊢ admit: t
```

### Note

- `admit` is the `undefined` in Haskell.
- `admit` is intended to be used ephemerally during development.

## `assert`

You can use `assert` to ensure that a condition is satisfied at runtime.

### Example

```neut
define fact(n: int) -> int {
  assert "the input must be non-negative" {
    ge-int(n, 0)
  };
  if eq-int(n, 0) {
    1
  } else {
    mul-int(n, fact(sub-int(n, 1)))
  }
}
```

### Syntax

```neut
assert "any-string" {
  e
}
```

### Semantics

If the [build mode](./commands.md#--mode) is `release`, `assert` does nothing.

Otherwise, `assert "description" { condition }` evaluates `condition` and checks if it is `True`. If it is `True`, the `assert` simply evaluates to `Unit`. Otherwise, it reports that the assertion `"description"` failed and exits the execution of the program with the exit code `1`.

### Type

```neut
Γ ⊢ condition: bool
--------------------------------------------
Γ ⊢ assert "description" { condition }: unit
```

## `_`

`_` is a hole that must be inferred by the type checker.

### Example

```neut
define id<a>(x: a) -> a {
  x
}

define use-hole() -> unit {
  id<_>(Unit) // ← using a hole (inferred to be `unit`)
}
```

### Syntax

```neut
_
```

### Semantics

`_` is a hole that must be inferred by the type checker. If the type checker resolves a hole into a term `e`, this hole behaves the same as `e`. If the type checker can't resolve a hole, the type checker reports a compilation error.

### Type

N/A

## `on`

`let p on y = e1; e2` can be used to introduce noetic values in a specific scope.

### Example

```neut
define play-with-on() -> int {
  let xs: list(int) = List[1, 2, 3];
  let len on xs =
    // the type of `xs` is `&list(int)` here
    length(xs);
  // the type of `xs` is `list(int)` here
  add-int(len, 42)
}
```

### Syntax

```neut
let p on x1, ..., xn = e1;
e2

let p: t on x1, ..., xn = e1;
e2
```

### Semantics

```neut
let p on x1, ..., xn = e1;
e2

// ↓ desugar

letbox-T tmp on x1, ..., xn = lift {e1};
let p = tmp;
e2
```

### Type

Derived from the desugared form.

## `*e`

You can use `*e` to create an ordinary value from a noetic value.

### Example

```neut
define clone-list<a>(xs: &list(a)) -> list(a) {
  case xs {
  | Nil =>
    Nil
  | Cons(y, ys) =>
    Cons(*y, clone-list(ys))
  }
}
```

### Syntax

```neut
*e
```

### Semantics

```neut
*e

↓

embody(e)
```

where the function `embody` is defined in the core library as follows:

```neut
// core.box

// □A -> A (Axiom T)
inline axiom-T<a>(x: +a) -> a {
  letbox-T x-tmp = x;
  x-tmp
}

inline embody<a>(x: &a) -> a {
  axiom-T(box x {x}) // ← this `box` copies the content of `x`
}
```

### Type

Derived from the desugared form.

### Note

Intuitively, given a term `e: &a`, `*e: a` is a clone of the content of `e`.

This clone is created by copying the content along the type `a`.

The original content is kept intact.

## `e::(e1, ..., en)`

You can use `e::(e1, ..., en)` to call a meta function.

### Example

```neut
define-meta make-pair<a, b>(x: 'a, y: 'b) -> 'pair(a, b) {
  quote {
    let x = unquote {x};
    let y = unquote {y};
    Pair(x, y)
  }
}

define use-meta() -> pair(int, bool) {
  make-pair::(10, True)
}
```

### Syntax

```neut
e::(e1, ..., en)
```

### Semantics

`e::(e1, ..., en)` is the following syntactic sugar:

```neut
e::(e1, ..., en)

↓

unquote {e(quote {e1}, ..., quote {en})}
```

### Type

Derived from the desugared form.

## `name[x1, ..., xn]`

You can use `name[x1, ..., xn]` after defining `name` using [`rule-right`](./statements.md#rule-right) or [`rule-left`](./statements.md#rule-left).

### Example

```neut
define make-list() -> list(int) {
  List[1, 2, 3]
}
```

### Syntax

```neut
name[x1, ..., xn]
```

### Semantics

The expansion of `name[x1, ..., xn]` depends on whether `name` was introduced by [rule-right](./statements.md#rule-right) or [rule-left](./statements.md#rule-left).

### Type

Derived from the desugared form.

## `if`

You can use `if` as in other languages.

### Example

```neut
define foo(b1: bool) -> unit {
  if b1 {
    print("hey")
  } else {
    print("yo")
  }
}

define bar(b1: bool, b2: bool) -> unit {
  let tmp =
    if b1 {
      "hey"
    } else-if b2 {
      "yo"
    } else {
      "baz"
    };
  print(tmp)
}
```

### Syntax

```neut
if b1 { e1 } else-if b2 { e2 }  ... else-if b_{n-1} { e_{n-1} } else { en }
```

### Semantics

`if` is the following syntactic sugar:

```neut
if b1 { e1 } else-if b2 { e2 }  ... else-if b_{n-1} { e_{n-1} } else { en }

↓

match b1 {
| True => e1
| False =>
  match b2 {
  | True => e2
  | False =>
    ...
    match b_{n-1} {
    | True => e_{n-1}
    | False => en
    }
  }
}
```

### Type

Derived from the desugared form.

## `when cond { e }`

You can use `when cond { e }` to perform `e` only when `cond` is true.

### Example

```neut
define foo(b1: bool) -> unit {
  when b1 {
    print("hey")
  }
}

```

### Syntax

```neut
when cond {
  e
}
```

### Semantics

`when` is the following syntactic sugar:

```neut
when cond {
  e
}

↓

if cond {
  e
} else {
  Unit
}
```

### Type

Derived from the desugared form.

## `e1; e2`

You can use `e1; e2` to perform sequential operations.

### Example

```neut
define foo() -> unit {
  print("hello");
  print(", ");
  print("world!");
  print("\n")
}
```

### Syntax

```neut
e1;
e2
```

### Semantics

`e1; e2` is the following syntactic sugar:

```neut
let _: unit = e1;
e2
```

### Type

Derived from the desugared form.

## `try x = e1; e2`

`try` is a shorthand for `match` + `either`.

### Example

```neut
define get-value-or-fail() -> either(error, int) {
  //  ...
}

define foo() -> either(error, int) {
  try x1 = get-value-or-fail();
  try x2 = get-value-or-fail();
  Right(add-int(x1, x2))
}
```

### Syntax

```neut
try p = e1;
e2

try p on y1, ..., yn = e1;
e2
```

### Semantics

`try p = e1; e2` is shorthand for the following:

```neut
match e1 {
| Left(err) =>
  Left(err)
| Right(p) =>
  e2
}
```

You can also combine `try` with `on`:

```neut
try p on y1, ..., yn = e1;
e2

// ↓ desugar

let tmp on y1, ..., yn = e1;
try p = tmp;
e2
```

### Type

Derived from the desugared form.

### Note

The definition of `either` is as follows:

```neut
data either(a, b) {
| Left(a)
| Right(b)
}
```

## `tie x = e1; e2`

You can use `tie` as a noetic `let`.

### Example

```neut
data config {
| Config(
    foo: int,
    bar: bool,
  )
}

define use-noetic-config(c: &config) -> int {
  tie Config{foo} = c;
  *foo
}
```

### Syntax

```neut
tie p = e1;
e2
```

### Semantics

`tie p = e1; e2` is shorthand for the following:

```neut
case e1 {
| p =>
  e2
}
```

### Type

Derived from the desugared form.

## `pin x = e1; e2`

You can use `pin` to create a value and use it as a noema.

### Example

```neut
// without `pin`
define foo() -> int {
  let xs = make-list(123);
  let result on xs = some-func(xs);
  let _ = xs;
  result
}

↓

// with `pin`
define foo() -> int {
  pin xs = make-list(123);
  some-func(xs)
}
```

### Syntax

```neut
pin x = e1;
e2

pin x on y1, ..., yn = e1;
e2
```

### Semantics

```neut
pin x = e1;
e2

↓

let x = e1;
let tmp on x = e2;
let _ = x;
tmp
```

You can also combine `pin` with `on`:

```neut
pin x on y1, ..., yn = e1;
e2

↓

let x on y1, ..., yn = e1;
let tmp on x = e2;
let _ = x;
tmp
```

## `?t`

You can use `?t` to represent an optional type.

### Example

```neut
define foo(x: int) -> ?int {
  if eq-int(x, 0) {
    Right(100)
  } else {
    Left(Unit)
  }
}
```

### Syntax

```neut
?t
```

### Semantics

`?t` is the following syntactic sugar:

```neut
?t

↓

either(unit, t)
```

### Type

Derived from the syntactic sugar.
