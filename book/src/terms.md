# Terms

## Table of Contents

### Basics

- [type](#type)
- [Local Variables](#local-variables)
- [Top-Level Variables](#top-level-variables)
- [let](#let)

### Primitive Value

- [Integers](#integers)
- [Floats](#floats)
- [Strings](#strings)
- [Runes](#runes)

### Function

- [(x1: a1, ..., xn: an) -> b](#x1-a1--xn-an---b)
- [(x1: a1, ..., xn: an) => { e }](#x1-a1--xn-an---e-)
- [define f(x1: a1, ..., xn: an) -> c { e }](#define-fx1-a1--xn-an-c--e-)
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

### Miscellaneous

- [lift](#lift)
- [magic](#magic)
- [introspect](#introspect)
- [static](#static)
- [admit](#admit)
- [assert](#assert)
- [\_](#_)

### Syntactic Sugar

- [let x on y1, ..., yn = e1; e2](#on)
- [\*e](#e)
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
  let x = type;
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
- It doesn't start with `A, B, .., Z` (the upper case alphabets)

### Semantics

If the content of a variable `x` is an immediate value, `x` is compiled into the name of a register that stores the immediate. Otherwise, `x` is compiled into the name of a register that stores a pointer to the content.

### Type

```neut
  Γ ⊢ a: type
----------------
Γ, x: a ⊢ x: a
```

### Notes

- The compiler reports unused variables. You can use the name `_` to suppress those.
- Variables in Neut are immutable. You'll need `core.cell` to achieve mutability.

## Top-Level Variables

### Example

```neut
import {
  core.bool {bool},
}

define sample() -> unit {
  // using top-level variables
  let _ = bool; // using an imported top-level name
  let _ = core.bool.bool; // using the definite description of `core.bool.bool`
  Unit
}
```

### Syntax

The name of a top-level variable is a (possibly) dot-separated symbols, where each symbol must satisfy the following conditions:

- It doesn't contain any of ``=()' `\"\n\t:;,<>[]{}/*|&?``

### Semantics

A top-level variable `f` is compiled into the following 3-word tuple:

```
(base.#.imm, 0, POINTER_TO_FUNCTION(f))
```

See the Note below for a more detailed explanation.

### Type

```neut
(Γ is a context)     (c: a is defined at the top-level)
-------------------------------------------------------
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
let x = e1; e2

let x: t = e1; e2
```

### Semantics

`let x = e1; e2` binds the result of `e1` to the variable `x`. This `x` can then be used in `e2`.

### Type

```neut
Γ ⊢ e1: a     Γ, x: a ⊢ e2: b
-----------------------------
   Γ ⊢ let x = e1; e2: b
```

### Note

When a pattern is passed, `let` is the following syntactic sugar:

```neut
let pat = x;
cont

↓

match x {
| pat =>
  cont
}
```

## Integers

### Example

```neut
define foo() -> unit {
  let _: int = 100;
  //           ^^^
  let _: int16 = 100;
  //             ^^^
  Unit
}

```

### Syntax

`3`, `-16`, `424242`, etc.

### Semantics

The same as LLVM integers.

### Type

The type of an integer is unknown in itself. It must be inferred to be one of the following types:

- `int1`
- `int2`
- ...
- `int64`

### Note

- The type `int` is also available. For more, see [Primitives](./primitives.md#primitive-types).

## Floats

### Example

```neut
define foo() -> unit {
  let _: float = 3.8;
  //             ^^^
  let _: float32 = 3.8;
  //               ^^^
  Unit
}

```

### Syntax

`3.8`, `-0.2329`, etc.

### Semantics

The same as LLVM floats.

### Type

The type of a float is unknown in itself. It must be inferred to be one of the following types:

- `float16`
- `float32`
- `float64`

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
  let _: rune = `\n`;
  //            ^^^
  Unit
}

```

### Syntax

`` `A` ``, `` `\n` ``, `` `\u{123}` ``, etc.

The available escape sequences in rune literals are the same as those of the literal form of [`static`](./terms.md#static).

### Semantics

The value of a rune literal is a Unicode codepoint encoded in UTF-8.

The underlying representation of a rune is an int32.

### Type

```neut
(Γ is a context)  (c is a rune literal)
---------------------------------------
         Γ ⊢ c: rune
```

### Note

(1) You can write `` `\1234` ``, for example, to represent U+1234 (`` `ሴ` ``).

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

A string literal is shorthand for `magic cast(text, &string, static "hello")`.

### Type

```neut
(Γ is a context)  (t is a string literal)
---------------------------------------
        Γ ⊢ t: &string
```

### Note

- For the exact syntax and internal representation of the literal part, see [`static`](#static).

## `(x1: a1, ..., xn: an) -> b`

`(x1: a1, ..., xn: an) -> b` is the type of functions.

### Example

```neut
// a function that accepts ints and returns bools
(value: int) -> bool

// this is equivalent to `(_: int) -> bool`:
(int) -> bool

// using a type variable
<a: type>(x: a) -> a

// this is equivalent to `<a: _>(x: a) -> a`
<a>(x: a) -> a
```

### Syntax

```neut
<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c
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

### Semantics

A function type is compiled into a pointer to `base.#.cls`. For more, please see [How to Execute Types](./how-to-execute-types.md)

### Type

```neut
  Γ, x1: a1, ..., xn: an, y1: b1, ..., ym: bm ⊢ c: type
--------------------------------------------------------
Γ ⊢ <x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c: type
```

## `(x1: a1, ..., xn: an) => { e }`

`=>` can be used to create an anonymous function.

### Example

```neut
define use-function() -> int {
  let f =
    (x: int, y: int) => {
      let z = add-int(x, y);
      mul-int(z, z)
    };
  f(10, 20)
}
```

### Syntax

```neut
(x1: a1, ..., xn: an) => {
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

All the free variables of an anonymous function must be at the same layer of the anonymous function. For example, the following is not a valid term:

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

because the free variable `x` in the anonymous function is at layer 0, whereas the anonymous function is at layer -1.

For more on layers, please see the section on [box](#box), [letbox](#letbox), and [letbox-T](#letbox-t).

### Semantics

Anonymous functions are compiled into three-word closures. For more, please see [How to Execute Types](./how-to-execute-types.md#advanced-function-types).

### Type

```neut
    Γ, x1: a1, ..., xn: an ⊢ e: t
-----------------------------------------
Γ ⊢ (x1: a1, ..., xn: an) => {e}: t

```

### Note

- Anonymous functions are reduced at compile-time when possible. If you would like to avoid this behavior, consider using `define`.

## `define f(x1: a1, ..., xn: an) -> c { e }`

`define` (at the term-level) can be used to create a function with possible recursion.

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
```

The following abbreviations are available:

```neut
define name(y1: b1, ..., ym: bm) -> c {e}

// ↓
// define name<>(y1: b1, ..., ym: bm) -> c {e}


define name<a1, ..., an>(y1: b1, ..., ym: bm) -> c {e}

// ↓
// define name<a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c
```

As in `function`, all the free variables of a `define` must be at the same layer of the `define`.

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

### Type

```neut
Γ, x1: a1, ..., xn: an, f: (x1: a1, ..., xn: an) -> t ⊢ e: t
------------------------------------------------------------
     Γ ⊢ define f(x1: a1, ..., xn: an) -> t {e}: t
```

### Note

- Functions defined by term-level `define` aren't inlined at compile-time, even if it doesn't contain any recursions.

## `e(e1, ..., en)`

Given a function `e` and arguments `e1, ..., en`, we can write `e(e1, ..., en)` to write a function application.

### Example

```neut
define use-function() -> unit {
  let _ = foo();
  //      ^^^^^
  let _ = bar(1);
  //      ^^^^^^
  let _ = buz("hello", True);
  //      ^^^^^^^^^^^^^^^^^^
  Unit
}
```

### Syntax

```neut
e(e1, ..., en)
```

### Semantics

Given a function application `e(e1, ..., en)` the system does the following:

1. Computes `e`, `e1`, ..., `en` into values `v`, `v1`, ..., `vn`
2. Extracts the contents from the closure `v`, obtaining the tuple of its free variables and a function label
3. Deallocates the tuple of the closure `v`
4. Calls the function label with the tuple and `v1, ..., vn` as arguments

### Type

```neut
Γ ⊢ e: <x1: a1, .., xn: an>(y1: b1, .., ym: bm) -> c    Γ ⊢ e1: b1  ..   Γ ⊢ em: bm
---------------------------------------------------------------------------------------
    Γ ⊢ e(e1, .., em): c[x1 := ?M1, .., xn := ?Mn, y1 := e1, .., ym := em]
```

The `?Mi`s in the above rule are metavariables that must be inferred by the compiler.

## `e{x1 := e1, ..., xn := en}`

`e{x1 := e1, ..., xn := en}` is an alternative notation for function application.

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

inline some-config {
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
       Γ ⊢ e: <x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c
--------------------------------------------------------------------
Γ ⊢ exact e: {(y1: b1, ..., ym: bm) -> c}[x1 := ?M1, ..., xn := ?Mn]
```

Here, `?Mi`s are metavariables that must be inferred by the type checker.

### Note

As you can see from its semantics, an `exact` is just a shorthand of a "hole-application" that fills in implicit parameters.

## ADT Formation

After defining an ADT using the statement `data`, you can use the ADT.

### Example

```neut
data my-nat {
| Zero
| Succ(my-nat)
}

define use-nat-type() -> type {
  my-nat
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
| c1
}

data other-adt(a: type) {
| c2
}
```

In this case,

- the type of `c1` is `some-adt`, and
- the type of `c2` is `other-adt(?M)`, where the `?M` must be inferred by the compiler.

If a constructor `c` isn't nullary, the type of `c` is the function type that takes specified arguments and turns them into the ADT type. For example, consider the following code:

```neut
data some-adt {
| c1(foo: int)
}

data other-adt(a: type) {
| c2(bar: bool, buz: other-adt(a))
}
```

In this case,

- the type of `c1` is `(foo: int) -> some-adt`, and
- the type of `c2` is `<a: type>(bar: bool, buz: other-adt(a)) -> other-adt(a)`.

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

When evaluating `match`, the computer inspects the first element of the "tuple" `n`.

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

If the first element is `0`, which means that we found an ADT value of `Zero`, the computer _frees_ the outer tuple of `(0)`, and then evaluates `100`.

If the first element is `1`, which means that we found an ADT value of `Succ`, the computer gets the pointer to the second element of `n`, binds it to `m`, _frees_ the outer tuple of `(1, pointer-to-m)`, and then evaluates `foo(m)`.

### Type

```neut
Γ ⊢ e1: a1
...
Γ ⊢ en: an

Γ, arg_{1,1}: t_{1,1}, ..., arg_{1, k_{1}}: t{1, k_{1}} ⊢ pat-1: a1
Γ, arg_{1,1}: t_{1,1}, ..., arg_{1, k_{1}}: t{1, k_{1}} ⊢ body-1: b

...

Γ, arg_{m,1}: t_{m,1}, ..., arg_{m, k_{m}}: t{m, k_{m}} ⊢ pat-m: an
Γ, arg_{m,1}: t_{m,1}, ..., arg_{m, k_{m}}: t{m, k_{m}} ⊢ body-m: b

(for all i = 1, ..., m, pat-i is a pattern for e1, ..., en)
(the sequence pat-1, ..., pat-m is a exhaustinve matching against e1, ..., en)
------------------------------------------------------------------------------
Γ ⊢ match e1, ..., en {
    | pat-1 => body-1
    ...
    | pat-m => body-m
    }: b
```

The above might be a bit overwhelming. Please see the following Note for an example.

### Note

An example of the application of the typing rule of `match`:

```neut
Γ ⊢ n: my-nat

Γ ⊢ Zero: my-nat // pat-1
Γ ⊢ 100: int // body-1

Γ, m: my-nat ⊢ Succ(m): my-nat // pat-2
Γ, m: my-nat ⊢ foo(m): int // body-2

(Zero and Succ(m) are patterns for n)
(the sequence Zero, Succ(m) is a exhaustinve matching against n)
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

For every type `a`, `+a` is compiled into the same term as `a`.

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

Given a type `a: type`, the `&a` is the type of noemata over `a`.

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

`box e` can be used to "lift" the layer of `e`.

### Example

```neut
define use-noema<a>(x: &a, y: &a) -> +b {
  // layer 0
  // - x: &a at layer 0
  // - y: a  at layer 0
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

Given noetic variables `x1: &a1, ..., xn: &an`, the term `box x1, ..., xn { e }` copies all the `xi`s and execute `e`:

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
Γ1; ...; Γn; Δ ⊢ e1: a
------------------------------------- (□-intro)
Γ1; ...; Γn, &Δ ⊢ box Δ {e1}: +a
```

where `Γ1; ...; Γn` is a sequence of contexts.

### Layers

The body of `define` is defined to be at layer 0:

```neut
define some-function(x: int) -> int {
  // here is layer 0
  // `x: int` is a variable at layer 0
  add-int(x, 1)
}
```

Since `box e` lifts the layer of `e`, if we use `box` at layer 0, the layer of `e` will become -1:

```neut
define use-box(x: int) -> +int {
  // here is layer 0
  box {
    // here is layer -1
    10
  }
}
```

_In layer n, we can only use variables at the layer_. Thus, the following is not a valid term:

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

The body of this term is typed as follows:

```neut
--------------
x: int ⊢ x: int // layer -1
---------------------------
x: int ⊢ add-int(x, 1): int // layer -1
-------------------------------------------- (□-intro with Δ = (x: int))
· ; x: &int ⊢ box x {add-int(x, 1)}: +int  // layer 0
```

Here, `·` is the empty context.

---

Incidentally, the rule "The body of `define` is at layer 0" is not really necessary. We can simply replace the 0 with any integer.

### Note

Firstly, observe that the following derivation is admissible:

```neut
Γ1; ...; Γn; x: a, Δ ⊢ e: b
-------------------------------- (slide)
Γ1; ...; Γn, x: +a; Δ ⊢ e: b
```

Also, by setting `Δ = ·` in the typing rule of `box`, we obtain the following:

```neut
Γ1; ...; Γn; · ⊢ e: a
-------------------------------- (□-intro')
Γ1; ...; Γn ⊢ box Δ {e}: +a
```

Thus, we can perform the following derivation:

```neut
Γ1; ...; Γn; Δ ⊢ e: a
----------------------------- (slide)
...
----------------------------- (slide)
Γ1; ...; Γn, +Δ; · ⊢ e: a
-------------------------------------  (□-intro')
Γ1; ...; Γn, +Δ ⊢ box {e}: +a
```

That is to say, the following rule is admissible without using `&`:

```neut
Γ1; ...; Γn; Δ ⊢ e: a
------------------------------------- (□-intro-slide)
Γ1; ...; Γn, +Δ ⊢ box {e}: +a
```

Now, compare the above with the rule of `box`:

```neut
Γ1; ...; Γn; Δ ⊢ e: a
------------------------------------- (□-intro)
Γ1; ...; Γn, &Δ ⊢ box Δ {e}: +a
```

As you can see, we can obtain `(□-intro)` from `(□-intro-slide)` by replacing `+Δ` with `&Δ`. That is to say, `&a` is the "structurally-defined" variant of `+a`.

If we write `+Δ` instead of `&Δ` in `(□-intro)`, the rule is equivalent to `(□-intro')`. By giving the "structural" part a name different from `+`, the rule `(□-intro)` restricts the way how variables in `&Δ` (which could have been the same as `+Δ`) are used.

In this sense, `&a` is the T-necessity modality defined through structural rules.

## `letbox`

You can use `letbox` to "unlift" terms.

### Example

```neut
define roundtrip(x: +a) -> +a {
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
cont
```

### Type

```neut
Γ1; ...; Γn ⊢ e1: +a
Γ1; ...; Γn; Δ, x: a ⊢ e2: b
------------------------------------------------ (□-elim-K)
Γ1; ...; Γn; Δ ⊢ letbox x = e1; e2: b
```

### Note

Given a term `e1` at layer n + 1, `letbox x = e1; e2` is at layer n:

```neut
define roundtrip(x: +a) -> +a {
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

_In layer n, we can only use variables at the layer_. Thus, the following is not a valid term:

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
cont
```

### Type

```neut
Γ1; ...; Γn, &Δ ⊢ e1: +a
Γ1; ...; Γn, Δ, Δ', x: a ⊢ e2: b
-------------------------------------------------- (□-elim-T)
Γ1; ...; Γn, Δ, Δ' ⊢ letbox-T x on Δ = e1; e2: b
```

Note that the layer of `e1`, `e2`, `letbox-T (..)` are the same.

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

`on` doesn't alter the layers of variables, too:

```neut
define extract-value-from-meta(x: int) -> int {
  // here is layer 0
  // x: int (at layer 0)
  letbox-T tmp on x =
    // here is layer 0
    // x: &int (at layer 0)
    x;
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

Γ, arg_{1,1}: t_{1,1}, ..., arg_{1, k_{1}}: t{1, k_{1}} ⊢ pat-1: a1
Γ, arg_{1,1}: &t_{1,1}, ..., arg_{1, k_{1}}: &t{1, k_{1}} ⊢ body-1: b

...

Γ, arg_{m,1}: t_{m,1}, ..., arg_{m, k_{m}}: t{m, k_{m}} ⊢ pat-m: an
Γ, arg_{m,1}: &t_{m,1}, ..., arg_{m, k_{m}}: &t{m, k_{m}} ⊢ body-m: b

(for all i = 1, ..., m, pat-i is a pattern for e1, ..., en)
(the sequence pat-1, ..., pat-m is a exhaustinve matching against e1, ..., en)
------------------------------------------------------------------------------
Γ ⊢ case e1, ..., en {
    | pat-1 => body-1
    ...
    | pat-m => body-m
    }: b
```

### Note

An example of the application of the typing rule of `case`:

```neut
Γ ⊢ n: &my-nat

Γ ⊢ Zero: my-nat // pat-1
Γ ⊢ 100: int // body-1

Γ, m: my-nat ⊢ Succ(m): my-nat // pat-2
Γ, m: &my-nat ⊢ foo-noetic(m): int // body-2

(Zero and Succ(m) are patterns for n)
(the sequence Zero, Succ(m) is a exhaustinve matching against n)
------------------------------------------------------------------------------
Γ ⊢ case n {
    | Zero => 100
    | Succ(m) => foo-noetic(m)
    }: int
```

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

## `magic`

You can use `magic` to perform weird stuff. Using `magic` is an unsafe operation.

### Example

```neut
// empty type
data descriptor {}

// add an element to the empty type
constant stdin: descriptor {
  magic cast(int, descriptor, 0) // ← cast
}

define malloc-then-free() -> unit {
  // allocates memory region (stack)
  let ptr = magic alloca(int64, 2); // allocates (64 / 8) * 2 = 16 byte

  // allocates memory region (heap)
  let size: int = 10;
  let ptr: pointer = magic external malloc(size); // ← external

  // stores a value
  let value: int = 123;
  magic store(int, value, ptr); // ← store

  // loads and print a value
  let value = magic load(int, ptr); // ← load
  print-int(value); // => 123

  // tells the compiler to treat the content of {..} as a value
  let v =
    magic opaque-value {
      get-some-c-constant-using-FFI()
    };

  // frees the pointer and return
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

magic opaque-value { e }

magic external func-name(e1, ..., en)

magic external func-name(e1, ..., en)(vararg-1: lowtype-1, ..., vararg-n: lowtype-n)

magic external call-type(some-type, switch, arg)
```

A "lowtype" is a term that reduces to one of the following:

- `int1`, `int2`, `int4`, `int8`, `int16`, `int32`, `int64`
- `float16`, `float32`, `float64`
- `pointer`

You can also use `int` and `float` as a lowtype. These are just syntactic sugar for `int64` and `float64`, respectively.

### Semantics (cast)

`magic cast(a, b, e)` casts the term `e` from the type `a` to `b`. `cast` does nothing at runtime.

### Semantics (store)

`magic store(lowtype, value, address)` stores a value `value` to `address`. This is the same as `store` [in LLVM](https://llvm.org/docs/LangRef.html#store-instruction).

### Semantics (load)

`magic load(lowtype, address)` loads a value from `address`. This is the same as `load` [in LLVM](https://llvm.org/docs/LangRef.html#load-instruction).

### Semantics (alloca)

`magic alloca(lowtype, num-of-elems)` allocates a memory region on the stack frame. This is the same as `alloca` [in LLVM](https://llvm.org/docs/LangRef.html#alloca-instruction).

### Semantics (opaque-value)

`magic opaque-value { e }` tells the compiler to treat the term `e` as a value. You may want to use this in combination with `define` or `inline` that don't have any explicit arguments.

### Semantics (external)

`magic external func(e1, ..., en)` can be used to call foreign functions (or FFI). See [foreign in Statements](./statements.md#foreign) for more information.

`magic external func(e1, ..., en)(e{n+1}: lowtype1, ..., e{n+m}: lowtypem)` can also be used to call variadic foreign functions like printf in C. A use of such variadic `external` can be found in the core library [here](https://github.com/vekatze/neut-core/blob/6ef2fed68a6b0b063e15350e788c82ea9371f6bb/source/string/io.nt#L43).

### Semantics (call-type)

Neut compiles types into functions. The first argument of such a function is usually 0 or 1, but we can actually pass other integers using `call-type`.

`magic call-type(some-type, switch, arg)` treats `some-type` as a function pointer and calls `some-type(switch, arg)`.

`magic call-type(some-type, 0, value)` discards `value`.

`magic call-type(some-type, 1, value)` copies `value` and returns a new value.

`magic call-type(some-type, 2, value)` returns the size of a value in words. This value is used when calling a function in destination-passing style.

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
Γ ⊢ address: pointer
------------------------------------------------------
Γ ⊢ magic load(t, address): t

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


Γ ⊢ some-type: type
Γ ⊢ switch: int
------------------------------------------------------
Γ ⊢ magic call-type(some-type, switch, arg): t

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
    // `2` is returned if target-os != linux
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

Firstly, `introspect key {v1 => e1 | ... | vn => en}` looks up the configuration value `v` of the compiler by `key`. Then it reads the configuration values `v1`, ..., `vn` in this order to find `vk` that is equal to the `v`. If such a `vk` is found, `introspect` executes the corresponding clause `ek`. If no such `vk` is found, `introspect` will report a compilation error.

The configuration value `default` is equal to any configuration values.

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

- The branching of an `introspect` is resolved at compile-time.

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

Below is the list of all the escape sequences in Neut for the literal form:

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
(Γ is a context)    (k is a text file's key)
--------------------------------------------
Γ ⊢ static k: text

(Γ is a context)    (s is a string literal)
-------------------------------------------
Γ ⊢ static s: text
```

### Note

- `static` has type `text`, not `&string`.
- For example, `static "hello": text`, whereas `"hello": &string`.
- If you need an `&string`, use `core.string.from-text`.
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

Evaluating `admit` will exit the program, displaying a message like the following:

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

You can use `assert` to ensure that a condition is satisfied at run-time.

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

```neut
Γ ⊢ e[tmp := e1]: a
-------------------
Γ ⊢ e[tmp := _]: a
```

## `on`

`let x on y = e1; e2` can be used to introduce noetic values in a specific scope.

### Example

```neut
define play-with-let-on() -> int {
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
let y on x1, ..., xn = e1;
e2
```

### Semantics

```neut
let result on x1, ..., xn = e1;
e2

// ↓ desugar

letbox-T result on x1, ..., xn = lift {e1};
e2
```

### Type

Derived from the desugared form.

## `*e`

You can use `*e` to create a non-noetic value from a noetic value.

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
      "pohe"
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
try x = e1;
e2
```

### Semantics

`try x = e1; e2` is shorthand for the following:

```neut
match e1 {
| Left(err) =>
  Left(err)
| Right(x) =>
  e2
}
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

You can use `tie` as a "noetic" `let`.

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
tie x = e1;
e2
```

### Semantics

`tie x = e1; e2` is shorthand for the following:

```neut
case e1 {
| x =>
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
define foo() -> unit {
  let xs = make-list(123);
  let result on xs = some-func(xs);
  let _ = xs;
  result
}

↓

// with `pin`
define foo() -> unit {
  pin xs = make-list(123);
  some-func(xs)
}
```

### Syntax

```neut
pin x = e1;
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
