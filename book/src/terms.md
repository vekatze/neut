# Terms

## Table of Contents

- `tau`
- Local Variables
- Top-Level Variables
- Literals
- `(x1: a1, ..., xn: an) -> b`
- `function (x1: a1, ..., xn: an) { e }`
- `define`
- `e(e1, ..., en)`
- `e of {x1 = e1, ..., xn = en}`
- `exact e`
- `let`
- ADT Formation
- ADT Introduction
- `match`
- `case`
- `&a`
- `on` (Noema Introduction)
- `*e` - Noema Elimination
- `magic`
- `introspect`
- `_`
- `use e {x} in cont`
- `e::x`
- `assert`
- `if`
- `when cond { e }`
- `e1; e2`
- `admit`
- `detach`, `attach`, and `new-channel`
- `try pat = e1 in e2`
- `tie pat = e1 in e2`
- `?t`
- `[e1, ..., en]`
- `with` / `bind`
- `{ e }`

## `tau`

`tau` is the type of types.

### Example

```neut
define sample(): unit {
  // `tau` used as a term
  let foo = tau in
  Unit
}

// `tau` used as a type
define identity(a: tau, x: a): a {
  x
}
```

### Syntax

```neut
tau
```

### Semantics

`tau` is compiled into a pointer to `base.#.imm`.

### Type

```neut
(Γ is a context)
----------------
  Γ ⊢ tau: tau
```

### Notes

- For any `x: tau`, `x` can be copied/discarded for free since `x` is lowered to an immediate.
- `tau` is used in combination with the inference rule of variables.

## Local Variables

### Example

```neut
define sample(): unit {
  // defining/using various local variables
  let x = Unit in
  let foo = x in
  let 'bar = foo in
  let buz' = 'bar in
  let theSpreadingWideMyNarrowHandsToGatherParadise = αβγ in
  let _h-e-l-l-o = buz' in
  let αβγ = _h-e-l-l-o in
  let 冥きより冥き道にぞ入りぬべきはるかに照らせ山の端の月 = Unit in
  let _ = Unit in

  // shadowing (not reassignment)
  let x = Unit in
  let x = tau in
  let x =
    function (x: bool) {
      x // x: bool
    }
  in
  Unit
}
```

### Syntax

The name of a local variable must satisfy the following conditions:

- It doesn't contain any of `=() "\n\t:;,<>[]{}/*`
- It doesn't start with `A, B, .., Z` (the upper case alphabets)

### Semantics

If the content of a variable `x` is an immediate, `x` is lowered to a name of a register which stores the immediate. Otherwise, `x` is lowered to a pointer to the content.

### Type

```neut
  Γ ⊢ a: tau
----------------
Γ, x: a ⊢ x: a
```

### Notes

- Variables in Neut are immutable. You'll need `core.cell` to achieve mutability.
- The compiler reports unused variables. You can use the name `_` to suppress those.

## Top-Level Variables

### Example

```neut
import {
- core.bool {bool}
- B
}

define sample(): unit {
  // using top-level variables
  let _ = bool // using an imported top-level name
  let _ = core.bool.bool // using the definite description of `core.bool.bool`
  let _ = B.bool // using a prefixed top-level name
  Unit
}
```

### Syntax

The name of a top-level variable is a (possibly) dot-separated symbols, where each symbol must satisfy the following conditions:

- It doesn't contain any of `=() "\n\t:;,<>[]{}/*`

### Semantics

A top-level variable `f` is lowered to the following 3-word tuple:

```
(base.#.imm, 0, POINTER_TO_FUNCTION(f))
```

See the Note below for more detailed explanation.

### Type

```neut
(Γ is a context)     (c: a is defined at the top-level)
-------------------------------------------------------
                  Γ ⊢ c: a
```

### Note

Let's see how top-level variables are lowered. Consider the following top-level functions:

```neut
// (source-dir)/sample.nt

// defining a top-level variable `increment`
define increment(x: int): int {
  add-int(x, 1)
}

define get-increment(): (int) -> int {
  increment // using a top-level variable `increment`
}
```

This `increment` and `get-increment` are lowered to LLVM functions like the below:

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

## Integers

### Example

```neut
define foo(): unit {
  let _: int = 100 in
  //           ^^^
  let _: int16 = 100 in
  //             ^^^
  Unit
}

```

### Syntax

`3`, `-16`, `424242`, etc.

### Semantics

The same as LLVM integers.

### Type

The type of an integer is unknown in itself, and must be inferred to be one of the followings:

- `int1`
- `int2`
- ...
- `int64`

### Note

- The type `int` is also available. For more, see [Primitives](./primitives.md#primitive-types).

## Floats

### Example

```neut
define foo(): unit {
  let _: float = 3.8 in
  //             ^^^
  let _: float32 = 3.8 in
  //             ^^^^^^
  Unit
}

```

### Syntax

`3.8`, `-0.2329`, etc.

### Semantics

The same as LLVM floats.

### Type

The type of an integer is unknown in itself, and must be inferred to be one of the followings:

- `float16`
- `float32`
- `float64`

### Note

- The type `float` is also available. For more, see [Primitives](./primitives.md#primitive-types).

## Texts

### Example

```neut
define foo(): unit {
  let _: &text = "test" in
  //             ^^^^^^
  Unit
}

```

### Syntax

`"hello"`, `"Hello, world!\n"`, etc.

### Semantics

A text literal is compiled into a pointer to a tuple like the below:

```text
(0, length-of-string, array-of-characters)
```

This tuple is static. That is, in terms of LLVM, a global constant like the below is inserted to the resulting IR.

```llvm
@"text-hello" = private unnamed_addr constant {i64, i64, [5 x i8]} {i64 0, i64 5, [5 x i8] c"hello"}
```

And a text like `"hello": &text` is compiled into `ptr @"text-hello"`.

### Type

```neut
(Γ is a context)  (t is a text literal)
---------------------------------------
         Γ |- t: &text
```

### Note

- In the current implementation, the set of recognized escape sequences like `\n` or `\t` are the same as that of Haskell.

## `(x1: a1, ..., xn: an) -> b`

`(x1: a1, ..., xn: an) -> b` is the type of functions.

### Example

```neut
// a function that accepts ints and returns bools
(value: int) -> bool

// this is equivalent to `(_: int) -> bool`:
(int) -> bool

// use `a`
(a: tau, x: a) -> a

// make the first argument implicit
<a: tau>(x: a) -> a

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

A function type is compiled into a pointer to `base.#.cls`. For more, please see [How to Execute Types](./on-executing-types.md)

### Type

```neut
  Γ, x1: a1, ..., xn: an, y1: b1, ..., ym: bm ⊢ c: tau
--------------------------------------------------------
Γ ⊢ <x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c: tau
```

## `function (x1: a1, ..., xn: an) { e }`

`function` can be used to create a lambda abstraction (an anonymous function).

### Example

```neut
define use-function(): int {
  let f =
    function (x: int, y: int) {
      let z = add-int(x, y) in
      mul-int(z, z)
    }
  in
  f(10, 20)
}
```

### Syntax

```neut
function (x1: a1, ..., xn: an) {
  e
}
```

### Semantics

A `function` is compiled into a three-word closure. For more, please see [How to Execute Types](./on-executing-types.md#advanced-function-types).

### Type

```neut
    Γ, x1: a1, ..., xn: an ⊢ e: t
-----------------------------------------
Γ ⊢ function (x1: a1, ..., xn: an) {e}: t

```

### Note

- Lambda abstractions defined by `function` are reduced at compile-time when possible. If you would like to avoid this behavior, consider using `define`.

## `define`

`define` (at the term-level) can be used to create a function with possible recursion.

### Example

```neut
define use-define(): int {
  let c = 10 in
  let f =
    // 🌟 term-level `define` with a free variable `c`
    define some-recursive-func(x: int): int {
      if eq-int(x, 0) {
        0
      } else {
        add-int(c, some-recursive-func(sub-int(x, 1)))
      }
    }
  in
  f(100)
}
```

### Syntax

```neut
define name<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm): c {
  e
}
```

The following abbreviations are available:

```neut
define name(y1: b1, ..., ym: bm): c {e}

// ↓
// define name<>(y1: b1, ..., ym: bm): c {e}


define name<a1, ..., an>(y1: b1, ..., ym: bm): c {e}

// ↓
// define name<a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c
```

### Semantics

A term-level `define` is lifted to a top-level definition using lambda lifting. For example, consider the following example:

```neut
define use-define(): int {
  let c = 10 in
  let f =
    // 🌟 term-level `define` with a free variable `c`
    define some-recursive-func(x: int): int {
      if eq-int(x, 0) {
        0
      } else {
        add-int(c, some-recursive-func(sub-int(x, 1)))
      }
    }
  in
  f(100)
}
```

The code above is compiled into something like the below:

```neut
// the free variable `c` is now a parameter
define some-recursive-func(c: int, x: int): int {
  if eq-int(x, 0) {
    0
  } else {
    let f =
      function (x: int) {
        some-recursive-func(c, x)
      }
    in
    add-int(c, f(sub-int(x, 1)))
  }
}

define use-define(): int {
  let c = 10 in
  let f =
    function (x: int) {
      some-recursive-func(c, x)
    }
  in
  f(100)
}
```

### Type

```neut
Γ, x1: a1, ..., xn: an, f: (x1: a1, ..., xn: an) -> t ⊢ e: t
------------------------------------------------------------
     Γ ⊢ (define f(x1: a1, ..., xn: an):t {e}): t
```

### Note

- Functions defined by term-level `define` aren't inlined at compile-time, even if it doesn't contain any recursions.

## `e(e1, ..., en)`

Given a function `e` and arguments `e1, ..., en`, we can write `e(e1, ..., en)` to write a function application.

### Example

```neut
define use-function(): unit {
  let _ = foo() in
  //      ^^^^^
  let _ = bar(1) in
  //      ^^^^^^
  let _ = buz("hello", True) in
  //      ^^^^^^^^^^^^^^^^^^
  Unit
}
```

### Syntax

```neut
e(e1, ..., en)
```

### Semantics

Given a funciton application `e(e1, ..., en)` the system does the following:

1. Computes `e`, `e1`, ..., `en` into values `v`, `v1`, ..., `vn`.
2. Extracts the content of the closure `v`, obtaining the label of the closed function and the tuple of the free variables.
3. Deallocates the tuple of the closure `v`.
4. Calls the function label is called with the tuple and `v1, ..., vn` as arguments.

### Type

```neut
Γ ⊢ e: <x1: a1, .., xn: an>(y1: b1, .., ym: bm) -> c    Γ ⊢ e1: b1  ..   Γ ⊢ em: bm
---------------------------------------------------------------------------------------
    Γ ⊢ e(e1, .., en): c[x1 := ?M1, .., xn := ?Mn, y1 := e1, .., ym := em]
```

The `?Mi`s in the above rule are metavariables that must be inferred by the compiler.

### Note

If the function `e` contains implicit arguments, holes are inserted automatically.

For example, consider the following code:

```neut
define id<a>(x: a): a {
  x
}

define use-id(): unit {
  id(Unit)
}
```

The `id(Unit)` in the example above is (conceptually) compiled into the below:

```neut
define _id(a: tau, x: a): a {
  x
}

define use-id(): unit {
  _id(_, Unit) // ← a hole `_` is inserted here
}
```

## `e of {x1 = e1, ..., xn = en}`

`e of {x1 = e1, ..., xn = en}` is an alternative notation of function application. Other languages would call this a feature of "keyword arguments".

### Example

```neut
define foo(x: int, y: bool, some-path: &text): unit {
  // whatever
}

define use-foo(): unit {
  // 🌟
  foo of {
  - x = 10
  - y = True
  - some-path = "/path/to/file"
  }
}
```

### Syntax

```neut
e of {
- x1 = e1
- ...
- xn = en
}
```

### Semantics

The same as `e(e1, ..., en)`.

### Type

The same as `e(e1, ..., en)`.

### Note

This notation might be useful when used in combination with ADTs:

```neut
data config {
- Config of {
  - count: int
  - path: &text
  - colorize: bool
  }
}

constant some-config {
  Config of {
  - count = 10
  - path = "/path/to/file"
  - colorize = True
  }
}
```

## `exact e`

Given a function `e`, `exact e` supplies all the implicit variables of `e` by inserting holes.

### Example

Suppose we have the following function:

```neut
define id<a>(x: a): a {
  x
}

define use-id() {
                           // 🌟
  let g: (x: int) -> int = exact id in
  Unit
}
```

Note that the following won't typecheck:

```neut
define id<a>(x: a): a {
  x
}

define use-id() {
  let g: (x: int) -> int = id in
  Unit
}
```

This is because the type of `id` is `<a>(x: a) -> a`.

### Syntax

```neut
exact e
```

### Semantics

Given a term `e` of type `<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c`,

```neut
exact e
```

is translated into the below:

```neut
function (y1: b1, ..., ym: bm) {
  e(_, ..., _, y1, ..., ym)
}
```

### Type

```neut
       Γ ⊢ e: <x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c
--------------------------------------------------------------------
Γ ⊢ exact e: ((y1: b1, ..., ym: bm) -> c)[x1 := ?M1, ..., xn := ?Mn]
```

where `?Mi`s are metavariables that must be inferred by the type checker.

### Note

As you can see from its semantics, an `exact` is just a shorthand of a "hole-application".

## `let`

### Example

```neut
define use-let(): unit {
  // 🌟 `let`
  let t = "test" in
  print(t)
}

define use-let(): unit {
  let bar =
    // 🌟 nested `let`
    let foo = some-func() in
    other-func(foo)
  in
  do-something(bar)
}

define use-let(): unit {
  // 🌟 `let` with a type annotation
  let t: &text = "test" in
  print(t)
}

```

`let` can be used to destruct an ADT value:

```neut
data item {
- Item(int, bool)
}

define use-item(x: item): unit {
  // 🌟 use `let` with a pattern
  let Item(i, b) = x in // ← here
  print-int(i)
}

define use-item-2(x: item): unit {
  // 🌟 use `let` with an of-pattern
  let Item of {i} = x in
  print-int(i)
}
```

### Syntax

```neut
let x = e1 in e2

let x: t = e1 in e2
```

### Semantics

`let x = e1 in e2` defines binds the result of `e1` to the variable `x`. This variable can then be used in `e2`.

### Type

```neut
Γ ⊢ e1: a     Γ, x: a ⊢ e2: b
-----------------------------
   Γ ⊢ let x = e1 in e2: b
```

### Remark

(1) `let x = e1 in e2` isn't exactly the same as `{function (x) {e2}}(e1)`. The difference lies in the fact that the type of `e2` can't depend on `x` in `let x = e1 in e2`.

(2) When a pattern is passed, `let` is the following syntax sugar:

```neut
let pat = x in
cont

↓

match x {
- pat =>
  cont
}
```

## ADT Formation

After defining an ADT using the statement `data`, the ADT types are made available.

### Example

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define use-nat-type(): tau {
  // 🌟
  my-nat
}
```

### Syntax

The same as that of top-level variables.

### Semantics

The same as that of top-level variables.

### Type

If an ADT `some-adt` is nullary, the type of `some-adt` is `tau`.

Otherwise, suppose that an ADT `some-adt` is defined as follows:

```neut
data some-adt(x1: a1, ..., xn: an) {..}
```

In this case, the type of `some-adt` is `(x1: a1, ..., xn: an) -> tau`.

## Constructors (ADT Introduction)

After defining an ADT using the statement `data`, the constructors can be used to construct values of the ADT.

### Example

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define create-nat(): my-nat {
  // 🌟 (`Succ` and `Zero` are constructors)
  Succ(Succ(Zero))
}
```

### Syntax

The same as that of top-level variables.

### Semantics

The same as that of top-level variables.

### Type

If a constructor `c` is nullary, the type of `c` is the ADT type. For example, consider the following code:

```neut
data some-adt {
- c1
}

data other-adt(a: tau) {
- c2
}
```

In this case,

- the type of `c1` is `some-adt`, and
- the type of `c2` is `other-adt(?M)`, where the `?M` must be inferred by the compiler.

If a constructor `c` isn't nullary, the type of `c` is the function type that takes specified arguments and turns them into the ADT type. For example, consider the following code:

```neut
data some-adt {
- c1(foo: int)
}

data other-adt(a: tau) {
- c2(bar: bool, buz: other-adt(a))
}
```

In this case,

- the type of `c1` is `(foo: int) -> some-adt`, and
- the type of `c2` is `<a: tau>(bar: bool, buz: other-adt(a)) -> other-adt(a)`.

## `match`

You can use `match` to destruct ADT values.

### Example

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define foo(n: my-nat): int {
  // 🌟
  match n {
  - Zero =>
    100
  - Succ(m) =>
    foo(m)
  }
}

define bar(n: my-nat): int {
  // 🌟 (You can use nested patterns)
  match n {
  - Zero =>
    100
  - Succ(Succ(m)) => // ← a nested pattern
    200

- Succ(m) =>
    foo(m)
  }
}

define eq-nat(n1: my-nat, n2: my-nat): bool {
  // 🌟 (`match` can handle multiple values)
  match n1, n2 {
  - Zero, Zero =>
    True
  - Succ(m1), Succ(m2) =>
    eq-nat(m1, m2)
  - _, _ =>
    False
  }
}


```

### Syntax

```neut
match e1, ..., en {
- pattern-1 =>
  body-1
- ...
- pattern-m =>
  body-m
}
```

### Semantics

The semantics of `match` is the same as the semantics of ordinary pattern matching except that ADT values are _consumed_ after branching.

For example, let's see how `my-nat` in the next code is used in `match`:

```neut
data my-nat {
- Zero
- Succ(my-nat)
}
```

The internal representation of `n: my-nat` is something like the below:

```neut
Zero:
  (0) // 1-word tuple
Succ:
  (1, pointer-to-m) // 2-word tuple
```

When evaluating `match`, the computer inspects the first element of the "tuple" `n`.

```neut
define foo(n: my-nat): int {
  // 🌟 (inspects the first element of `n` here)
  match n {
  - Zero =>
    100
  - Succ(m) =>
    foo(m)
  }
}
```

If the first element is `0`, which means in this case that we found an ADT value of `Zero`, the computer _frees_ the outer tuple of `(0)`, and then evaluates `100`.

If the first element is `1`, which means in this case that we found an ADT value of `Succ`, the computer gets the pointer to the second element of `n`, binds it to `m`, _frees_ the outer tuple of `(1, pointer-to-m)`, and then evaluates `foo(m)`.

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
    - pat-1 => body-1
    ...
    - pat-m => body-m
    }: b
```

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
    - Zero => 100
    - Succ(m) => foo(m)
    }: int
```

## `case`

You can use `case` to inspect noetic ADT values.

### Example

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define foo-noetic(n: &my-nat): int {
  case n {
  - Zero =>
    100
  - Succ(m) =>
    // the type of foo-noetic is `(&my-nat) -> int`
    foo-noetic(m)
  }
}
```

### Syntax

```neut
case e1, ..., en {
- pattern-1 =>
  body-1
- ...
- pattern-m =>
  body-m
}
```

### Semantics

The semantics of `case` is the same as `match` except that `case` doesn't consume ADT values.

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
    - pat-1 => body-1
    ...
    - pat-m => body-m
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
    - Zero => 100
    - Succ(m) => foo-noetic(m)
    }: int
```

## `&a`

Given a type `a: tau`, the `&a` is the type of noema over `a`.

### Example

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

                     // 🌟
define foo-noetic(n: &my-nat): int {
  case n {
  - Zero =>
    100
  - Succ(m) =>
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
Γ ⊢ t: tau
-----------
Γ ⊢ &t: tau
```

### Note

- Values of type `&a` can be created using `on`.
- Values of type `&a` are expected to be used in combination with `case` or `*e`.
- Since `&a` is compiled into `base.#.imm`, values of type `&a` aren't discarded or copied even when used non-linearly.

## `on`

`let x on y = e1 in e2` can be used to introduce noetic values in specific scope.

### Example

```neut
define play-with-let-on(): unit {
  let xs: list(int) = [1, 2, 3] in
  let len on xs =
    // the type of `xs` is `&list(int)` here
    length(xs)
  in
  // the type of `xs` is `list(int)` here
  print-int(len)
}
```

### Syntax

```neut
let y on x1, ..., xn = e1 in
e2
```

### Semantics

`on` is conceptually the following syntax sugar:

```neut
let result on x = e in
cont

// ↓ desugar

let x = unsafe-cast(a, &a, x) in // cast: `a` ~> `&a`
let result = e in                // (use `&a`)
let x = unsafe-cast(&a, a, x) in // uncast: `&a` ~> `a`
cont
```

### Type

```neut
Γ ⊢ x1: a1
...
Γ ⊢ xn: an
Γ, x1: &a1, ..., xn: &an ⊢ e1: b // note: the context `Γ` is ordered
Γ, y: b ⊢ e2: c
(the type `b` is realistic) // see the below note for the definition of "realistic"
---------------------------------------
Γ ⊢ let y on x1, ..., xn = e1 in e2: c
```

### Note

As you can see from the definition of `let-on`, a noema (noetic value) always has its "source". We'll call it the hyle of a noema.

A noema doesn't make sense if its hyle is discarded. This means, for example, we can break memory safety if `let-on` can return a noema:

```neut
let xs = [1, 2] in
let result on xs = xs in // **CAUTION** the result of let-on is a noema
let _ = xs in    // ← Since the variable `_` isn't used,
                 // the hyle of `result`, namely `xs: list(int)`, is discarded here
match result {   // ... and thus using `result` here is a use-after-free!
- [] =>
  print("hey")
- y :: ys =>
  print("yo")
}
```

Thus, we need to restrict the value `result` so that it can't contain any noemata. For example, types like `list(int)`, `unit`, or `except(list(int), text)` are allowed. types like `&text`, `list(a)`, `int -> bool` are disallowed.

More specifically, the type of `result` must be "realistic". That is, the type must satisfy all of the following conditions:

- It doesn't contain any free variables
- It doesn't contain any noetic types
- It doesn't contain any function types (since a noema can reside in it)
- It doesn't contain any "dubious" ADTs

Here, a "dubious" ADT is something like the below:

```neut
// the type `joker-x` is dubious since it contains a noetic argument
data joker-x {
- HideX(&list(int))
}

// the type `joker-y` is dubious since it contains a functional argument
data joker-y {
- HideY(int -> bool)
}

// the type `joker-z` is dubious since it contains a dubious ADT argument
data joker-z {
- HideZ(joker-y)
}
```

Indeed, if we were to allow returning these dubious ADTs, we can exploit them to hide a noema:

```neut
let result on xs = HideX(xs) in // the type of `result` is `jokerX` (dubious)
let _ = xs in                   // `xs` is discarded here
match result {
- HideX(xs) =>
  *xs                           // CRASH: use-after-free!
}
```

This restriction is checked at compile time by the type system of Neut.

## `*e`

You can use `*e` to create a non-noetic value from a noetic value.

### Example

```neut
define clone-list<a>(xs: &list(a)): list(a) {
  case xs {
  - Nil =>
    Nil
  - Cons(y, ys) =>
    Cons(*y, clone-list(ys))
  }
}
```

### Syntax

```neut
*e
```

### Semantics

Given a noema `e: &t`, `*e` is a clone of the hyle of the noema.

This clone is created by copying the hyle along the type `t`.

The original hyle is kept intact.

### Type

```neut
Γ ⊢ e: &a
----------
Γ ⊢ *e: a
```

## `magic`

You can use `magic` to perform weird stuff. Using a `magic` is an unsafe operation.

### Example

```neut
// empty type
data descriptor {}

// add an element to the empty type
constant stdin: descriptor {
  magic cast(int, descriptor, 0) // 🌟 cast
}

define malloc-then-free(): unit {
  // allocate memory region
  let size: int = 10 in
  let ptr: int = magic external malloc(size) in // 🌟 external

  // store a value
  let value: int = 123 in
  magic store(int, value, ptr); // 🌟 store

  // load and print a value
  let value = magic load(int, ptr) in // 🌟 load
  print-int(value); // => 123

  // free the pointer and return
  magic external free(ptr); // 🌟 external
  Unit
}

```

### Syntax

```neut
magic cast(from-type, to-type, value)

magic store(lowtype, stored-value, address)

magic load(lowtype, address)

magic external func-name(e1, ..., en)

magic external func-name(e1, ..., en)(vararg-1: lowtype-1, ..., vararg-n: lowtype-n)
```

A "lowtype" is one of the following:

- `int1`, `int2`, ..., `int64`
- `float16`, `float32`, `float64`
- `pointer`

Except for `cast`, the result type of `magic` is unspecified, so you must supply type annotations if necessary.

You can also use `int` and `float` as a lowtype. This is a platform-dependent lowtype. If the target architecture is 64-bit, `int` is interpreted as `int64`.

### Semantics

`magic cast (a, b, e)` casts the term `e` from the type `a` to `b`. This is just a trick against the type checker, and does nothing at runtime.

`magic store(lowtype, value, address)` stores a value `value` to `address`. This is the same as `store` [in LLVM](https://llvm.org/docs/LangRef.html#store-instruction).

`magic load(lowtype, address)` loads a value from `address`. This is the same as `load` [in LLVM](https://llvm.org/docs/LangRef.html#load-instruction).

`magic external func(e1, ..., en)` can be used to call foreign functions (or FFI). See [foreign in Statements](./statements.md#foreign) for more information.

`magic external func(e1, ..., en)(e{n+1}: lowtype1, ..., e{n+m}: lowtypem)` can also be used to call variadic foreign functions like printf in C. A use of such varidic `external` can be found in the core library [here](https://github.com/vekatze/neut-core/blob/6ef2fed68a6b0b063e15350e788c82ea9371f6bb/source/text/io.nt#L43).

### Type

```neut
Γ ⊢ t1: tau
Γ ⊢ t2: tau
Γ ⊢ e: t1
-----------------------------
Γ ⊢ magic cast(t1, t2, e): t2


Γ ⊢ stored-value: t1
Γ ⊢ address: t2
Γ ⊢ t3: tau
(value-type is a low-type)
------------------------------------------------------
Γ ⊢ magic store(value-type, stored-value, address): t3


Γ ⊢ address: t1
Γ ⊢ t2: tau
(value-type is a low-type)
------------------------------------------------------
Γ ⊢ magic load(value-type, address): t2


Γ ⊢ e1: t1
...
Γ ⊢ en: tn
Γ ⊢ t: tau
(func-name is a foreign function)
--------------------------------------------------
Γ ⊢ magic external func-name(e1, ..., en): t


Γ ⊢ e1: t1
...
Γ ⊢ en: tn
Γ ⊢ e{n+1}: t{n+1}
...
Γ ⊢ e{n+m}: t{n+m}
(lt-1 is a low-type)
...
(lt-m is a low-type)
Γ ⊢ t: tau
(func-name is a foreign function)
-----------------------------------------------------------------------------
Γ ⊢ magic external func-name(e1, ..., en)(e{n+1}: lt-1, ..., e{n+m}: lt-m): t
```

### Note

Except for `cast`, the result type of `magic` is unspecified. You may have to supply annotations.

## `introspect`

You can use `introspect key {..}` to introspect the configuration of the compiler.

### Example

```neut
define arch-dependent-constant(): int {
  introspect target-arch {
  - arm64 =>
    1
  - amd64 =>
    2
  }
}

define os-dependent-constant(): int {
  introspect target-os {
  - linux =>
    1
  - default =>
    // `2` is returned if target-os != linux
    2
  }
}
```

### Syntax

```neut
introspect key {
- value-1 =>
  e1
...
- value-n =>
  en
}
```

You can use the following configuration `key`s and configuration `value`s:

| Configuration Key | Configuration Value |
| ----------------- | ------------------- |
| `target-arch`     | `amd64` or `arm64`  |
| `target-os`       | `linux` or `darwin` |

You can also use `default` as a configuration value to represent a fallback case.

### Semantics

Firstly, `introspect key {- v1 => e1 ... - vn => en}` looks up the configuration value `v` of the compiler by `key`. Then it reads the configuration values `v1`, ..., `vn` in this order to find `vk` that is equal to the `v`. If such a `vk` is found, `introspect` executes the corresponding clause `ek`. If no such `vk` is found, `introspect` reports a compilation error.

The configuration value `default` is equal to to any configuration values.

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
    - v1 => e1
    ...
    - vn => en
    }: a
```

### Note

- `introspect` is resolved at compile-time.

## `_`

`_` is a hole that must be inferred by the type checker.

### Example

```neut
define id(a: tau, x: a): a {
  x
}

define use-hole(): unit {
  id(_, Unit) // ← using a hole (inferred to be `unit`)
}
```

### Syntax

```neut
_
```

### Semantics

`_` is a hole that must be inferred by the type checker. If the type checker resolves a hole into a term `e`, this hole behaves exactly the same as `e`. If the type checker can't resolve a hole, the type checker reports a compilation error.

### Type

```neut
Γ ⊢ e[tmp := e1]: a
-------------------
Γ ⊢ e[tmp := _]: a
```

## `use e {x} in cont`

You can use `use e {x} in cont` as a shorthand to destruct an ADT that has only one constructor.

### Example

```neut
data config {
- Config of {
  - path: &text
  - count: int
  }
}

define use-config(c: config): unit {
  use c {count} in
  print-int(count)
}

// cf.
define use-config-2(c: config): unit {
  let Config of {count} = c in
  print-int(count)
}
```

### Syntax

```neut
use e {x1, ..., xn} in
cont
```

### Semantics

`use` is the following syntax sugar:

```neut
use e {x1, ..., xn} in
cont

↓

let K of {x1, ..., xn} = e in
cont
```

Here, the `K` is the only one constructor of the type of `e`. If the type of e contains more than one constructors, `use` results in a compilation error.

### Type

Derived from the desugared form.

## `e::x`

`e::x` is a shorthand of the below:

```neut
use e {x} in
x
```

An example:

```neut
data config {
- Config of {
  - path: &text
  - count: int
  }
}

define use-config(c: config): unit {
  print-int(c::count)
}
```

## `assert`

`assert "explanation" { condition }` evaluates `condition` and check if it is `True`. If it is `True`, the `assert` evaluates to `Unit`. Otherwise, it reports that the assertion `"explanation"` failed and exits with `1`.

An example usage:

```neut
// a bit artificial but I believe you'll get the point
define fact(n: int): int {
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

## `if`

`if cond { e1 } else { e2 }` is a shorthand of the below:

```neut
match cond {
- True => e1
- False => e2
}
```

## `when cond { e }`

`when cond { e }` is a shorthand of the below:

```neut
match cond {
- True => e
- False => Unit
}
```

## `e1; e2`

`e1; e2` is a shorthand of the below:

```neut
let _: unit = e1 in
e2
```

## `admit`

`admit` is the `undefined` in Haskell. `admit` can have any type. Evaluating `admit` will exit the program, displaying a message like the below:

```text
admit: /path/to/file.nt:1:2
```

This `admit` is intended to be used ephemerally during development.

## `detach`, `attach`, and `new-channel`

Given a term `e: t`, `detach { e }` creates a term of type `flow(t)`. It creates a thread using pthreads and starts computation in the thread.

Given a term `e: flow(t)`, `attach { e }` creates a term of type `t`. It waits the computational flow in other thread to be completed and gets its resulting value.

An example:

```neut
let f1: flow(int) =
  // creates a thread and start computation
  detach {
    print("fA");
    1
  }
in
let f2: flow(int) =
  // creates a thread and start computation
  detach {
    print("fb");
    2
  }
in
let v1 = attach { f1 } in // waits f1 to be completed
let v2 = attach { f2 } in // waits f2 to be completed
print("hey")
```

### Channels

Flows can send/receive values using channels, like in Go.

You can create a channel using `new-channel`, and send/receive values using those channels.

```neut
let ch0 = new-channel(int) in
let ch1 = new-channel(int) in
// channels as queues
let result on ch0, ch1 =
  let f =
    detach {
      let message0 = receive(_, ch0) in // receive value from ch0
      send(int, ch1, add-int(message0, 1)); // send value to ch1
      message0
    }
  in
  let g =
    detach {
      let message1 = receive(_, ch1) in // receive value from ch1
      add-int(message1, 1)
    }
  in
  send(int, ch0, 0); // send value to ch0
  let v1 = attach { f } in
  let v2 = attach { g } in
  print("hey")
in
// ... cont ...
```

The type of a channel is `channel(a)`, where the `a` is the type of values that are sent/received. You'll use the noema of a channel because both `send` and `receive` expect the noema of a channel.

You can send a value into a channel using `send`, and receive one using `receive`.

A channel internally has a queue, and `send` stores a value to that queue.

When you call `receive`, if the queue isn't empty, the first element of the queue is extracted (the element is deleted from the queue). Otherwise, `receive` blocks until a value is sent to the queue.

## `try pat = e1 in e2`

`try pat = e1 in e2` is a shorthand of the below:

```neut
match e1 {
- Fail(err) =>
  Fail(err)
- Pass(pat) =>
  e2
}
```

## `tie pat = e1 in e2`

`tie pat = e1 in e2` is a shorthand of the below:

```neut
case e1 {
- pat =>
  e2
}
```

## `?t`

`?t` is a shorthand of `except(unit, t)`, where the `except` is defined in the core library as follows:

```neut
data except(a: tau, b: tau) {
- Fail(a)
- Pass(b)
}
```

## `[e1, ..., en]`

`[e1, ..., en]` is a shorthand of the below:

```neut
Cons(e1, Cons(..., Cons(en, Nil)))
```

That is, a shorthand for lists.

## `with` / `bind`

`with` and `bind` can be used as the "do-notations" in other languages.

`with` and `bind` is a syntax sugar defined as follows:

```neut
// (1) -----------------------------------------------------

with f {
  bind x = e1 in
  e2
}

↓

f(
  with f {e1},
  function (x) {
    with f {e2}
  }
)

// (2) -----------------------------------------------------

with f {
  LET x = e1 in // LET must be one of `let`, `try`, or `tie`
  e2
}

↓

LET x = with f {e1} in
with f {e2}


// (3) -----------------------------------------------------

with f {
  e1;
  e2
}

↓

with f {e1};
with f {e2}

// (4) -----------------------------------------------------

with f {
  use e {x1 ..., xn} in
  cont
}

↓

use e {x1, ..., xn} in
with f {cont}

// (5) -----------------------------------------------------

with f {e}

↓

e
```

where the rule `(5)` is used only when all the other rules are unavailable.

## `{ e }`

`{ e }` can be used as parentheses in other languages.
