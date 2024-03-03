# Terms

A **term** in Neut is a syntactic construct that can be appeared in the body of `define`.

## Table of Contents

Lorem ipsum.

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

`tau` is lowered to a pointer to `base.#.imm`. Thus `tau` is lowered to an immediate.

### Inference Rule

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
  let f = function (x: bool) { x } in

  Unit
}
```

### Syntax

A token can be the name of a variable if it doesn't contain any of `=() "\n\t:;,<>[]{}/*`.

The name of a variable is interpreted as that of a local variable if the name is defined in a local scope (using `let` for example).

### Semantics

If the content of a variable `x` is an immediate, `x` is lowered to a name of a register which stores the immediate. Otherwise, `x` is lowered to a pointer to the content.

If a variable `x: a` is used n times, the content of `x` is copied along `a` for `n-1` times. If a variable `x: a` isn't used, the content is discarded. This copying/discarding happens immediately after definition.

### Inference Rule

```neut
  Γ ⊢ a: tau
----------------
Γ, x: a ⊢ x: a
```

### Notes

- Variables in Neut are immutable. You'll need `core.cell` to achieve mutability.
- The compiler detects and reports unused variables. You can use the name `_` to suppress it.

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

stub

A token can be the name of a top-level variable if it doesn't contain any of `=() "\n\t:;,<>[]{}/*`.

The name of a variable is interpreted as that of a top-level variable if the name is defined in a local scope (using `let` for example).

If a name is defined in a local scope (using `let` for example), the name is interpreted as that of a local variable. If the name is defined in the top-level scope (using `define` for example), the name is interpreted as that of a top-level variable.

### Semantics

A top-level variable `f` is lowered to the following 3-word tuple:

```
(base.#.imm, 0, POINTER_TO_FUNCTION(f))
```

See the Note below for more detailed explanation.

### Inference Rule

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

Incidentally, 3-word tuples of global variables are usually reduced at compile time.

## Literals

- integers
- floats
- texts

### Example

```neut
define foo(var: int): unit {
  let _: int = 100 in
  //           ^^^
  let _: float = 3.8 in
  //             ^^^
  let _: &text = "test" in
  //             ^^^^^^
  Unit
}

```

## `(x1: a1, ..., xn: an) -> b`

`(x1: a1, ..., xn: an) -> b` is the type of functions.

### Examples

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
↓
<>(y1: b1, ..., ym: bm) -> c


(b1, ..., bm) -> c
↓
(_: b1, ..., _: bm) -> c


<a1, ..., an>(y1: b1, ..., ym: bm) -> c
↓
<a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c
```

## `function (x1: a1, ..., xn: an) { e }`

## `define name(x1: a1, ..., xn: an): b { e }`

## Function Elimination

## `e of {x1 = e1, ..., xn = en}`

## `exact e`

Given a function `e`, `exact e` supplies all the implicit variables of `e` by inserting holes.

For example, suppose we have the following function:

```neut
define id<a>(x: a): a {
  x
}
```

Then,

```neut
define use-id() {
  let g: (x: int) -> int = exact id in
  Unit
}
```

Note that the following won't typecheck:

```neut
define use-id() {
  let g: (x: int) -> int = id in
  Unit
}
```

since the type of `id` is `<a>(x: a) -> a`.

`exact` is a machinery to resolve implicit arguments without using function application. You can think of `exact id` as a shorthand of the below:

```neut
function (x: a) {
  id(x)
}
```

Note that implicit holes are inserted at `id(x)` during type checking (elaboration).

## `let pat = e1 in e2`

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

## ADT Formation

After defining an ADT using the statement `data`, the ADT types are made available.

For example:

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define use-nat-type(): tau {
  my-nat
}
```

## ADT Introduction

After defining an ADT using the statement `data`, the constructors can be used to construct values of the ADT.

For example:

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define create-nat(): my-nat {
  Succ(Succ(Zero))
}
```

In the example above, the type of `Succ` is `(my-nat) -> my-nat`.

## ADT Elimination

## Noema Formation: `&a`

Given a type `a: tau`, the `&a` is the type of noema over `a`.

## `on` (Noema Introduction)

## `*e` - Noema Elimination

Given a noema `e: &t`, `*e: t` is a clone of the noema. The original noema is kept intact.

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

## `magic`

`magic` can be used to perform weird stuff. Using a `magic` is an unsafe operation.

Except for `cast`, the result type of `magic` is unspecified, so you must supply type annotations if necessary.

### `cast`

`magic cast (a, b, e)` casts the term `e` from the type `a` to `b`.

Example usage:

```neut
// empty type
data descriptor {}

// add an element to the empty type
constant stdin: descriptor {
  magic cast(int, descriptor, 0)
}
```

### `store` / `load`

`store(type, value, address)` stores a value `value` to `address`.

`load(type, address)` loads a value from `address`.

Example usage:

```neut
define malloc-then-free(): unit {
  // allocate memory region
  let size: int = 10 in
  let ptr: int = magic external malloc(size) in

  // store a value
  let value: int = 123 in
  magic store(int, value, ptr);

  // load and print a value
  let value = magic load(int, ptr) in
  print-int(value); // => 123

  // free the pointer and return
  magic external free(ptr);
  Unit
}
```

These magics are lowered to the `store` and `load` in LLVM IR after compilation.

### `external`

`external` can be used to call foreign functions. See [foreign in Statements](./statements.md#foreign) for more information.

## `introspect`

`introspect key {..}` introspects the setup of the compiler and select corresponding terms. It should look like the below:

```neut
define arch-dependent-constant(): int {
  introspect target-arch {
  - arm64 =>
    1
  - amd64 =>
    2
  }
}
```

Currently, the following keys/values are available:

| Key           | Value               |
| ------------- | ------------------- |
| `target-arch` | `amd64` or `arm64`  |
| `target-os`   | `linux` or `darwin` |

## `_`

`_` is a hole that must be inferred by the type checker. It should look like the below:

```neut
define id(a: tau, x: a): a {
  x
}

define use-hole(): unit {
  id(_, Unit) // ← using a hole (inferred to be `unit`)
}
```

If the compiler couldn't infer the content of the hole, the compiler reports an error.

## `use e {x} in cont`

`use e {x} in cont` is only valid when the `e` is an ADT with only one constructor.

Given such an `e`, `use e {x} in cont` is a shorthand of the below:

```neut
let K of {x} = e in
cont
```

where the `K` is the only constructor of `K`.

An example:

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

## `{ e }`

`{ e }` can be used as parentheses in other languages.
