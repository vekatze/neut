# Terms

A **term** in Neut is a syntactic construct that can be appeared in the body of `define`.

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
- `try pat = e1 in e2`
- `tie pat = e1 in e2`
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
- `assert`
- `if`
- `when cond { e }`
- `e1; e2`
- `admit`
- `detach`, `attach`, and `new-channel`
- `?t`
- `[e1, ..., en]`
- `with` / `bind`
- `e::x`
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

### Integers

Neut has integer literals like `3`, `-16`, `424242`, etc. The type of an integer value must be one of the followings:

- `int1`
- `int2`
- ...
- `int64`

The type `int` is also available. `int` is an architecture-dependent type. If the word size of the target platform is 64 bits, `int` is compiled into `int64`. If the word size of the target platform is 32 bits, `int` is compiled into `int32`, etc.

### Floats

Neut has float literals like `3.8`, `0.2329`, etc. The type of a float value must be one of the followings:

- `float16`
- `float32`
- `float64`

The type `float` is also available. `float` is an architecture-dependent type. If the word size of the target platform is 64 bits, `float` is compiled into `float64`. If the word size of the target platform is 32 bits, `float` is compiled into `float32`, etc.

### Texts

Neut has text literals like `"hello"` or `"Hello, world!\n"`. The type of a text literal is `&text`.

In the current implementation, the set of recognized escape sequences like `\n` or `\t` are the same as that of Haskell.

### Example

```neut
define foo(var: int): unit {
  let _: int = 100 in
  //           ^^^
  let _: int16 = 100 in
  //             ^^^
  let _: float = 3.8 in
  //             ^^^
  let _: float32 = 3.8 in
  //               ^^^
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

`function` can be used to create a lambda abstraction (an anonymous function).

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

Lambda abstractions defined by `function` are reduced at compile-time when possible. If you would like to avoid this behavior, consider using `define`.

## `define`

`define` (at the term-level) can be used to create a function with possible recursion.

```neut
define use-define(): int {
  let c = 10 in
  let f =
    // ↓ term-level `define`
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

Functions defined by term-level `define` aren't inlined at compile-time, even if it doesn't contain any recursions.

## `e(e1, ..., en)`

Given a function `e` and arguments `e1, ..., en`, we can write `e(e1, ..., en)` to write a function application.

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

Suppose that we have the following function:

```neut
define foo(x: int, y: bool, some-path: &text): unit {
  // ..
}
```

In this case, we can call this `foo` by specifying key-value pairs of its arguments:

```neut
define use-foo(): unit {
  foo of {
  - x = 10
  - y = True
  - some-path = "/path/to/file"
  }
}
```

This might be useful when used in combination with ADTs:

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
  - path ="/path/to/file"
  - colorize = True
  }
}
```

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

## `let`

`let x = e1 in e2` defines a variable `x` as the result of `e1` so that it can be used in `e2`:

```neut
define use-let(): unit {
  let t = "test" in
  print(t)
}
```

`let`s can be nested:

```neut
define use-let(): unit {
  let bar =
    let foo = some-func() in
    other-func(foo)
  in
  do-something(bar)
}
```

You can also add type annotations in `let`s:

```neut
define use-let(): unit {
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
  let Item(i, b) = x in // ← here
  print-int(i)
}

define use-item-2(x: item): unit {
  let Item of {i} = x in // ← here
  print-int(i)
}
```

When passing a pattern, `let` is the following syntax sugar:

```neut
let pat = x in
cont

↓

match x {
- pat =>
  cont
}
```

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

## ADT

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

## Constructors

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

## `match`

ADT values can be destructed using `match`:

```neut
data my-nat {
- Zero
- Succ(my-nat)
}

define foo(n: my-nat): int {
  match n {
  - Zero =>
    100
  - Succ(m) =>
    foo(m)
  }
}
```

Multiple values can be `match`ed at the same time:

```neut
// True iff `n1 == n2`
define eq-nat(n1: my-nat, n2: my-nat): bool {
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

### Memory Behavior

The arguments of `match` is _consumed_.

Let's see how `my-nat` in the next code is used in `match`:

```neut
data my-nat {
- Zero
- Succ(my-nat)
}
```

The internal representation of `n: my-nat` is something like the below:

```neut
Zero:
  (0)
Succ:
  (1, pointer-to-m)
```

That is, if `n` is `Zero`, the internal representation is a 1-word tuple that contains only one element `0`. If `n` is `Succ(m)`, the internal representation is a 2-word tuple that contains `1` and a pointer to `m`.

```neut
define foo(n: my-nat): int {
  match n {
  - Zero =>
    100
  - Succ(m) =>
    foo(m)
  }
}
```

When evaluating `match`, the computer inspects the first element of the "tuple" `n`.

If it is `0`, the computer frees the outer tuple of `(0)`, and goes to the branch of `Zero` (which is `100` in the example above).

If it is `1`, the computer gets the pointer to the second element of `n`, binds it to `m`, frees the outer tuple of `(1, pointer-to-m)`, and then goes to the branch of `Succ` (which is `foo(m)` in the example above).

## `case`

Given an ADT type `t`, `case` can be used to inspect values of type `&t`.

`case` is a "noetic" variant of `match`:

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
    foo-noetic(m)
  }
}
```

Note that the type of `m` in the example above is `&my-nat`, not `my-nat`. Contents of constructors are wrapped by `&(_)` after using `case`.

### Memory Behavior

The semantics of `case` is the same as `match` except that `case` doesn't consume (free) its arguments. `case` can be considered as a "read-only" version of `match`.

## `&a`

Given a type `a: tau`, the `&a` is the type of noema over `a`.

The values of type `&a` aren't discarded/copied when used non-linearly.

## `on` (Noema Introduction)

`let x on y = e1 in e2` can be used to introduce noetic values in specific scope.

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

`let-on` is essentially the following syntax sugar:

```neut
let result on x = e in
cont

// ↓ desugar

let x = unsafe-cast(a, &a, x) in // cast: `a` ~> `&a`
let result = e in                     // (use `&a`)
let x = unsafe-cast(&a, a, x) in // uncast: `&a` ~> `a`
cont
```

`let-on` can take multiple variables:

```neut
// ↓ the below is a valid term
let len on xs, ys, zs = e in
cont
```

The result of `let-on` (here, `len`) can't contain any noetic terms. Below is the reason.

### Result Type Restriction

As you can see from the definition of `let-on`, a noema always has its "source" value. We'll call it the hyle of a noema.

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

More specifically, the type of `result` must satisfy all of the followings:

- it doesn't contain any free variables,
- it doesn't contain any noetic types,
- it doesn't contain any function types (since a noema can reside in it), and
- it doesn't contain any "dubious" ADTs.

Here, a "dubious" ADT is an ADT like the below:

```neut
data joker-x {
- HideX(&A) // contains a noetic argument
}

data joker-y {
- HideY(int -> bool) // contains a functional argument
}

data joker-z {
- HideZ(joker-y) // contains a dubious ADT argument
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

`external` can be used to call foreign functions (or FFI). See [foreign in Statements](./statements.md#foreign) for more information.

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
