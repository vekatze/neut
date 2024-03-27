# Programming in Neut

Let's code in Neut. We'll see how to write basic programs in Neut here. You are assumed to be already familiar with functional programming.

## What You'll Learn Here

- How to define and use variables
- How to define and use functions
- How to define and use ADTs
- How to perform parallel computations
- Other syntactic utilities

## Programming in Neut

Let's create a new module `start` and edit `source/start.nt`:

```sh
neut create start
cd start
edit source/start.nt
```

### Binding Variables Using `let`

Rewrite `start.nt` into the below:

```neut
define hey(): unit {
  // 🌟
  let x = "hello" in
  let y: int = 100 in
  let z: float = 3.8 in
  print("hey")
}

define main(): unit {
  hey()
}
```

Building and executing the module should output `hey` to the stdout.

Points:

- Functions can be defined using `define`
- Functions can be called by writing `f(e1, ..., en)`
- `let` can be used to define variables

As you can see from the example above, `let` can be used to define variables.

You might have noticed that the compiler reports unused variables (`x`, `y`, and `z` in the example above). You can use the name `_` when defining variables to suppress those warnings:

```neut
define hey(): unit {
  // 🌟
  let _ = "hello" in
  let _: int = 100 in
  let _: float = 3.8 in
  print("hey")
}
```

`let`s can be nested:

```neut
define hey(): unit {
  let x =
    // 🌟
    let y: int = 100 in
    let z: float = 3.8 in
    "hello"
  in
  print(x) // => hello
}
```

`e1; e2` is a syntax sugar of `let _: unit = e1 in e2`:

```neut
define hey(): unit {
  // 🌟
  print("a");
  print("b")
}

// ↓

define hey(): unit {
  let _ = print("a") in
  print("b")
}
```

### Defining Functions at the Top Level

You can use the statement `define` to define functions:

```neut
// 🌟
// defining an ordinary function
define my-func1(x1: int, x2: bool): bool {
  x2
}

// 🌟
// defining a recursive function
define my-func2(cond: bool): int {
  if cond {
    1
  } else {
    my-func2(not(cond))
  }
}
```

`define` can also define a function with implicit arguments (or "generics"):

```neut
// 🌟
// The `a` in the angle bracket is the implicit argument of `id`
define id<a>(x: a): a {
  x
}

define use-id(): int {
  let str = 10 in
  id(str) // calling `id` without specifying `a` explicitly
}
```

The definition of `id` in the example above is the same as the below:

```neut
// you can explicitly write the type of `a`
define id<a: tau>(x: a): a {
  x
}
```

If we don't use any implicit arguments, the example of `id` and `use-id` becomes as follows (just for comparison):

```neut
define id(a: tau, x: a): a {
  x
}

define use-id(): int {
  let str = 10 in
  id(int, str)
}
```

### Defining Functions in a Body of a Function

You can use `function` to define an anonymous function:

```neut
define foo() {
  let f =
    // 🌟
    function (x: int, cond: bool) {
      if cond {
        x
      } else {
        add-int(x, 1)
      }
    }
  in
  f(10, False) // → 11
}
```

You can also use `define` in the body of a function to define recursive functions:

```neut
define foo() {
  let f =
    // 🌟
    define print-multiple-hellos(counter: int) {
      if eq-int(counter, 0) {
        Unit
      } else {
        print("hello\n");
        print-multiple-hellos(sub-int(counter, 1))
      }
    }
  in
  f(10) // prints 10 "hello"s
}
```

### Calling Functions

Functions `f` can be called against arguments `e1`, ..., `en` by writing `f(e1, ..., en)`:

```neut
define my-func(x: int, y: int): int {
  // 🌟
  add-int(x, y)
}

define use-my-func(): int {
  // 🌟
  my-func(10, 20)
}
```

The syntax sugar `of` can be used to rewrite the above `use-my-func` into the below:

```neut
define use-my-func(): int {
  // 🌟
  my-func of {
  - x = 10
  - y = 20
  }
}
```

A lot of primitive functions (from LLVM) are also available. Please see [Primitives](./primitives.md) for more.

### Defining ADTs

You can use the statement `data` to define ADTs:

```neut
// 🌟
data my-nat {
- My-Zero
- My-Succ(my-nat)
}
// Haskell Equivalent:
//   data my-nat
//     = My-Zero
//     | My-Succ my-nat


//------------

// 🌟
data my-list(a) {
- My-Nil
- My-Cons(a, my-list(a))
}
// Haskell Equivalent:
//   data my-list a
//     = My-Nil
//     | My-Cons a (my-list a)
```

Arguments in constructors can optionally have explicit names:

(fixme: make `config` simpler)

```neut
data config {
  // 🌟 ("count", "path", "status")
- Config(count: int, path: &text, status: my-status)
}
```

The syntax sugar `of` can be used to rewrite the above definition of `config` into:

```neut
data config {
  // 🌟
- Config of {
  - count: int
  - path: &text
  - status: my-status
  }
}
```

### Creating ADT Values

You can use constructors as usual functions:

```neut
define make-my-list(): my-list(int) {
  // 🌟
  My-Cons(1, My-Cons(2, My-Nil))
}

define make-config(path: &text, status: my-status): term {
  // 🌟
  Config of {
  - count = 10
  - path = path
  - status = status
  }
}
```

We've just used the constructor `Var` here.

### Using ADT values

You can use `match` to deconstruct ADT values:

```neut
define sum(xs: my-list(int)): int {
  // 🌟
  match xs {
  - My-Nil =>
    0
  - My-Cons(y, ys) =>
    add-int(y, sum(ys))
  }
}
```

Nested matching is also available:

```neut
define foo(xs: my-list(int)): int {
  match xs {
  - My-Nil =>
    0
    // 🌟
  - My-Cons(y, My-Cons(z, My-Nil)) =>
    1
  - My-Cons(_, _) =>
    2
  }
}
```

The core library defines `bool` with the following definition:

```neut
// 🌟
data bool {
- False
- True
}
```

A special syntax `if` is there to use this `bool` as in other languages:

```neut
define factorial(n: int) {
  // 🌟
  if le-int(n, 0) { // `le-int(n, 0)` means `n <= 0`
    1
  } else {
    mul-int(n, sub-int(n, 1)) //  n * (n - 1)
  }
}
```

The result of `if` can be bound to a variable:

```neut
define yo(cond: bool) {
  let x =
    // 🌟
    if cond {
      1
    } else {
      2
    }
  in
  print-int(x)
}

```

### `admit`

You can use `admit` to postpone implementing a function and just satisfy the type checker:

```neut
define my-complex-function(x: int, y: bool): int {
  // 🌟
  admit
}
```

### Assertion

You can use `assert` as follows:

```neut
// factorial
define fact(n: int): int {
  // 🌟
  assert "n must be non-negative" {
    ge-int(n, 0)
  };
  if eq-int(n, 0) {
    1
  } else {
    let next = sub-int(n, 1) in
    mul-int(n, fact(next))
  }
}
```

The type of `assert ".." { .. }` is `Unit`.

`assert` checks if given condition is satisfied. If the condition is True, it does nothing. Otherwise, it reports that the assertion has failed, and kills the program with exit code `1`.

If you pass `--mode release` to `neut build`, `assert` does nothing.

### Parallel Computation

You can use `detach` and `attach` to perform parallel computation:

```neut
define foo(): unit {
  let flow-1: flow(unit) =
    // 🌟
    detach {
      let value = some-heavy-computation() in
      print(value)
    }
  in
  let flow-2: flow(unit) =
    // 🌟
    detach {
      let value = other-heavy-computation() in
      print(value)
    }
  in
                 // 🌟
  let result-1 = attach { flow-1 } in
                 // 🌟
  let result-2 = attach { flow-2 } in
  Unit
}
```

`detach` receives a term of type `t` and turns it into a term of type `flow(t)`. Internally, `detach` creates a new thread and computes the term in that thread.

`attach` receives a term of type `flow(t)` and turns it into a term of type `t`. Internally, `attach` waits given computational flow (thread) to finish and extracts its result.

### Auxiliary Syntaxes

- Additional syntax sugars are also available. For more, please see the [language reference](./terms.md#use-e-x-in-cont).
- If you want to call foreign functions (FFI), please see the [here](statements.md#foreign).

## What You've Learned Here

- `let` can be used to define variables.
- Defined variables can be used by specifying their names
- Functions can be defined using `function` or `define`
- Functions can be called by writing `f(e1, ..., en)`
- The statement `data` can be used to define ADTs.
- Constructors of ADTs are normal functions.
- ADT values can destructed using `match`
- `detach` and `attach` can be used to perform parallel computation
- Additional syntax sugars are documented in the [language reference](./terms.md)
