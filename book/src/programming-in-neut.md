# Programming in Neut

Now that we know how to use modules, let's write programs in Neut.

## Table of Contents

- [Variables](#variables)
- [Functions](#functions)
- [Algebraic Data Types](#algebraic-data-types)
- [Parallel Computation](#parallel-computation)
- [Miscs](#miscs)

## Variables

You can use `let` to define variables:

```neut
define hey(): unit {
  let x = "hello" in
  let y: int = 100 in
  let z: float = 3.8 in
  print("hey")
}
```

The compiler reports unused variables (`x`, `y`, and `z` in the example above). You can use the name `_` to suppress those warnings:

```neut
define hey(): unit {
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
    let y: int = 100 in
    let z: float = 3.8 in
    "hello"
  in
  print(x) // => hello
}
```

You can use `e1; e2` as a syntax sugar of `let _: unit = e1 in e2`:

```neut
define hey(): unit {
  print("a");
  print("b")
}

// ↓ (desugar)

define hey(): unit {
  let _ = print("a") in
  print("b")
}
```

## Functions

### Defining Functions at the Top Level

You can use the statement `define` to define functions:

```neut
// defining an ordinary function
define my-func1(x1: int, x2: bool): bool {
  x2
}

// defining a recursive function
define my-func2(cond: bool): int {
  if cond {
    1
  } else {
    my-func2(not(cond)) // `my-func2` is available here
  }
}

// a function that returns a function
define my-func3(): (int, bool) -> bool {
  my-func1
}
```

`define` can also define a function with implicit arguments (or "generics"):

```neut
// The `a` in the angle bracket is the implicit argument of `id`
define id<a>(x: a): a {
  x
}

define use-id(): int {
  let str = 10 in
  id(str) // calling `id` without specifying `a` explicitly
}
```

Incidentally, you can explicitly write the type of `a`:

```neut
define id<a: type>(x: a): a { // `type` is the type of types
  x
}
```

We can define `id` without using any implicit arguments as follows (just for comparison):

```neut
define id(a: type, x: a): a {
  x
}

// using `id`
define use-id(): int {
  let str = 10 in
  id(int, str) // ← the first argument `int` is now made explicit
}
```

### Defining Functions in a Body of a Function

You can use `function` to define an anonymous function:

```neut
define foo() {
  let f =
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

You can also use `define` in the body of a function to define a recursive function:

```neut
define foo() {
  let f =
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

<div class="info-block">

The compiler reports an error if you rewrite the example above so that it uses the variable `f` more than once. This behavior is to avoid unexpected copying of values. You can satisfy the compiler by renaming `f` into `!f`. The next section will cover this topic.

</div>

### Calling Functions

You can call a function `f` with arguments `e1`, ..., `en` by writing `f(e1, ..., en)`:

```neut
define my-func(x: int, y: int): int {
  add-int(x, y)
}

define use-my-func(): int {
  my-func(10, 20)
}
```

The syntax sugar `of` can be used to rewrite the above `use-my-func` as follows:

```neut
define use-my-func(): int {
  my-func of {
    x = 10,
    y = 20,
  }
}
```

A lot of primitive functions (from LLVM) are also available. Please see [Primitives](./primitives.md) for more.

## Algebraic Data Types

You can use the statement `data` to define ADTs:

```neut
data my-nat {
| My-Zero
| My-Succ(my-nat)
}
// In Haskell:
//   data my-nat
//     = My-Zero
//     | My-Succ my-nat


//------------

data my-list(a) {
| My-Nil
| My-Cons(a, my-list(a))
}
// In Haskell:
//   data my-list a
//     = My-Nil
//     | My-Cons a (my-list a)
```

Arguments in constructors can optionally have explicit names:

```neut
data config {
| Config(count: int, cond: bool)
}
```

You might want to write this vertically using a trailing comma:

```neut
data config {
| Config(
    count: int,
    cond: bool,
  )
}
```

### Creating ADT Values

You can use constructors just like normal functions to create ADT values:

```neut
define make-my-list(): my-list(int) {
  My-Cons(1, My-Cons(2, My-Nil))
}

define make-config(): config {
  Config of {
    count = 10,
    cond = True,
  }
}
```

### Using ADT values

You can use `match` to destructure ADT values:

```neut
define sum(xs: my-list(int)): int {
  match xs {
  | My-Nil =>
    0
  | My-Cons(y, ys) =>
    add-int(y, sum(ys))
  }
}
```

Nested matching is also possible:

```neut
define foo(xs: my-list(int)): int {
  match xs {
  | My-Nil =>
    0
  | My-Cons(y, My-Cons(z, My-Nil)) =>
    1
  | My-Cons(_, _) =>
    2
  }
}
```

The result of `match` can be bound to a variable:

```neut
define yo(xs: my-list(int)): int {
  let val =
    match xs {
    | My-Nil =>
      0
    | My-Cons(_, _) =>
      1
    }
  in
  val
}
```

## Parallel Computation

You can use `detach` and `attach` to perform parallel computation:

```neut
define foo(): unit {
  let t1: thread(unit) =
    // creates a thread
    detach {
      let value = some-heavy-computation() in
      print(value)
    }
  in
  let t2: thread(unit) =
    // creates a thread
    detach {
      let value = other-heavy-computation() in
      print(value)
    }
  in
  // wait
  let result-1 = attach { t1 } in
  // wait
  let result-2 = attach { t2 } in
  Unit
}
```

`detach` receives a term of type `t` and returns a term of type `thread(t)`. Internally, `detach` creates a new thread and starts computing the term in that thread.

`attach` receives a term of type `thread(t)` and returns a term of type `t`. Internally, `attach` waits for a given thread to finish and extracts its result.

## Miscs

### `nominal`

You can use `nominal` for forward references:

```neut
nominal {
  is-odd(x: int): int, // ← declaration of `is-odd`
}

define is-even(x: int): bool {
  if eq-int(x, 0) {
    True
  } else {
    is-odd(sub-int(x, 1)) // ← using the nominal definition of `is-odd`
  }
}

// ↓ the real definition of `is-odd`
define is-odd(x: int): bool {
  if eq-int(x, 0) {
    False
  } else {
    is-even(sub-int(x, 1))
  }
}
```

### `if`

The core library defines `bool` as follows:

```neut
data bool {
| False
| True
}
```

You can use `if` when you use this `bool`:

```neut
define yo(cond: bool) {
  if cond {
    print("yo!")
  } else {
    print("yo")
  }
}

// ↓ desugar

define yo(cond: bool) {
  match cond {
  | True =>
    print("yo!")
  | False =>
    print("yo")
  }
}
```

### `admit`

You can use `admit` to postpone implementing a function and satisfy the type checker:

```neut
define my-complex-function(x: int, y: bool): int {
  admit
}
```

### `assert`

You can use `assert` as follows:

```neut
define fact(n: int): int {
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

The type of `assert ".." { .. }` is `unit`.

`assert` checks if a given condition is satisfied. If the condition is `True`, it does nothing. Otherwise, it reports that the assertion has failed and kills the program with exit code `1`.

If you pass `--mode release` to `neut build`, `assert` does nothing.

### Notes

- Additional syntactic sugars are also available. For more, please see the [language reference](./terms.md#syntactic-sugar).
- If you want to call foreign functions (FFI), please see the [here](statements.md#foreign).
