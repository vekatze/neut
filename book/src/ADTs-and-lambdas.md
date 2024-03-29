# ADTs and Lambdas

In this section, we'll see how to use basic values of Neut: algebraic data types (ADTs) and lambdas.

## Algebraic Data Types

### Basics

ADTs can be defined like below:

```neut
// Defining an algebraic data type
data my-list(a) {
- MyNil
- MyCons(a, my-list(a))
}

// nullary data types are also allowed
data test {
- Foo
- Bar
- Buz() // explicit nullary data types
}
```

They can be used like the below:

```neut
define create-my-list(): my-list(int) {
  // using constructors
  MyCons(1, MyCons(2, MyNil))
}

define get-length(a: tau, xs: my-list(a)): int {
  // pattern matching against a my-list
  match xs {
  - MyNil =>
    0
  - MyCons(1, MyNil) => // nested pattern matching is available
    1
  - MyCons(_, rest) =>
    add-int(1, get-length(a, rest))
  }
}
```

The name of a constructor must start with an uppercase letter.

### Memory Behavior

When a constructor is called, a memory region for the constructor is allocated. The internal representation of things like `MyCons(1, Nil)` is:

```neut
(pointer-to-a, discriminant, 1, pointer-to-Nil) // 4-word tuple
```

where the `discriminant` is an integer that is used to distinguish constructors; In this case, the actual value for `MyCons` will be 1. That of `MyNil` will be 0.

When `match` is used against a value of an ADT, the inner values of the given value are extracted, and the unnecessary data is freed. For example, if `MyCons(1, Nil)` is supplied to a `match`, the following will happen:

1. the `1` and `pointer-to-Nil` are bound to some variables,
2. the `pointer-to-a` is discarded along its type `tau`,
3. the outer 4-word tuple is freed, and
4. the body of the clause is executed.

---

If an ADT and all its constructor don't have any arguments, the internal representation of the type is optimized into an enum. For example, consider the following code:

```neut
data color {
- Red
- Blue
- Green
}
```

The internal representation of `Red` in this case is optimized into `0`. That of `Blue` is optimized into `1`, and so on.

---

Also, if an ADT has only one constructor and the constructor has only one argument (i.e. the ADT is like a `newtype` in Haskell), its internal representation is automatically optimized into its content. For example, consider the following code:

```neut
data semigroup(a) {
- Semigroup((a, a) -> a)
}
```

Since the type `semigroup` has only one constructor (`Semigroup`) and the constructor has only one argument (`(a, a) -> a`), the internal representation of `Semigroup(e)` is optimized into that of `e`.

## Lambdas

### Basics

You can create a lambda abstraction (anonymous function) and use it as an ordinary function:

```neut
define sample(): int {
  let inc = (x) => { add-int(x) } in // create a lambda function int -> int
  inc(10) // and call it
}
```

Also, the type of functions is written as follows in Neut:

```neut
define sample(): int {
  let type1 = int -> int in         // receives int          / returns int
  let type2 = (int, bool) -> int in // receives int and bool / returns int
  let type3 = () -> int in          // receives nothing      / returns int
  0
}
```

### Memory Behavior

A `lambda` is compiled into a three-word tuple:

```neut
(type-of-free-variables, (freevar-1, ..., freevar-n), pointer-to-closed-function)
```

When you call a lambda, things like below will happen:

```neut
func(a, b, c)

// ↓ (source code to LLVM)

let free-variables = func[1] in
let closed-function = func[2] in
free(func); // free the outer tuple
CALL closed-function(a, b, c, free-variables) // LLVM-level function call
```

## Local Recursion

### Basics

As we've seen, a recursive function can be defined by using `define`. You can also use `mu` to define a "local" recursive function:

```neut
define foo(x: int): int {
  let some-variable = 1 in
  let some-rec-func =
    // creating a local recursive function `my-rec-func`
    mu my-rec-func(y: int): int {
      if eq-int(y, 0) {
        some-variable // free variables can be used normally in `mu`
      } else {
        print("hey\n");
        my-rec-func(sub-int(y, 1)) // recursive call
      }
    }
  in
  some-rec-func(x)
}
```

### Behavior

An anonymous recursive function is translated into a top-level recursive function (i.e. ordinary lambda lifting). For example, the example above is translated into something like below:

```neut
define my-rec-func(y: int, z: int) {
  if le-int(y, 0) {
    z
  } else {
    print("hey\n");
    my-rec-func(sub-int(y, 1), z)
  }
}

define fact(x: int): int {
  let some-variable = 1 in
  let some-rec-func = (y: int) => { my-rec-func(y, some-variable) } in
  some-rec-func(x)
}
```

In practice, one may think of `mu` as a nested `define`.

## Some Useful Notations

### Holes and Implicit Arguments

A hole in Neut is written as `_`. The content of a hole is inferred by the type system at compile time. This can be useful when we, for example, call a polymorphic function. Suppose we have the following `for-each` function:

```neut
define for-each(a: tau, b: tau, xs: list(a), f: a -> b): list(b) {
  match xs {
  - [] =>
    []
  - y :: ys =>
    f(y) :: for-each(a, b, ys, f)
  }
}
```

If it were not for holes, we'd have to call this function as follows:

```neut
let bool-list = for-each(int, bool, int-list, int-to-bool-func) in
cont
```

Using holes, the function can be called without specifying its type arguments explicitly:

```neut
let bool-list = for-each(_, _, int-list, int-to-bool-func) in
cont
```

### Supplying Holes

When defining a function, you can specify some of its arguments to be implicit:

```neut
// `a: tau` and `b: tau` are implicit
define for-each[a: tau, b: tau](xs: list(a), f: a -> b): list(b) {
  ..
}
```

Or even shorter:

```neut
define for-each[a, b](xs: list(a), f: a -> b): list(b) {
  ..
}
```

You can call this `for-each` as if it were a binary function:

```neut
let bool-list = for-each(int-list, int-to-bool-func) in
cont

// ↓ (automatically translated to)

let bool-list = for-each(_, _, int-list, int-to-bool-func) in
cont
```

This can be useful when achieving better readability (YMMV):

```neut
// with implicit arguments
for-each(xss, (xs) => {
  for-each(xs, (x) => {
    int-to-bool(x)
  })
})

// without implicit arguments
for-each(list(int), list(bool), xss, (xs) => {
  for-each(int, bool, xs, (x) => {
    int-to-bool(x)
  })
})
```

You can also write `@foo` to pass implicit arguments explicitly (as in Coq and Lean):

```neut
let bool-list = @for-each(int, bool, int-list, int-to-bool-func) in
cont
```

### Keyword Arguments

The arguments of a function can be supplied using their names:

```neut
define some-function(a: int, some-argument: tau, b: tau): int {
  // ...
}

define caller(): int {
  let _ =
    // calling a function using the name of arguments
    some-function of {
    - b => tau
    - a => 20
    - some-argument => tau
    }
  in
  0
}

```

This `of`-notation can also be used with a constructor:

```neut
data config {
- Config(some-value: my-list(int), foo: int, bar: my-list(int))
}

define create-struct(): config(my-list(int)) {
  // create a struct using keyword arguments
  Config of {
  - foo => 30
  - bar => MyNil
  - some-value => MyCons(3, MyNil)
  }
}
```

or when defining an ADT:

```neut
data config {
- Config of {
  - some-value: my-list(int)
  - foo: int
  - bar: my-list
  }
}
```

This notation can also be used in pattern matchings:

```neut
define use-config(cfg: config): unit {
  let Config of { foo => a, bar => b, some-value => c } = config in
  do-something(a, b, c)
}
```

Or more shortly:

```neut
define use-config(cfg: config): unit {
  let Config of { foo, bar, buz } = config in
  do-something(foo, bar, buz)
}

// The following two are equivalent:
//   let Config of { foo,        bar,        buz        } = config in cont
//   let Config of { foo => foo, bar => bar, buz => buz } = config in cont
```

You can also omit unused fields:

```neut
define use-config(cfg: config): unit {
  let Config of { foo, bar } = config in // `buz` is omitted
  do-something(foo, bar)
}

// The following two are equivalent:
//   let Config of { foo,        bar                  } = config in cont
//   let Config of { foo => foo, bar => bar, buz => _ } = config in cont
```

### A Notation for Single-Branch Pattern Matching

You can rewrite a single-branch `match` into `let` as follows:

```neut
// an example ADT
data item {
- Item of {
  - foo: int
  - bar: bool
  }
}

// before
define some-func(x: item): int {
  match x {
  - Item(foo, bar) =>
    foo
  }
}

// after
define some-func(x: item): int {
  let Item(foo, bar) = x in
  foo
}
```

## Other Basic Types

Basic types (integers, floats, bools, etc.) are also available in Neut, of course. This will be covered in [the last section of this chapter](./other-built-in-utilities.md).

## How Can I Say Hello to The World?

The above should cover the basics of Neut. It won't suffice, however; We can't even do the beloved Hello World now. This is because the type of static text is a noetic type, which is covered in the next section. Let's go ahead.
