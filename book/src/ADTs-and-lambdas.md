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

If an ADT and all its constructor don't need any arguments, the internal representation of the type is optimized into an enum. For example, consider the following code:

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

### Wildcard in Pattern Arguments

You can use `..` to specify names automatically:

```neut
// an example ADT
data item {
- Item of {
  - foo: int
  - bar: bool
  }
}

define some-func(x: item): int {
  let Item(..) = x in // ".." is expanded into "foo, bar"
  foo
}
```

### Nested Struct and Automatic Pattern Matching

You may want to use `data` to structure your code as follows:

```neut
data semigroup(a) {
- Semigroup of {
  - append: (a, a) -> a
  }
}

data monoid(a) {
- Monoid of {
  - empty: a
  - as-semigroup: semigroup(a)
  }
}

inline some-func[a](m: monoid(a)): a {
  let Monoid(..) = m in
  let Semigroup(..) = as-semigroup in
  let _ = append in // from Semigroup
  append(empty, empty)
}
```

Note that the code above uses `inline` so that structs are reduced at compile-time.

You may find it a bit tedious to write both of `Monoid(..)` and `Semigroup(..)`. This can be relieved by using following `via`-notation:

```neut
// the same
data semigroup(a) {
- Semigroup of {
  - append: (a, a) -> a
  }
}

// modified
data monoid(a) {
- Monoid of {
  - empty: a
  - as-semigroup: semigroup(a) via Semigroup // added "via Semigroup"
  }
}

// modified
inline some-func[a](m: monoid(a)): a {
  let Monoid(..) = m in
  let _ = append in // from Semigroup
  append(empty, empty)
  // (`let Semigroup(..) = as-semigroup` is inserted automatically)
}
```

More generally, if `via CONSTRUCTOR` is specified when defining a constructor, the corresponding

```neut
let CONSTRUCTOR(..) = constructor in
```

is automatically inserted every time `..` is supplied to the parent constructor (here, the `Monoid`).

## Other Basic Types

Basic types (integers, floats, bools, etc.) are also available in Neut, of course. This will be covered in [the last section of this chapter](./other-built-in-utilities.md).

## How Can I Say Hello to The World?

The above should cover the basics of Neut. It won't suffice, however; We can't even do the beloved Hello World now. This is because the type of static text is a noetic type, which is covered in the next section. Let's go ahead.
