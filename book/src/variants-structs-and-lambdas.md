# Variants, Structs, and Lambdas

In this section, we'll see how to use the key colors of Neut: variants, structs, and lambdas.

Examples in this section can be downloaded as follows:

```sh
git clone <insert-link-here>
```

## Variants

### Basics

Let's see how to write actual code. Firstly, you can define a variant type like below:

```neut
// Defining a variant type (or an algebraic data type)
variant my-list(a) {
- MyNil
- MyCons(a, my-list(a))
}

// nullary variant types are also allowed
variant test {
- Foo
- Bar
}
```

and use it like the below:

```neut
define create-my-list(): my-list(i64) {
  MyCons(1, MyCons(2, MyNil))
}

define get-length[a](xs: my-list(a)): i64 {
  // pattern matching against a my-list
  match xs {
  - MyNil =>
    0
  - MyCons(1, MyNil) => // nested pattern matching is available
    1
  - MyCons(_, rest) =>
    add-i64(1, get-length(rest))
  }
}
```

The name of a constructor must start with an uppercase letter.

Incidentally, the `[a]` at the definition of `get-length` specifies implicit arguments. Without that, `get-length` will become:

```neut
// `tau` is the type of types.
define get-length(a: tau, xs: my-list(a)): i64 {
  match xs { // pattern matching against a my-list
  - MyNil =>
    1
  - MyCons(_, rest) =>
    add-i64(1, get-length(a, rest)) // recursion
  }
}
```

which might not be what you want, because you need to pass the type `a` every time you call this function.

You can specify multiple implicit arguments by writing, for example, `[a, b, c, d]`.

### Memory Behavior

When calling a constructor, memory for the constructor is allocated. The internal representation of things like `MyCons(1, Nil)` is:

```neut
(pointer-to-a, discriminant, 1, pointer-to-Nil) // 4-word tuple
```

where the `discriminant` is an integer that is used to distinguish constructors; In this case, the actual value for `MyCons` will be 1. That of `MyNil` will be 0.

When `match` is used against a value of variant type, the inner values of the given value are extracted, and the unnecessary data is freed. For example, if the given value is `MyCons(1, Nil)`, the following will happen:

1. the `1` and `pointer-to-Nil` are extracted to be used later,
2. the `pointer-to-a` is discarded along its type `tau`, and
3. the outer 4-word tuple is freed.

---

Also, if the variant type and all its constructor don't need any arguments, the internal representation of the variant type is optimized into an enum. For example, consider the following code:

```neut
variant color {
- Red
- Blue
- Green
}
```

Then, the internal representation of `Red` is optimized into `0`. That of `Blue` is optimized into `1`, and so on.

## Structs

### Basics

You can define a struct type like the below:

```neut
// Define a struct.
struct config(a) by Config { // `by Config` specifies the name of the constructor
- foo: i64
- bar: a
- some-value: my-list(i64)
}
```

and use the type as follows:

```neut
define create-struct(): config(my-list(i64)) {
  // create a struct
  new config {
  - foo <= 30
  - bar <= MyNil
  - some-value <= MyCons(3, MyNil)
  }
  // you can also write `Config(30, MyNil, MyCons(3, MyNil))`
}

define use-struct(c: config(a)): i64 {
  let Config(x, y, z) = c // destructive bind
  0
}
```

It might be illuminating to see that a struct is essentially just a syntax sugar for a variant type:

```neut
struct config(a) by Config {
- foo: i64
- bar: a
- some-value: my-list(i64)
}

↓

variant config(a) {
- Config(foo: i64, bar: a, some-value: my-list(i64))
}
```

<!-- When defining a struct, projections for struct fields are also defined; This topic is covered in the next section. -->

### Memory Behavior

The same as the corresponding variant type.

## Lambdas

### Basics

You can create an anonymous function by using `lambda`, and use it as an ordinary function:

```neut
define sample(): i64 {
  let inc = lambda (x) { add-i64(x) } // create a lambda function i64 -> i64
  inc(10) // and call it
}
```

Also, the type of functions is written as follows in Neut:

```neut
define sample(): i64 {
  let type1 = i64 -> i64         // receives i64          / returns i64
  let type2 = (i64, bool) -> i64 // receives i64 and bool / returns i64
  let type3 = () -> i64          // receives nothing      / returns i64
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
cls(a, b, c)

// ↓

// let type-of-free-variables = cls[0] (unused)
let free-variables = cls[1]
let closed-function = cls[2]
free(cls) // free the outer tuple
closed-function(a, b, c, free-variables)
```

## Other Basic Types

Basic types (integers, floats, bools, etc.) are also available in Neut, of course. This will be covered in [the last section of this chapter](./other-built-in-utilities.md).

## How Can I Say Hello to The World?

The above should cover the basics of Neut. Still, it won't suffice; We can't even do the beloved Hello World now. This is because the type of static text is a noetic type, which is covered in the next section. Let's go ahead.
