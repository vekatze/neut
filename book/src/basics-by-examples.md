# Basics by Examples

In this section, you'll see how to create, build, and run a project. Then how to use variant types, struct types, lambdas, and primitive types.

Examples in this section can be downloaded as follows:

```sh
git clone <insert-link-here>
```

## Creating a Project

Run the following command to create your first project:

```neut
neut create hello
cd hello
```

The project should contain a file `hello.nt` with the following content:

```neut
define main(): i64 {
  0
}
```

Rewrite the file to something like below:

```neut
define main(): i64 {
  print("Hi!\n")
  0
}
```

Then compile and run the project:

```sh
$ neut build --execute
# => Hi!
```


## Building a Project

At the root of your project, you should find a file `module.ens`. In the file, something like below can be found:

```neut
target = {
  hello = "hello.nt"
}
```

This defines the entry point for a target named `hello`. The command `neut build TARGET` starts compilation assuming that the entrypoint is in `"hello.nt"`. That is, the file `"hello.nt"` must define a function named `main`.

If you omit `TARGET`, `neut build` simply builds all the targets.

If you pass `--execute`, `neut build` runs the resulting executable after building it.

Incidentally, the paths of source files specified in `module.ens` is relative to `/source/`.

## Structure of a Source File

The basic structure of a source file is as follows:

```neut
// import other files
import {
- foo.bar
- this.buz
}

// export names defined in this file
export {
- func
}

// ... and here comes the main section.
// We'll define functions and types here.

define func(arg-1: i64): i64 {
  arg-1
}

// ...
```

The `import` and `export` are covered later. Here, we'll see basic programming contents, like defining functions, using recursions, etc.

Also, the order of functions doesn't matter in Neut.

## Memory in Neut

Let's first check the most prominent feature of Neut. In this language, a variable is copied/discarded according to the number of the uses of the variables.

The content of a variable is copied if the variable is used multiple times. Consider the following code:

```neut
define foo(): my-list(i64) {
  let c = MyCons(1, MyCons(2, MyNil))
  // (X)
  let c1 = c
  // (Y)
  let c2 = c
  c
}
```

In this example, since the variable `c` is used for three times, `c` is copied twice. This will happen immediately after the variable is defined. In our running example, the content of `c` is copied twice at `(X)`.

Also, the content of a variable is discarded if the variable isn't used. In the example above, since `c1` and `c2` aren't used, the contents of those variables are discarded. Again, this will happen immediately after the variable is defined. In our running example, the content of `c1` is discarded at `(Y)`.

Try to use variables as linearly as possible for better performance. When you can't avoid non-linearlity, you'll use a noema, which is explained in the next section. Before that, let's see how non-noetic fragment of the language works here.

## Using Variant Types

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

and use it like below:

```neut
define create-my-list(): my-list(i64) {
  MyCons(1, MyCons(2, MyNil))
}

define get-length[a](xs: my-list(a)): i64 {
  match xs { // pattern matching against a my-list
  - MyNil =>
    1
  - MyCons(_, rest) =>
    add-i64(1, get-length(rest)) // recursion
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

which might not be what you want, because in this case you need to specify the type `a` every time you call this function.

You can specify multiple implicit arguments by writing, for example, `[a, b, c, d]`.

### Memory Behavior

When calling a constructor, memory for the constructor is allocated. The internal representation of things like `MyCons(1, Nil)` is:

```neut
(pointer-to-a, discriminant, 1, pointer-to-Nil) // 4-word tuple
```

where the `discriminant` is an integer that is used to distinguish constructors; In this case the actual value for `MyCons` will be 1. That of `MyNil` will be 0.

When `match` is used against a value of variant type, the inner values of given value is extracted, and the unnecessary data is freed. For example, if the given value is `MyCons(1, Nil)`, the following will happen:

1. the `1` and `pointer-to-Nil` is extracted to be used later,
2. the `pointer-to-a` is discarded along its type `tau`, and
3. the outer 4-word tuple is freed.

---

Also, if the variant type and all its constructor doesn't need any arguments, the internal representation of the variant type is optimized into enum. For example, consider the following code:

```neut
variant color {
- Red
- Blue
- Green
}
```

Then, the internal representation of `Red` is optimized into `0`. That of `Blue` is optimized into `1`, and so on.

## Using a Struct

### Basics

You can define a struct type like bellow:

```neut
// Define a struct.
struct config(a) by Config { // `by Config` specifies the name of the constructor
- foo: i64
- bar: a
- some-value: my-list(i64)
}

// nullary struct type is also allowed
struct env by Env {
- value-1: i64
- ...
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

When defining a struct, projections for struct fields are also defined; This topic is covered in the next section.

### Memory Behavior

The same as the corresponding variant type.

## Creating & Using Lambdas

### Basics

You can create an annonymous function by using `lambda`, and use it as an ordinary function:

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

A `lambda` is compiled into three-word tuple:

```neut
(type-of-free-variables, (freevar-1, ..., freevar-n), pointer-to-closed-function)
```

When you call a lambda, things like below will happen:

```neut
cls(a, b, c)

// ↓

let free-variables = cls[1]
let closed-function = cls[2]
free(cls) // free the outer tuple
closed-function(a, b, c, free-variables)
```

<!-- The name of a constructor must start with an uppercase letter. -->

## Primitive Types and Operations

### Basics

Integers and floats are of course supported in Neut. For example, the factorial function will be written as follows:

```neut
define fact(x: i64): i64 {
  if eq-i64(x, 0) {
    1
  } else {
    mul-i64(x, fact(sub-i64(x, 1)))
  }
}
```

The above used `i64` for an integer type. There are other number types in Neut;:

- signed integer types: `iN (= i1, i2, i3, ..., i64)`
- unsigned integer types: `uN (= u1, u2, u3, ..., u64)`
- float types: `fN (= f16, f32, f64)`

These types have a lot of primitive operations. These inherit from LLVM IR:

|             | integers                                          | floats                                                                            |
|             | ----                                              | ----                                                                              |
| arithmetics | add, sub, mul, div, rem, or, xor, shl, lshr, ashr | neg, add, sub, mul, div, rem                                                      |
| comparison  | eq, ne, gt, ge, lt, le                            | oeq, ogt, oge, olt, ole, one, ord, ueq, ugt, uge, ult, ule, une, uno, false, true |

For example, all of `add-u32`, `neg-f64`, `eq-i64`, and `oeq-f32` are available.


For the detailed behaviors of them, please refer to [the language reference of LLVM](https://llvm.org/docs/LangRef.html). Also, as usual, please be careful when you compare floats. I can actually hear a faint voice from deep within my heart saying "I want to rename `oeq` into `I-know-what-I-am-doing-and-still-want-to-check-if-two-floats-are-ordered-and-equal` or something like that".

By the way, LLVM IR in itself doesn't distinguish signed integer types with unsigned integer types. Rather, it has operations for signed/unsigned integers. For example, LLVM has an operation `udiv` that calculates the quotient of two operands, regarding both of them as unsigned integers. Neut, on the other hand, distinguishes these two types, and provides operations for both of them. The `udiv` in LLVM is `div-uN` in Neut. The `div` in LLVM is `div-iN`. And so on.

### Memory Behavior

Primitive values are stored as it is; There is no box or something like that. The value is stored in the stack. Primitive values aren't actually copied or discarded even if they are used non-linearly.

## Core Types

The core library (something like Prelude in other languages) is imported automatically and provides basic variant types and functions. Things like below are defined in the library:

```neut
variant bottom {}

variant top {
- Unit
}

variant bool {
- False
- True
}

variant list(a) {
- Nil
- Cons(a, list(a))   // `Cons(x, xs)` can also be written as `x :< xs`
                     // `Cons(x, Cons(y, Nil))` can also be written as `[x, y]`
}

// you can write `option(a)` as `?a`
variant option(a) {
- None
- Some(a)
}

variant sum(a, b) {
- Left(a)
- Right(b)
}

struct product(a, b) by Product {
- left: a
- right: b
}
```

Basic operations for those types are also defined in the library. For more, see the library definition (FIXME: insert a link here).

## Miscs

### Inline Functions

You can define an inline functions as follows:

```neut
define-inline increment(x: i64): i64 {
  add-i64(x, 1)
}
```

Inline functions are reduced at compile time.

### Type Alias

You can define a type alias as follows:

```neut
alias my-int {
  i64
}
```

The above is essentially the same as below:

```neut
define-inline my-int(): tau {
  i64
}
```

The difference lies in the fact that you don't have to "call" the type if you use `alias`:

```neut
// when you use `alias`
define use-my-int(x: my-int) {
  ...
}

// when you use `define-inline`
define use-my-int(x: my-int()) {
  ...
}
```

which relieves code cluttering.

### Tail Call Optimization

Neut optimizes all the tail calls. Thus, calculating length can be done faster using the following way:

```neut
define length-of-my-list[a](xs: my-list(a)): i64 {
  let helper =
    // (you can also define a recursive function in a function)
    define get-length(ys: my-list(a), acc: i64): i64 {
      match ys {
      - Nil =>
        acc
      - Cons(_, zs) =>
        get-length(zs, add-i64(1, acc)) // tail call of `get-length`
      }
    }
  helper(xs, 0)
}
```

## Done! ... But How Can I Say Hello to The World?

The above should cover the basics of Neut. Still it won't suffice; We can't even do the beloved Hello world now. This is because the type of a text is a noetic type, which is covered in the next section. Let's go ahead.
