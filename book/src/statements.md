# Statements

## Table of Contents

- [import](#import)
- [define](#define)
- [inline](#inline)
- [constant](#constant)
- [data](#data)
- [resource](#resource)
- [nominal](#nominal)
- [foreign](#foreign)

## `import`

`import` imports names from other files. It should look like the below:

```neut
import {
- this.foo
- this.item.bar {some-func, other-func}
- sample.buz
- Qux
- ZZ
}
```

### Normal Entry

A normal entry in `import` is something like `this.item.bar {some-func, other-func}` or `sample.buz`.

A normal entry in `import` starts from the alias of the module (`this`, `sample`). The alias of the module is specified in `dependency` in `module.ens`. If the file that we want to import is inside the current module, we'll write `this`.

The remaining part of the normal entry is the relative path from the source directory. For example, if we want to import `(source-dir)/item/bar`, we'll have to write `item.bar` after the alias of the module.

A normal entry can be constructed by connecting the alias part and the path part by `.`. In the case of `this.item.bar`, the alias part is `this`, and the path part is `item.bar`.

You can specify names in `{}` after each locator. The names specified here can be used without qualifiers:

```neut
import {
- this.item.bar {some-func}
}

define yo(): unit {
  some-func(arg-1, arg-2)
}
```

Unlisted names must be qualified:

```neut
import {
- this.item.bar
}

define yo(): unit {
  this.item.bar.some-func(arg-1, arg-2)
}
```

### Prefix Entry

A prefix entry in `import` is something like `Qux` or `ZZ`. That is, a capitalized name that doesn't contain any `.`.

A prefix entry in `import` must be defined in the `prefix` of the current module's `module.ens`. Suppose that `module.ens` contains the following:

```ens
{
  // ..
  prefix {
    Qux "this.item.bar"
  }
  // ..
}
```

Then, the code

```neut
import {
- this.item.bar
}

define use-some-func(): unit {
  this.item.bar.some-func()
}
```

can be rewritten into:

```neut
import {
- Qux
}

define use-some-func(): unit {
  Qux.some-func()
}
```

You may also want to see the explanation on `prefix` in [Modules](./modules.md).

## `define`

`define` defines a function. It should look like the below:

```neut
define foo(x: int, y: int): int {
  add-int(x, y)
}

define identity-1(a: tau, x: a): a {
  x
}

// a function with an implicit argument
define identity-2<a>(x: a): a {
  x
}
```

Defined functions can then be used:

```neut
define use-foo(): int {
  foo(1, 2)
}
```

`define` can optionally have implicit arguments, as in `identity-2` in the above example. These implicit arguments are inserted by the compiler at compile time, so you don't have to write them explicitly:

```neut
define use-func-with-implicit-arg(): int {
  let x = 10 in
  let y = identity-1(int, x) in // ← explicit version
  let z = identity-2(x) in      // ← implicit version
  z
}
```

A function with the same name can't be defined in the same file.

All the tail-recursions in Neut are optimized into loops (thanks to geniuses in the LLVM team).

Note that Neut's statements are order-sensitive. Thus, the following code results in an error:

```neut
define bar(): int {
  foo() // `foo` is undefined here
}

define foo(): int {
  10
}
```

You have to explicitly use the stamement `nominal` for forward references.

## `inline`

`inline` defines an inline function. It should look like the below:

```neut
inline foo(x: int, y: int): int {
  print("foo");
  add-int(x, y)
}
```

`inline` is the same as `define` except that the definition is always expanded at compile-time. For example, if you write

```neut
define use-inline-foo(): int {
  let val =
    foo(10, 20)
  in
  val
}
```

the compiler will translate the above code into the below:

```neut
define use-inline-foo(): int {
  let val =
    let tmp1 = 10 in
    let tmp2 = 20 in
    print("foo");
    add-int(tmp1, tmp2)
  in
  val
}
```

## `constant`

`constant` defines a constant. It should look like the below:

```neut
constant some-number: int {
  123
}
```

The compiler tries to reduce the body of a constant at compile-time, and reports an error if it can't reduce it into a value. For example, the following should raise an error:

```neut
constant some-number: int {
  print("hello");
  123
}
```

since `print("hello"); 123` isn't a value.

Constants can be used just like ordinary variables:

```neut
// define a constant
constant some-number: int {
  123
}

define use-constant(): int {
  // ... and use it
  print-int(some-number);
  456
}
```

## `data`

`data` defines an algebraic data type (ADT). It should look like the below:

```neut
data nat {
- Zero
- Succ(nat)
}

data list(a) {
- Nil
- Cons(a, list(a))
}

data config {
- Config of {
  - count: int
  - foo-path: &text
  - colorize: bool
  }
  // the above is equivalent to:
  //   Config(count: int, foo-path: &text, colorize: bool)
}
```

ADTs can be used with `match` or `case`:

```neut
define length<a>(xs: list(a)): int {
  // destruct ADT values using `match`
  match xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    add-int(1, length(ys))
  }
}

define length-noetic<a>(xs: &list(a)): int {
  // read noetic ADT values using `case`
  case xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    add-int(1, length-noetic(ys))
  }
}

define use-config(c: config) {
  // pattern-matching in `let` is also possible
  let Config of {count, some-path} = c in
  print(count)
}
```

## `resource`

`resource` defines a new type by specifying how to discard/copy the values of the type. It should look like the below:

```neut
resource my-new-type {
- function (value: int) {
    // .. discard the value ..
  }
- function (value: int) {
    // .. create a new clone of the value and return it as int ..
  }
}
```

`resource` takes two terms. The first term ("discarder") receives a value of the type, and must specify how to discard the value. The second term ("copier") receives a value of the type, and must return the clone of the value.

The type of a discarder is `int -> int`. The type of the argument is `int`, and thus you'll have to cast it as necessary. The return value in this term dosen't have any particular sense. After discarding the argument, you can just return 0. You might want to call functions like `free` in this term.

The type of a copier is `int -> int`. The type of the argument is `int`, and thus you'll have to cast it as necessary. The return value in this term is the new clone of the argument, casted to `int`. You might want to call functions like `malloc` in this term.

For example, the following is a definition of a "boxed" integer type, with some noisy messages:

```neut
resource boxed-int {
- function (v: int) {
    print("discarded!\n");
    free(v);
    0
  }
- function (v: int) {
    let orig-value = load-int(v) in
    let new-ptr = malloc(1) in
    magic store(int, orig-value, new-ptr);
    new-ptr
  }
}

// provide a way to introduce new boxed integer
define create-new-boxed-int(x: int): boxed-int {
  let new-ptr = malloc(8) in
  store-int(x, new-ptr);
  magic cast(int, boxed-int, new-ptr)
}
```

A value of type `boxed-int` prints `"discarded!\n"` when the value is discarded.

`resource` can be used to define low-level types like arrays.

You can find an example usage of `resource` in the `int8-array.nt` in the [core library](https://github.com/vekatze/neut-core/blob/main/source/int8-array.nt).

## `nominal`

`nominal` declares functions for forward references. It should look like the below:

```neut
nominal {
- is-odd(x: int): int
}
```

An entry of `nominal` is the same form as found in `define`. Nominal definitions can be used to achieve mutual recursions:

```neut
nominal {
- is-odd(x: int): int // nominal definition of `is-odd`
}

// given a non-negative integer `x`, returns true if `x` is even.
define is-even(x: int): bool {
  if eq-int(x, 0) {
    True
  } else {
    is-odd(sub-int(x, 1)) // ← using nominal definition
  }
}

// given a non-negative integer `x`, returns true if `x` is odd.
// ("real" definition of `is-odd`)
define is-odd(x: int): bool {
  if eq-int(x, 0) {
    False
  } else {
    is-even(sub-int(x, 1))
  }
}
```

If a nominal definition isn't followed by a real definition, the compiler reports an error.

## `foreign`

`foreign` declares functions that is defined in linked objects. It should look like the below:

```neut
foreign {
- add_const(int): int
}
```

Foreign functions declared here can be called by using the term `magic external(..)`.

Suppose that you have a C source file with the following definition:

```c
// add_const.c

int add_const(int value) {
  return value + 100;
}
```

You compile this file with `clang -c` to produce an object file, and put it to [a foreign directory of your module](modules.md#foreign). Under this setting, the following code can utilize `add_const`:

```neut
foreign {
- add_const(int): int
}

define main(): unit {
  let x: int = 10 in
  print-int(magic external add_const(x)); // ← `magic external` is used here
  print("\n")
}
```

An example project that uses `foreign` can be found [here](https://github.com/vekatze/neut/tree/main/test/misc/foreign).

You can also use LLVM intrinsics. For example, the LLVM langref states that `llvm.sin.*` intrinsic is [available](https://llvm.org/docs/LangRef.html#llvm-sin-intrinsic):

```llvm
declare float     @llvm.sin.f32(float  %Val)
declare double    @llvm.sin.f64(double %Val)
declare x86_fp80  @llvm.sin.f80(x86_fp80  %Val)
declare fp128     @llvm.sin.f128(fp128 %Val)
declare ppc_fp128 @llvm.sin.ppcf128(ppc_fp128  %Val)
```

Thus the next is a valid use of `foreign`:

```neut
foreign {
- llvm.sin.f64(float): float
}

define sin(x: float): float {
  magic external llvm.sin.f64(x)
}
```

Syscall wrapper functions and library functions are also available:

```neut
foreign {
- write(int, pointer, int): int // write(2)
- exit(int): void // exit(3)
- pthread_exit(pointer): void // pthread_exit(3)
}
```

In foreign entries, you can use `int`, `int1`, ..., `int64` `float`, `float16`, `float32`, `float64`, `void`, `pointer` as types.

When declairing the interface of a variadic function, we'll only declare the non-variadic part:

```neut
foreign {
- printf(pointer): void
}
```

... and then specify the types of variadic arguments when using `magic external`:

```neut
define print(t: &text): unit {
  // ..
  magic external printf(fmt)(len: int, val: pointer)
  //                         ^^^^^^^^^^^^^^^^^^^^^^
  //                         passing variadic arguments with types
}
```
