# Statements

## Table of Contents

- [import](#import)
- [define](#define)
- [inline](#inline)
- [data](#data)
- [resource](#resource)
- [nominal](#nominal)
- [foreign](#foreign)

## `import`

`import` imports names from other files. It should look like the following:

```neut
import {
  Qux,
  ZZ,
  sample.buz,
  this.foo,
  this.foo.bar {some-func, other-func},
}
```

`import` can only be at the top of a file.

Every item in `import` is something like the following:

- `this.foo`
- `this.foo.bar {some-func, other-func}`
- `sample.buz`

An import item starts from the alias of the module (`this`, `sample`). The alias of the module is specified in `dependency` in `module.ens`. If the file we want to import is inside the current module, we'll write `this`.

The remaining part of the item is the relative path from the source directory. For example, if we want to import `(source-dir)/foo/bar`, we'll have to write `foo.bar` after the alias of the module.

An import item can be constructed by concatenating the alias and the path with `.`. In the case of `this.foo.bar`, the alias part is `this`, and the path part is `foo.bar`.

You can specify names in `{}`. The names specified here can be used without qualifiers:

```neut
import {
  this.foo.bar {some-func},
}

define yo(): unit {
  some-func(arg-1, arg-2)
}
```

Unlisted names must be qualified:

```neut
import {
  this.foo.bar,
}

define yo(): unit {
  this.foo.bar.some-func(arg-1, arg-2)
}
```

You can also list static files in `import`:

```neut
import {
  static {some-file, other-file}
}
```

For more on static files, please see [the section in Modules](modules.md#static).

## `define`

`define` defines a function. It should look like the following:

```neut
define foo(x: int, y: int): int {
  add-int(x, y)
}

define identity-1(a: type, x: a): a {
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

`define` can optionally have implicit arguments, as in `identity-2` in the above example. The compiler inserts these implicit arguments at compile time, so you don't have to write them explicitly:

```neut
define use-func-with-implicit-arg(): int {
  let x = 10;
  let y = identity-1(int, x); // ← explicit version
  let z = identity-2(x);      // ← implicit version
  z
}
```

You can also use `define` without any explicit arguments:

```neut
define foo: int {
  10
}

define empty-list<a>: list(a) {
  Nil
}

define use-constants(): list(int) {
  let x = foo;
  empty-list
}
```

The above code is translated into the following during compile time:

```neut
define foo(): int {
  10
}

define empty-list<a>(): list(a) {
  Nil
}

define use-constants(): list(int) {
  let x = foo();
  empty-list()
}
```

The compiler tries to reduce the body of a `define` into a value at compile time if the `define` doesn't have any explicit arguments. The compiler reports an error if it can't get a value. For example, the following should result in an error:

```neut
define bar: int {
  print("hello");
  123
}
```

A function with the same name can't be defined in the same file.

All the tail-recursions in Neut are optimized into loops (thanks to geniuses in the LLVM team).

Note that statements are order-sensitive as in F#. Thus, the following code results in an error:

```neut
define bar(): int {
  foo() // `foo` is undefined here
}

define foo(): int {
  10
}
```

You have to use the statement `nominal` explicitly for forward references.

## `inline`

`inline` defines an inline function. It should look like the following:

```neut
inline foo(x: int, y: int): int {
  print("foo");
  add-int(x, y)
}
```

`inline` is the same as `define` except that the definition is always expanded at compile-time. For example, if you write

```neut
define use-inline-foo(): int {
  let val = foo(10, 20);
  val
}
```

The compiler will translate the above code into the following:

```neut
define use-inline-foo(): int {
  let val = {
    let x = 10;
    let y = 20;
    print("foo");
    add-int(x, y)
  };
  val
}
```

You can also use `inline` without any explicit arguments:

```neut
inline foo: int {
  10
}

inline empty-list<a>: list(a) {
  Nil
}

define use-constants(): list(int) {
  let x = foo;
  empty-list
}
```

The above code is translated into the following during compile time:

```neut
inline foo(): int {
  10
}

inline empty-list<a>(): list(a) {
  Nil
}

define use-constants(): list(int) {
  let x = foo();
  empty-list()
}
```

The compiler tries to reduce the body of an `inline` into a value at compile time if the `inline` doesn't have any explicit arguments. The compiler reports an error if it can't get a value. For example, the following should result in an error:

```neut
inline bar: int {
  print("hello");
  123
}
```

## `data`

`data` defines an algebraic data type (ADT). It should look like the following:

```neut
data nat {
| Zero
| Succ(nat)
}

data list(a) {
| Nil
| Cons(a, list(a))
}

data config {
| Config(
    count: int,
    foo-path: &text,
    colorize: bool,
  )
}
```

You can use the content of an ADT value by using `match` or `case`:

```neut
define length<a>(xs: list(a)): int {
  // destruct ADT values using `match`
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}

define length-noetic<a>(xs: &list(a)): int {
  // read noetic ADT values using `case`
  case xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length-noetic(ys))
  }
}

define use-config(c: config) {
  // pattern-matching in `let` is also possible
  let Config of {count, some-path} = c;
  print(count)
}
```

## `resource`

`resource` defines a new type by specifying how to discard/copy the values of the type. It should look like the following:

```neut
resource my-new-type {
  function (value: pointer) {
    // .. discard the value ..
  },
  function (value: pointer) {
    // .. create a new clone of the value and return it as int ..
  },
}
```

`resource` takes two terms. The first term ("discarder") receives a value of the type and discards the value. The second term ("copier") receives a value of the type and returns the clone of the value (keeping the original value intact).

The type of a discarder is `(a) -> unit` for some `a`. You might want to call functions like `free` in this term.

The type of a copier is `(int) -> int` for some `a`. This `a` must be the same as the `a` used in the discarder. You might want to call functions like `malloc` in this term.

For example, the following is a definition of a "boxed" integer type with some noisy messages:

```neut
resource boxed-int {
  // discarder: (pointer) -> unit
  function (v: pointer) {
    print("discarded!\n");
    free(v)
  },
  // copier: (pointer) -> pointer
  function (v: pointer) {
    let orig-value = load-int(v);
    let new-ptr = malloc(1);
    magic store(int, orig-value, new-ptr);
    new-ptr
  },
}

// provide a way to introduce new boxed integer
define create-new-boxed-int(x: int): boxed-int {
  let new-ptr = malloc(8);
  store-int(x, new-ptr);
  magic cast(int, boxed-int, new-ptr)
}
```

A value of type `boxed-int` prints `"discarded!\n"` when the value is discarded.

`resource` can be used to define low-level types like arrays.

You can find an example usage of `resource` in the `binary.nt` in the [core library](https://github.com/vekatze/neut-core/blob/main/source/binary.nt).

## `nominal`

`nominal` declares functions for forward references. It should look like the following:

```neut
nominal {
  is-odd(x: int): int,
}
```

An entry of `nominal` is the same form as found in `define`. Nominal definitions can be used to achieve mutual recursions:

```neut
nominal {
  is-odd(x: int): int, // nominal definition of `is-odd`
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

`foreign` declares functions that are defined in linked objects. It should look like the following:

```neut
foreign {
  neut_myapp_v1_add_const(int): int,
}
```

Foreign functions declared here can be called by using `magic external(..)`.

Suppose that you have a C source file with the following definition:

```c
// add_const.c

int64_t neut_myapp_v1_add_const(int64_t value) {
  return value + 100;
}
```

You can add the field `foreign` to your `module.ens` to compile and link this C source file, as written [here](modules.md#foreign). Under this setting, the following code can utilize `neut_myapp_v1_add_const`:

```neut
foreign {
  neut_myapp_v1_add_const(int): int,
}

define my-func(): int {
  let x: int = 10;
  magic external neut_myapp_v1_add_const(x)
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

Thus, the next is a valid use of `foreign`:

```neut
foreign {
  llvm.sin.f64(float): float,
}

define sin(x: float): float {
  magic external llvm.sin.f64(x)
}
```

Syscall wrapper functions and library functions are also available:

```neut
foreign {
  exit(c-int): void,
  sleep(c-int): c-int,
}
```

Here, the definition of `c-int` is as follows:

```neut
inline _c-int: type {
  introspect architecture {
  | amd64 =>
    int32
  | arm64 =>
    int32
  }
}

data c-int {
| C-Int(_c-int)
}
```

The type of each argument in every foreign entry must be a term that compiles to one of `int{N}`, `float{N}`, or `pointer` during compilation. For example, the `c-int` in `exit(c-int): void` is valid because it compiles to `int32` (thanks to an optimization like Haskell's `newtype`).

The resulting type of every foreign entry must be `void` or a term that compiles to one of `int{N}`, `float{N}`, or `pointer` during compilation.

When declaring a variadic function, declare only the non-variadic part:

```neut
foreign {
  printf(pointer): void,
}
```

Then, specify the types of variadic arguments when using `magic external`:

```neut
define print(t: &text): unit {
  // ..
  magic external printf(fmt)(len: int, val: pointer)
  //                         ^^^^^^^^^^^^^^^^^^^^^^
  //                         passing variadic arguments with types
}
```
