# Statements

## Table of Contents

- [import](#import)
- [define](#define)
- [inline](#inline)
- [define-meta](#define-meta)
- [inline-meta](#inline-meta)
- [constant](#constant)
- [data](#data)
- [alias](#alias)
- [alias-opaque](#alias-opaque)
- [resource](#resource)
- [rule-right](#rule-right)
- [rule-left](#rule-left)
- [nominal](#nominal)
- [foreign](#foreign)

## `import`

`import` imports names from other files. It should look like the following:

```neut
import {
  sample.baz,
  this.foo,
  this.foo.bar {some-func, other-func},
}
```

`import` can only appear at the top of a file.

Every item in `import` is something like the following:

- `this.foo`
- `this.foo.bar {some-func, other-func}`
- `sample.baz`

An import item starts from the alias of the module (`this`, `sample`). The alias of the module is specified in `dependency` in `module.ens`. If the file we want to import is inside the current module, we'll write `this`.

The remaining part of the item is the relative path from the source directory. For example, if we want to import `(source-dir)/foo/bar`, we'll have to write `foo.bar` after the alias of the module.

An import item can be constructed by concatenating the alias and the path with `.`. In the case of `this.foo.bar`, the alias part is `this`, and the path part is `foo.bar`.

You can specify names in `{}`. The names specified here can be used without qualifiers:

```neut
import {
  this.foo.bar {some-func},
}

define yo() -> unit {
  some-func(arg-1, arg-2)
}
```

Unlisted names must be qualified:

```neut
import {
  this.foo.bar,
}

define yo() -> unit {
  this.foo.bar.some-func(arg-1, arg-2)
}
```

You can also list text files in `import`:

```neut
import {
  text-file {some-file, other-file}
}
```

For more on text files, please see [the section in Modules](modules.md#text-file).

## `define`

`define` defines a function. It should look like the following:

```neut
define foo(x: int, y: int) -> int {
  add-int(x, y)
}

define identity<a>(x: a) -> a {
  x
}
```

Defined functions can then be used:

```neut
define use-foo() -> int {
  foo(1, 2)
}
```

`define` can also declare default arguments by inserting `[z1: c1 := d1, ..., zk: ck := dk]` between the ordinary parameter list and `->` (or `->>`):

```neut
define bump(x: int)[step: int := 1] -> int {
  add-int(x, step)
}
```

Such a function has type `(x: int)[step: int] -> int`, and callers can override the default with `bump(10)[step := 5]`. If the caller omits `step`, its default expression is evaluated at the time of the call. The bracketed part may be omitted, and `[]` is also accepted.

`define` also accepts `->>` in place of `->`. Such a function is still called in the usual way, but its compiled code uses destination-passing style. For the details of this behavior, please see the section on [functions in Terms](./terms.md#x1-a1--xn-an---e-).

`define` can optionally have implicit type parameters, as in `identity` in the example above. The compiler inserts these type parameters at compile time, so you don't have to write them explicitly:

```neut
define use-func-with-implicit-arg() -> int {
  let x = 10;
  let z = identity(x);
  z
}
```

A function with the same name can't be defined in the same file.

All tail-recursive calls in Neut are optimized into loops.

Note that statements are order-sensitive as in F#. Thus, the following code results in an error:

```neut
define bar() -> int {
  foo() // `foo` is undefined here
}

define foo() -> int {
  10
}
```

You have to use the statement `nominal` explicitly for forward references.

## `inline`

`inline` defines an inline function. It should look like the following:

```neut
inline foo(x: int, y: int) -> int {
  print("foo");
  add-int(x, y)
}
```

`inline` is the same as `define` except that the definition is always expanded at compile time. For example, if you write

```neut
define use-inline-foo() -> int {
  let val = foo(10, 20);
  val
}
```

The compiler will translate the above code into the following:

```neut
define use-inline-foo() -> int {
  let val = {
    let x = 10;
    let y = 20;
    print("foo");
    add-int(x, y)
  };
  val
}
```

`inline` also accepts `->>` in place of `->`. As with `define`, such a function is still called in the usual way, while the compiled code uses destination-passing style. For the details of this behavior, please see the section on [functions in Terms](./terms.md#x1-a1--xn-an---e-).

As with `define`, you can also place a default-argument list in `[]` between the ordinary parameter list and the arrow.

## `define-meta`

`define-meta` defines a top-level meta function. It should look like the following:

```neut
define-meta make-pair<a, b>(x: 'a, y: 'b) -> 'pair(a, b) {
  quote {
    let x = unquote {x};
    let y = unquote {y};
    Pair(x, y)
  }
}
```

`define-meta` starts at stage 1. When evaluating a call to `define-meta`, the compiler first specializes the definition to its type arguments and memoizes the result. This memoization is performed on a per-file basis. This allows `define-meta` to generate recursive code.

As with ordinary functions, `define-meta` can also have default arguments by placing `[]` between the ordinary parameter list and the arrow.

Every explicit parameter of `define-meta` must have a type of the form `'a`:

```neut
// valid
define-meta eq-data<a>(x: 'a, y: 'a) -> 'bool {
  ..
}

// invalid
define-meta bad<a>(x: int) -> 'int {
  ..
}
```

## `inline-meta`

`inline-meta` defines an inline meta function. It should look like the following:

```neut
inline-meta duplicate(x: 'int) -> 'pair(int, int) {
  quote {
    let y = unquote {x};
    Pair(y, y)
  }
}
```

`inline-meta` is the same as `inline` except that the body starts at stage 1, not 0.

It also supports the same default-argument syntax as `define-meta`.

## `constant`

`constant` defines a top-level constant. It should look like the following:

```neut
constant foo: int {
  10
}

constant empty-list<a>: list(a) {
  Nil
}

define use-constants() -> list(int) {
  let x = foo;
  empty-list
}
```

The compiler tries to reduce the body of a `constant` into a value at compile time. The compiler reports an error if it can't get a value. For example, the following should result in an error:

```neut
constant bar: int {
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
    foo-path: &string,
    colorize: bool,
  )
}
```

You can use the content of an ADT value by using `match` or `case`:

```neut
define length<a>(xs: list(a)) -> int {
  // destructure ADT values using `match`
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}

define length-noetic<a>(xs: &list(a)) -> int {
  // read noetic ADT values using `case`
  case xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length-noetic(ys))
  }
}

define use-config(c: config) -> int {
  // pattern-matching in `let` is also possible
  let Config{count, foo-path} = c;
  let _ = foo-path;
  count
}
```

## `alias`

`alias` defines a type alias. It should look like the following:

```neut
alias optional(a: type) {
  either(unit, a)
}

alias my-type {
  either(int, bool)
}
```

The body of `alias` can be used wherever a type is expected.

## `alias-opaque`

`alias-opaque` defines an opaque type alias. It should look like the following:

```neut
alias-opaque vector(_: type) {
  _vector-internal
}

alias-opaque my-type {
  either(int, bool)
}
```

`alias-opaque` can be used when you want to expose a type constructor while hiding its actual body.

## `resource`

`resource` defines a new type by specifying how to discard/copy the values of the type. It should look like the following:

```neut
resource my-type {
  (value: pointer) => {
    // .. discard the value ..
  },
  (value: pointer) => {
    // .. create a new clone of the value and return it as pointer ..
  },
  size, // integer value
}
```

`resource` takes three terms. The first term ("discarder") receives a value of the type and discards it. The second term ("copier") receives a value of the type and returns a clone of the value (keeping the original value intact). The third term is the size returned when calling `magic call-type(my-type, 2, (..))`. This size is also used when a value of the type is returned from a function written using `->>`: when the size is non-negative, the caller prepares a destination of that size, and otherwise it uses a one-word temporary slot.

The type of a discarder is `(a) -> unit` for some `a`. You might want to call functions like `free` in this term.

The type of a copier is `(a) -> a` for some `a`. This `a` must be the same as the `a` used in the discarder. You might want to call functions like `malloc` in this term.

The third term must have type `int`. See also: [Semantics (call-type)](./terms.md#semantics-call-type)

For example, the following is a definition of a "boxed" integer type with some noisy messages:

```neut
resource boxed-int {
  // discarder: (pointer) -> unit
  (v: pointer) => {
    print("discarded!\n");
    free(v)
  },
  // copier: (pointer) -> pointer
  (v: pointer) => {
    let orig-value = load-int(v);
    let new-ptr = malloc(8);
    magic store(int, orig-value, new-ptr);
    new-ptr
  },
  -1,
}

// provide a way to introduce a new boxed integer
define create-new-boxed-int(x: int) -> boxed-int {
  let new-ptr = malloc(8);
  store-int(x, new-ptr);
  magic cast(pointer, boxed-int, new-ptr)
}
```

A value of type `boxed-int` prints `"discarded!\n"` when the value is discarded.

`resource` can be used to define low-level types like arrays.

You can find an example usage of `resource` in the `binary.nt` in the [core library](https://github.com/vekatze/neut-core/blob/main/source/binary.nt).

## `rule-right`

`rule-right` defines a macro-like construct that expands bracketed expressions in a fold-right manner. It should look like the following:

```neut
rule-right name {
  leaf,
  node,
  root,
}
```

Once defined, `name` can be used with square brackets to accept variable-length arguments:

```neut
name[x, y, z, w]
```

This expands in a fold-right manner to:

```neut
root(node(x, node(y, node(z, node(w, leaf(4))))))
```

where the `4` is the length of `[x, y, z, w]`.

### Example: List Construction

The `List` construct available in the core library is defined using `rule-right`:

```neut
rule-right List {
  inline leaf<a>(_: int) -> list(a) {
    Nil
  },
  inline node<a>(x: a, acc: list(a)) -> list(a) {
    Cons(x, acc)
  },
  inline root<a>(x: a) -> a {
    x
  },
}
```

With this definition, `List[x, y, z]` simplifies as follows:

```neut
List[x, y, z]

↓

root(node(x, node(y, node(z, leaf(3)))))

↓

Cons(x, Cons(y, Cons(z, Nil)))
```

## `rule-left`

`rule-left` defines a macro-like construct that expands bracketed expressions in a fold-left manner. It should look like the following:

```neut
rule-left name {
  leaf,
  node,
  root,
}
```

Once defined, `name` can be used with square brackets to accept variable-length arguments:

```neut
name[x, y, z, w]
```

This expands in a fold-left manner to:

```neut
root(node(node(node(node(leaf(4), x), y), z), w))
```

where the `4` is the length of `[x, y, z, w]`.

### Example: Vector Construction

The `Vector` construct available in the core library is defined using `rule-left`:

```neut
rule-left Vector {
  inline leaf<a>(size: int) -> vector(a) {
    make(size)
  },
  inline node<a>(acc: vector(a), x: a) -> vector(a) {
    push-back(acc, x)
  },
  inline root<a>(x: a) -> a {
    x
  },
}
```

With this definition, `Vector[a, b, c]` simplifies as follows:

```neut
Vector[a, b, c]

↓

root(node(node(node(leaf(3), a), b), c))

↓

push-back(push-back(push-back(make(3), a), b), c)
```

## `nominal`

`nominal` declares top-level items for forward references. It should look like the following:

```neut
nominal {
  define is-odd(x: int) -> bool,
  data stream(a: type),
}
```

Nominal definitions can be used to achieve mutual recursion:

```neut
nominal {
  define is-odd(x: int) -> bool, // nominal definition of `is-odd`
}

// given a non-negative integer `x`, returns true if `x` is even.
define is-even(x: int) -> bool {
  if eq-int(x, 0) {
    True
  } else {
    is-odd(sub-int(x, 1)) // ← using nominal definition
  }
}

// given a non-negative integer `x`, returns true if `x` is odd.
// ("real" definition of `is-odd`)
define is-odd(x: int) -> bool {
  if eq-int(x, 0) {
    False
  } else {
    is-even(sub-int(x, 1))
  }
}
```

If a nominal definition isn't followed by a corresponding real definition, the compiler reports an error.

The following kinds of top-level items can be declared in `nominal`:

- `define`
- `inline`
- `constant`
- `define-meta`
- `inline-meta`
- `alias`
- `alias-opaque`
- `data`
- `resource`

## `foreign`

`foreign` declares functions that are defined in linked objects. It should look like the following:

```neut
foreign {
  neut_myapp_v1_add_const(int) -> int,
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

By configuring the `foreign` field in `module.ens` as described in [Modules](./modules.md#foreign), you can use the C function above as follows:

```neut
foreign {
  neut_myapp_v1_add_const(int) -> int,
}

define my-func() -> int {
  let x: int = 10;
  magic external neut_myapp_v1_add_const(x)
}
```

You can also use LLVM intrinsics. For example, the LLVM LangRef states that the `llvm.sin.*` intrinsic is [available](https://llvm.org/docs/LangRef.html#llvm-sin-intrinsic):

```llvm
declare float     @llvm.sin.f32(float  %Val)
declare double    @llvm.sin.f64(double %Val)
declare x86_fp80  @llvm.sin.f80(x86_fp80  %Val)
declare fp128     @llvm.sin.f128(fp128 %Val)
declare ppc_fp128 @llvm.sin.ppcf128(ppc_fp128  %Val)
```

Thus, the following is a valid use of `foreign`:

```neut
foreign {
  llvm.sin.f64(float) -> float,
}

define sin(x: float) -> float {
  magic external llvm.sin.f64(x)
}
```

Syscall wrapper functions and library functions are also available:

```neut
foreign {
  exit(c-int) -> void,
  sleep(c-int) -> c-int,
}
```

Here, the definition of `c-int` is as follows:

```neut
constant _c-int: type {
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

The type of each parameter in every foreign entry must be a term that compiles to one of `int{N}`, `float{N}`, or `pointer` during compilation. For example, the `c-int` in `exit(c-int) -> void` is valid because it compiles to `int32` (thanks to an optimization like Haskell's `newtype`).

The resulting type of every foreign entry must be `void` or a term that compiles to one of `int{N}`, `float{N}`, or `pointer` during compilation.

When declaring a variadic function, declare only the non-variadic part:

```neut
foreign {
  printf(pointer) -> c-int,
}
```

Then, specify the types of variadic arguments when using `magic external`:

```neut
define print-raw(fmt: pointer, len: int, val: pointer) -> c-int {
  magic external printf(fmt)(len: int, val: pointer)
  //                                 ^^^^^^^^^^^^^^^^^^^^^^
  //                                 passing variadic arguments with types
}
```
