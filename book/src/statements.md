# Statements

## Table of Contents

- [import](#import)
- [namespace](#namespace)
- [define](#define)
- [inline](#inline)
- [define-meta](#define-meta)
- [trope](#trope)
- [inline-meta](#inline-meta)
- [constant](#constant)
- [constant-meta](#constant-meta)
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
  sample::baz,
  sample.another-module::foo,
  util::somefile {* as f},
  this::foo,
  this::foo.bar {some-func, other-func},
  this::foo.baz {* as baz, some-name, other-name as another-name},
}
```

`import` can only appear at the top of a file.

Every item in `import` is something like the following:

- `this::foo`
- `this::foo.bar {some-func, other-func}`
- `sample::baz`
- `sample.another-module::foo`
- `this::foo.baz {* as baz, some-name}`

The full form of an import item is:

```text
module.path::source.path
module.path::source.path {entry-1, ..., entry-n}
```

where each entry is one of the following:

```text
name
name as local-name
* as local-name
```

Module paths are dot-separated dependency aliases, with `this` as the identity element. For example, `sample.another-module::foo` imports `foo.nt` from a public dependency `another-module` of `sample`.

Source paths are the relative paths from the source directories. For example, if we want to import `(source-dir)/foo/bar.nt`, we'll have to write `foo.bar`.

Each entry in `{}` introduces an import alias. An entry `name` introduces the same name as an implicit import alias and makes it usable without qualifiers:

```neut
import {
  this::foo.bar {some-func},
}

define yo() -> unit {
  some-func(arg-1, arg-2)
}
```

Unlisted names must be qualified:

```neut
import {
  this::foo.bar,
}

define yo() -> unit {
  this::foo.bar::some-func(arg-1, arg-2)
}
```

An entry `name as local-name` introduces the explicitly chosen import alias:

```neut
import {
  this::foo.bar {some-func as f},
}

define yo() -> unit {
  f(arg-1, arg-2)
}
```

An entry `* as local-name` imports the whole file as a namespace under the explicitly chosen import alias:

```neut
import {
  this::foo.bar {* as bar},
}

define yo() -> unit {
  bar.some-func(arg-1, arg-2); // == this::foo.bar::some-func(arg-1, arg-2)
}
```

All import aliases share the same name environment as the names defined in the current file:

```neut
// error
import {
  this::foo {* as f},
  this::bar {* as f},
}

// error
import {
  this::bar {f},
  this::foo {* as f},
}

// error
import {
  this::bar {f},
  this::foo {some-name as f},
}
```

When the module path has multiple segments, the same path is used in the fully-qualified name:

```neut
import {
  sample.another-module::foo,
}

define yo() -> unit {
  sample.another-module::foo::some-func(arg-1, arg-2)
}
```

You can also list static files in `import`:

```neut
import {
  static-file {some-file, other-file}
}
```

For more on static files, please see [the section in Modules](modules.md#static-file).

## `namespace`

`namespace` groups names inside a file:

```neut
// arithmetic.nt

namespace integer {
  define add(x: int, y: int) -> int {
    add-int(x, y)
  }

  namespace checked {
    define increment(x: int) -> int {
      add(x, 1)
    }
  }
}
```

The names above can be imported and used as follows:

```neut
import {
  this::arithmetic {integer},
}

define example() -> int {
  let x = integer.add(1, 2);
  integer.checked.increment(x)
}
```

Namespaces can contain the ordinary statement forms and can be nested. Namespace bodies follow the usual statement order. In the example, `increment` can use the earlier `add`, but not a later member.

Moving a definition into a namespace changes its name; `integer.add` and a file-level `add` are different definitions.

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

The requirement of `lift` is checked against the specialized generated code, not the generic meta function body. This allows the following pattern:

```neut
define-meta lift-value<a>(x: 'a) -> '+a {
  quote {
    let y = unquote {x};
    lift {y} // `lift`ing `y: a`
  }
}

define use-meta() -> +unit {
  lift-value::(Unit)
}

```

`lift-value` typechecks even though it uses `lift` on `y: a`, since it is a meta function. Later, `use-meta` specializes it with `a := unit`, and `unit` is liftable, so `use-meta` is well-typed. Conversely, specializing with `a := &string` would be a type error, since `&string` is not liftable.

## `trope`

`trope` defines custom specializations of top-level `define-meta` functions. It should look like the following:

```neut
trope terse {
  define-meta print<bool>(x: '&bool) -> 'unit {
    quote {print("<bool>")}
  }

  define-meta print<int>(x: '&int) -> 'unit {
    quote {print("<int>")}
  }
}
```

Each entry must target a top-level `define-meta` function, and its type arguments must be written explicitly. The target function must not have default arguments. The body of each entry starts at stage 1, as with ordinary `define-meta`.

When a `trope` is enabled, its entries are available as pre-registered memoized specializations.

Entries in a `trope` are ordered. If multiple entries match the same meta function and type arguments, the later entry is used.

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
  let _ = x;
  let _ = empty-list<bool>;
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

## `constant-meta`

`constant-meta` defines a top-level meta constant. It should look like the following:

```neut
constant-meta foo: 'int {
  quote {10}
}

constant-meta bar<a>: 'int {
  quote {20}
}

define use-meta-constants() -> unit {
  print-int-line(foo);
  print-int-line(bar<int>)
}
```

`constant-meta` is the following syntax sugar:

```neut
constant-meta foo: 'int {
  quote {10}
}

constant-meta bar<a>: 'int {
  quote {20}
}

define use-meta-constants() -> unit {
  print-int-line(foo);
  print-int-line(bar<int>)
}

↓

inline-meta foo(): 'int {
  quote {10}
}

inline-meta bar<a>(): 'int {
  quote {20}
}

define use-meta-constants() -> unit {
  print-int-line(foo::());
  print-int-line(bar::<int>())
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

### Memory Representation

All the constructors of an ADT share the same allocation size: the size of its largest constructor. Each value stores a discriminant that identifies its constructor, followed by the constructor's fields.

For example, consider the following code:

```neut
data list(a) {
| Nil
| Cons(a, list(a))
}
```

For `list(a)`, every value occupies 4 words:

- 1 word for the discriminant,
- 1 word for `a`,
- 2 words for the largest constructor payload (`Cons(a, list(a))`).

So the internal representation of `Nil` is:

```neut
(0, a, _, _)
```

Here `0` is the discriminant for `Nil`, and the trailing two words are unused and remain uninitialized. Likewise, `Cons(10, xs)` is represented as:

```neut
(1, a, 10, xs)
```

where `1` is the discriminant for `Cons`. Even when a constructor carries fewer fields, the allocation size is still the one determined by the largest constructor; the unused slots are simply left untouched.

<div class="info-block">

A major motivation for this fixed allocation size is destination-passing style. By giving each ADT type a fixed size, the caller can allocate a destination buffer of the required size in advance.

</div>

#### Single-Constructor Types

When an ADT has just one constructor, its values carry no discriminant at all.

```neut
data point {
| Point(x: int, y: int)
}
```

Since `point` has a single constructor, the internal representation of `Point(10, 20)` is simply:

```neut
(10, 20)
```

rather than the `(0, 10, 20)` we would get if a discriminant were stored.

#### Mixing Nested Fields

Consider the following code:

```neut
data point {
| Point(x: int, y: int)
}

data entity {
| Entity(point, point)
}
```

By default, `Entity(Point(1, 2), Point(3, 4))` is compiled into a pointer to:

```neut
(ptr1, ptr2)
```

where:

- `ptr1` points to `(1, 2)`,
- `ptr2` points to `(3, 4)`.

You can mix the content of `point` into `entity` by:

```neut
data entity {
| Entity(point mix, point)
}
```

In this case, `Entity(Point(1, 2), Point(3, 4))` is compiled into a pointer to:

```
(1, 2, ptr2)
```

where `ptr2` points to `(3, 4)`.

Taking a mixed field out with `match` or `let` repacks it into a fresh allocation:

```neut
let Entity(p, q) = e;
cont

// ↓ (compile)

// repack `p` from `e` into a fresh allocation
let p = malloc({2-words});
store(p[0], e[0]);  // x1
store(p[1], e[1]);  // y1
// q isn't mixed
let q = e[2];
cont
```

Reading it through a noema with `case` or `tie` does no repacking. `p` and `q` become interior pointers into `e`, with no allocation:

```neut
tie Entity(p, q) = e;
cont

// ↓ (compile)

// (`p` and `q` point directly into `e`'s words; no malloc)
```

`mix` can also be used with a `resource` type when the resource has a fixed non-negative byte size. The mixed field uses the minimum number of words that can contain those bytes.

## `alias`

`alias` defines a type alias. It should look like the following:

```neut
alias my-type {
  int
}

alias mylist(a) {
  list(a)
}

define use-my-type(xs: &mylist(int), y: my-type) -> my-type {
  let len = core::list::length(xs);
  add-int(len, y) // well-typed
}
```

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
  (value: pointer, should-release: int) => {
    // .. discard the value ..
  },
  (value: pointer, dest: pointer) => {
    // .. copy the value ..
  },
  size, // integer value
}
```

`resource` takes three terms. The first term ("discarder") receives a value of the type and a `should-release` flag, then discards it. The second term ("copier") receives a value of the type and an optional destination pointer. The third term must reduce to an integer at compile time. It is the size of the flattened representation in bytes, and is returned as-is when calling `magic call-type(my-type, 2, null, null)`.

The type of a discarder is `(pointer, int) -> unit`. The value is passed as a pointer; cast it to the intended representation inside the discarder. If `should-release` is `0`, it must destroy the contents without releasing the outer storage. If `should-release` is `1`, it performs the ordinary owned discard. For a resource whose size is negative, `should-release` is always `1`.

The type of a copier is `(pointer, pointer) -> pointer`. If the destination pointer is `null`, the copier returns an owned copy. If the destination pointer is not `null`, the copier writes the copy into the destination; the return value is unspecified and must be ignored. For a resource whose size is negative, the destination is always `null`.

The third term must have type `int`.

For example, the following is a definition of a "boxed" integer type with some noisy messages:

```neut
resource boxed-int {
  // discarder: (pointer, int) -> unit
  (v: pointer, should-release: int) => {
    print("discarded!\n");
    free(v)
  },
  // copier: (pointer, pointer) -> pointer
  (v: pointer, dest: pointer) => {
    let orig-value = load-int(v);
    let new-ptr = malloc(8);
    magic store(int, orig-value, new-ptr);
    if is-null-pointer(dest) {
      new-ptr
    } else {
      store-pointer(new-ptr, dest);
      null-pointer // this return value is ignored
    }
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

`rule-right` defines a variable-length rule application that expands in a fold-right manner. It should look like the following:

```neut
rule-right name {
  leaf,
  node,
  root,
}
```

Once defined, `name` can be used with `::[...]`:

```neut
name::[x, y, z, w]
```

This expands in a fold-right manner to:

```neut
unquote {
  root(
    node(
      quote {x},
      node(
        quote {y},
        node(
          quote {z},
          node(
            quote {w},
            leaf(4)
          )
        )
      )
    )
  )
}
```

where the `4` is the length of `[x, y, z, w]`.

### Example: List Construction

The `List` construct available in the core library is defined using `rule-right`:

```neut
rule-right List {
  inline-meta leaf<a>(_: int) -> 'list(a) {
    quote {Nil}
  },
  inline-meta node<a>(x: 'a, acc: 'list(a)) -> 'list(a) {
    quote {
      Cons(unquote {x}, unquote {acc})
    }
  },
  inline-meta root<a>(x: 'list(a)) -> 'list(a) {
    x
  },
}
```

With this definition, `List::[x, y, z]` simplifies as follows:

```neut
List::[x, y, z]

↓

unquote {root(node(quote {x}, node(quote {y}, node(quote {z}, leaf(3)))))}

↓

Cons(x, Cons(y, Cons(z, Nil)))
```

## `rule-left`

`rule-left` defines a variable-length rule application that expands in a fold-left manner. It should look like the following:

```neut
rule-left name {
  leaf,
  node,
  root,
}
```

As with `rule-right`, `leaf`, `node`, and `root` must be meta functions that assemble code. Once defined, `name` can be used with `::[...]`:

```neut
name::[x, y, z, w]
```

This expands in a fold-left manner to:

```neut
unquote {
  root(
    node(
      node(
        node(
          node(
            leaf(4),
            quote {x}
          ),
          quote {y}
        ),
        quote {z}
      ),
      quote {w}
    )
  )
}
```

where the `4` is the length of `[x, y, z, w]`. As with `rule-right`, `name::[..]` can be used at any stage.

### Example: Vector Construction

The `Vector` construct available in the core library is defined using `rule-left`:

```neut
rule-left Vector {
  inline-meta leaf<a>(size: int) -> 'vector(a) {
    quote {make(unquote {promote {size}})}
  },
  inline-meta node<a>(acc: 'vector(a), x: 'a) -> 'vector(a) {
    quote {push-back(unquote {acc}, unquote {x})}
  },
  inline-meta root<a>(x: 'vector(a)) -> 'vector(a) {
    x
  },
}
```

With this definition, `Vector::[a, b, c]` simplifies as follows:

```neut
Vector::[a, b, c]

↓

unquote {root(node(node(node(leaf(3), quote {a}), quote {b}), quote {c}))}

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

Default arguments can't be used in `nominal`.

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
  introspect target-arch {
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
