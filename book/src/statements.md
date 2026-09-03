# Statements

## Table of Contents

- [import](#import)
- [require](#require)
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
- [expose](#expose)

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

Source paths are the relative paths from the source directories. For example, if we want to import `(source-dir)/foo/bar.nt`, we'll have to write `foo.bar`. A dot in a source path therefore always stands for a directory, and neither a directory nor a source file under a source directory may have a dot in its name.

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

An item can also be a choice between two groups of items, made according to a capability:

```neut
import {
  if thread {
    core::sync.pool {for-each-parallel},
  } else {
    this::serial {for-each-parallel},
  },
}
```

If the platform of the target provides the capability, the `if` branch is taken; otherwise the `else` branch is. Both branches are mandatory.

Both branches must introduce the same names, and those names must have the same types. The following cannot appear inside a branch:

- a name that has no type, such as a namespace
- an item with no `{}`
- a static file

A source that a branch imports cannot be named in full. For example, the `else` branch above imports `this::serial`, but `this::serial::for-each-parallel` cannot be written anywhere in the file; you must use `for-each-parallel` instead.

A branch can contain any number of items, including further choices.

The branch that is not taken is still read and type-checked. The files it names are not compiled or linked.

You can also list static files in `import`:

```neut
import {
  static-file {some-file, other-file}
}
```

For more on static files, please see [the section in Modules](modules.md#static-file).

## `require`

`require` declares the capabilities that a file needs. It should look like the following:

```neut
require {
  thread,
}
```

It exists to catch a mismatch before anything is compiled. Without it, code written for threads on a platform that has none would surface as linker errors naming missing symbols, for example.

`require` can only appear at the top of a file. `require` and `import` can be written there in any order.

The following capabilities exist:

- `thread`: threads, with the mutexes and condition variables that go with them
- `subprocess`: child processes
- `javascript`: a JavaScript host to reach

Which capabilities a platform provides is part of the definition of that platform. See [platform](./modules.md#platform).

`require { thread }` says that the file is meant for a platform where threads exist. It introduces nothing into the file and gives it nothing to work with. Threads themselves come from a library, such as `core::sync`.

`neut build` rejects a target whose platform doesn't provide a required capability:

```text
source/flow.nt:3:3
Error: `thread` is not available on wasm32:
            this::flow
         ~> core::sync.channel
         ~> core::sync._posix
```

The chain shows how the build reached the file that declares the requirement. The error points at the import that led out of the module being built.

`require` can only be used when `universal` is `false`.

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

`define` can also declare default arguments by inserting `[z1: c1 := d1, ..., zk: ck := dk]` between the ordinary parameter list and `->`:

```neut
define bump(x: int)[step: int := 1] -> int {
  add-int(x, step)
}
```

Such a function has type `(x: int)[step: int] -> int`, and callers can override the default with `bump(10)[step := 5]`. If the caller omits `step`, its default expression is evaluated at the time of the call. The bracketed part may be omitted, and `[]` is also accepted.

`define` can optionally have implicit type parameters, as in `identity` in the example above. The compiler inserts these type parameters at compile time, so you don't have to write them explicitly:

```neut
define use-func-with-implicit-arg() -> int {
  let x = 10;
  let z = identity(x);
  z
}
```

A function with the same name can't be defined in the same file.

Statements are order-sensitive as in F#. Thus, the following code results in an error:

```neut
define bar() -> int {
  foo() // `foo` is undefined here
}

define foo() -> int {
  10
}
```

You have to use the statement `nominal` explicitly for forward references.

A `define` also records the calling convention of the function. `->>` in place of `->` means that the caller provides the place the result is written into (destination-passing style):

```neut
define scale(it: item, k: int) ->> item {
  body
}

define use-scale() -> item {
  // a call to a `->>` function introduces its argument list with `@`
  scale@(Item(42), 10)
}

// ↓ compile (pseudo-code)

define scale(dst: pointer, it: item, k: int) -> item {
  let result = body;
  memcpy(dst, result);
  free(result);
  dst
}

define use-scale() -> item {
  let dst = malloc(size(item));
  scale(dst, Item(42), 10)
}

```

`+` in front of a parameter means that the caller provides the place that argument is read from (source-passing style):

```neut
define area(+s: shape) -> int {
  match s {
  | Circle(r) =>
    r
  | Rect(w, h) =>
    mul-int(w, h)
  }
}

define use-area() -> int {
  // an argument that fills a `+` parameter is written with `~`
  area(~Circle(42))
}

// ↓ compile (pseudo-code)

define area(s-src: pointer) -> int {
  let s = malloc(size(shape));
  memcpy(s, s-src);
  match s {
  | Circle(r) =>
    r
  | Rect(w, h) =>
    mul-int(w, h)
  }
}

define use-area() -> int {
  let val = Circle(42);
  let src = malloc(size(shape));
  src[i] := val[i]
  let result = area(src);
  free(src);
  result
}
```

As the comments above say, a convention is chosen on the definition and echoed at every call: `->>` is answered by `@`, and `+` is answered by `~`. The two sides must agree, so neither mark can be added or left out on its own.

A convention chosen this way is part of the function type, so `(a) ->> b` and `(a) -> b` are different types, and so are `(+x: a) -> b` and `(x: a) -> b`.

The type of a `+` parameter and the result type of a `->>` function must be a type that can be stored inline. A type variable in such a position must be declared `sized`:

```neut
define push-back<sized a>(xs: array(a), +x: a) -> array(a) {
  // ...
}
```

`sized a` means that `a` can only be instantiated with a type that can be stored inline, so `push-back(xs, ~Item(1))` is accepted and `push-back(xs, ~1)` is rejected.

Every tail-recursive call in Neut is optimized into loops as long as it isn't a source-passing style function.

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

`inline` records a calling convention in the same way as `define`. For the details of this behavior, please see the section on [functions in Terms](./terms.md#x1-a1--xn-an---e-).

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

Unlike `define`, `define-meta` can't have default arguments, since a call is memoized by its type arguments alone. Wrap it in an `inline-meta` when a default is needed.

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

All the constructors of an ADT share the same allocation size: the size of its largest constructor. Each value stores a discriminant that identifies its constructor, then one entry per type argument, then the constructor's fields.

Fields are laid out in the order they are written. Each one takes as many bytes as its type needs:

| field | bytes |
|---|---|
| `int8` and narrower | 1 |
| `int16`, `float16` | 2 |
| `int32`, `float32`, `rune` | 4 |
| `int64`, `float64`, `int`, `float` | 8 |
| anything represented by a pointer | the pointer of the target |
| a type variable | 8 |

The discriminant is an integer just wide enough to tell the constructors apart:

- 1 byte for up to 256 constructors,
- 2 bytes for up to 65536 constructors,
- 4 bytes beyond that.

Every entry starts at the next multiple of its own width, so a value is always read at the alignment its width asks for (i.e. natural alignment). The bytes between entries are padding and hold nothing, and the size of the whole value is rounded up to the width of its widest entry.

For example, consider the following code:

```neut
data list(a) {
| Nil
| Cons(a, list(a))
}
```

The internal representation of `Cons(10, xs)` is:

```neut
(1, a, 10, xs)
```

where `1` is the discriminant for `Cons`, `a` is the type descriptor for the element type, and `10` and `xs` are the fields. On a 64-bit target the value takes 32 bytes:

- 1 byte for the discriminant, then 7 bytes of padding,
- 8 bytes for `a`,
- 8 bytes for `10`,
- 8 bytes for `xs`.

On a 32-bit target `a` and `xs` take four bytes each, so the value takes 20 bytes rounded up to 24. The element takes eight bytes on both, since `Cons` holds a value whose type it does not know.

The representation of `Nil` is:

```neut
(0, a, _, _)
```

Even when a constructor carries fewer fields, the allocation size is still the one determined by the largest constructor; the unused bytes are simply left untouched.

A value of a type variable takes eight bytes whatever the target, so writing an explicit width keeps a field at the width its own type asks for.

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

#### Storing Fields Inline

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

You can store the content of `point` inline in `entity` by:

```neut
data entity {
| Entity(+point, point)
}
```

A constructor is an ordinary function, so a `+` field is filled by a `~` argument. In this case, `Entity(~Point(1, 2), Point(3, 4))` is compiled into a pointer to:

```
(1, 2, ptr2)
```

where `ptr2` points to `(3, 4)`.

An inline field starts at the alignment its type asks for and takes as many bytes as a value of that type takes, including the padding at its end.

A pattern carries `+` as well, since it names a field that the `data` stores inline:

```neut
let Entity(+p, q) = e;
cont

// ↓ (compile)

// repack `p` from `e` into a fresh allocation
let p = malloc({size-of-point});
store(p[0], e[0]);  // x1
store(p[1], e[1]);  // y1
// q isn't stored inline
let q = e[2];
cont
```

Taking such a field out with `match` or `let` repacks it into a fresh allocation, as above.

Reading it through a noema with `case` or `tie` does no repacking: `p` and `q` become interior pointers into `e`, with no allocation. The mark is written the same way in both, since it describes the field rather than the way the field is read:

```neut
tie Entity(+p, q) = e;
cont

// ↓ (compile)

// (`p` and `q` point directly into `e`'s bytes; no malloc)
```

The same mark can also be used with a `resource` type when the resource has a fixed non-negative byte size. The field then takes exactly those bytes.

The mark is the same one that is used for a source-passing parameter. For the details, please see [function types in Terms](./terms.md#x1-a1--xn-an---b).

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
  inline twice(x: int) -> int,
  constant answer: int,
  define-meta emit(x: 'int) -> 'int,
  constant-meta limit: 'int,
  alias number,
  alias-opaque handle(a),
  data stream(a: type),
  resource buffer,
}
```

An entry is written like the header of the corresponding statement, without its body. A `data` entry declares only the type; its constructors become available at the real definition.

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
- `constant-meta`
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

Here, `c-int` is defined in the core library as follows:

```neut
data c-int {
| C-Int(int32)
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

Then, specify the types of variadic arguments when using `magic external`. Each variadic argument is written as its lowtype followed by the term:

```neut
define print-raw(fmt: pointer, len: int, val: pointer) -> c-int {
  magic external printf(fmt)(int len, pointer val)
  //                        ^^^^^^^^^^^^^^^^^^^^^^
  //                        passing variadic arguments with types
}
```

## `expose`

`expose` makes functions in the current file callable from outside Neut. It should look like the following:

```neut
define add-const(x: int) -> int {
  add-int(x, 100)
}

expose {
  add-const as my_app_add_const,
}
```

The wrapper is then called from C as follows:

```c
int64_t my_app_add_const(int64_t x);

int64_t y = my_app_add_const(7); // 107
```

Each entry names a function defined in the same file, optionally followed by `as` and its external name. Without `as`, the name of the function is used. No two entries can expose the same external name, and the entry point of the target (`main`, or `__main_argc_argv` on wasm32) can't be exposed.

The compiler emits a wrapper with the C calling convention for each entry. On native targets, the external name becomes a public symbol. On wasm targets, it also becomes a wasm export.

Every parameter of the wrapper and its result are 64-bit integers. A pointer is zero-extended to that width.

The wrapper takes the parameter list of the compiled function, so `expose` publishes the calling convention as well:

- a type parameter becomes a leading parameter
- a default argument becomes an ordinary parameter, with no default
- a destination-passing function (one written with `->>`) takes its destination first, and returns it

A destination-passing function, for example:

```neut
data pair-of-int {
| Pair-Of-Int(int, int)
}

define make(x: int) ->> pair-of-int {
  Pair-Of-Int(x, x)
}

expose {
  make as my_app_make,
}
```

```c
int64_t my_app_make(int64_t destination, int64_t x);

int64_t storage[2];
my_app_make((int64_t)storage, 7);
```
