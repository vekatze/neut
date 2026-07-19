# Basis

## Table of Contents

- [On Executing Types](#on-executing-types)
- [Free-Malloc Canceling](#free-malloc-canceling)
- [Malloc-Free Canceling](#malloc-free-canceling)
- [Order of Memory Optimizations](#order-of-memory-optimizations)
- [Name Resolution](#name-resolution)
- [Leading Bars and Trailing Commas](#leading-bars-and-trailing-commas)
- [Compiler Configuration](#compiler-configuration)
- [Other Basic Facts](#other-basic-facts)

## On Executing Types

A type in Neut is compiled into a pointer to a binary function like the following (pseudocode):

```neut
define discard-or-copy-value(action-selector, value) {
  if eq-int(action-selector, 0) {
    discard-value(value);
    Unit
  } else {
    let new-value = copy-value(value);
    new-value
  }
}
```

These functions are then used to discard/copy values when necessary.

### Discarding Values

Let's see how types are executed when discarding values. For example, consider the following code:

```neut
define foo(xs: list(int)) -> unit {
  Unit
}
```

Note that the variable `xs` isn't used. Because of that, the compiler translates the code above into the following (pseudocode; won't typecheck):

```neut
define foo(xs: list(int)) -> unit {
  let f = list(int);
  f(0, xs); // passing `0` to discard `xs`
  Unit
}
```

Note that the above example executes the type `list(int)` as a function.

### Copying Values

Let's see how types are executed when copying values. For example, consider the following code:

```neut
define foo(!xs: list(int)) -> unit {
  some-func(xs, xs)
}
```

Note that the variable `xs` is used twice. Because of that, the compiler translates the above code into the following (pseudocode; won't typecheck):

```neut
define foo(!xs: list(int)) -> unit {
  let f = list(int);
  let xs-clone = f(1, xs); // passing `1` to copy `xs`
  some-func(xs-clone, xs)
}
```

Note that the above example executes the type `list(int)` as a function.

You must prefix a variable with `!` at its definition site if the variable may need to be copied. Likewise, if a free variable captured by a term-level `define` cannot be copied for free, that free variable must have been defined with the `!` prefix.

The prefix `!` is unnecessary if the variable can be copied for free.

### On Immediate Values

We don't have to discard immediates like integers or floats because their internal representations don't depend on memory-related operations like `malloc` or `free`. Because of that, "discarding" immediate values does nothing. Also, "copying" immediate values means reusing the original values.

More specifically, the type of an immediate is compiled into a pointer to the following function (pseudocode):

```neut
inline discard-or-copy-immediate(selector, value) {
  if eq-int(selector, 0) {
    0     // discard: we have nothing to do on `value`
  } else {
    value // copy: we can simply reuse the immediate `value`
  }
}
```

These fake discard/copy operations are optimized away at compile time.

Also, this function is internally called `"base::#::imm"`. Try compiling your project as follows:

```sh
neut build TARGET --emit llvm --skip-link
```

Then, take a peek at the `build` directory. You'll find the name here and there.

<div class="info-block">

Since every type is translated into a pointer to a function, a type is an immediate value. Thus, `type` is compiled into `base::#::imm`.

</div>

## Free-Malloc Canceling

Thanks to its static nature, memory allocation in Neut can sometimes be optimized away. Consider the following code:

```neut
data int-list {
| Nil
| Cons(int, int-list)
}

// [1, 5, 9] => [2, 6, 10]
define increment(xs: int-list) -> int-list {
  match xs {
  | Nil =>
    Nil
  // ↓ the `Cons` clause
  | Cons(x, rest) =>
    Cons(add-int(x, 1), increment(rest))
  }
}
```

The expected behavior of the `Cons` clause above would be something like the following:

1. obtain `x` and `rest` from `xs`
2. `free` the outer tuple of `xs`
3. calculate `add-int(x, 1)` and `increment(rest)`
4. allocate a memory region using `malloc` to hold the result
5. store the calculated values to the pointer and return it

However, since the size of `Cons(x, rest)` and `Cons(add-int(x, 1), increment(rest))` is known to be the same at compile time, the pair of `free` and `malloc` can be optimized away, as follows:

1. obtain `x` and `rest` from `xs`
2. calculate `add-int(x, 1)` and `increment(rest)`
3. store the calculated values to `xs` (overwrite)

Neut performs this optimization. When a `free` is required, Neut looks for a later `malloc` whose allocation can fit in the freed region and optimizes away such a pair if one exists. The resulting assembly code thus performs in-place updates.

### Size Matching

A known-size `free` can be canceled with a later known-size `malloc` when the freed region is large enough for the allocation.

For example, the following lowered pseudocode can be optimized because the sizes are the same:

```neut
free(p, 16);
let q = malloc(16);
cont

// ↓

let q = p;
cont
```

The next one can also be optimized because the freed region is larger than the later allocation:

```neut
free(p, 16);
let q = malloc(8);
cont

// ↓

let q = p;
cont
```

### Search Order

When there are multiple possible allocations in the same lowered continuation, the freed region is reused for the earlier suitable allocation.

For example, in the following lowered pseudocode, the region pointed to by `p` is reused for `q`:

```neut
free(p, 16);
let q = malloc(16);
let r = malloc(16);
cont

// ↓

let q = p;
let r = malloc(16);
cont
```

The compiler doesn't skip `q` and reuse `p` for `r`, since `q` is already a suitable allocation.

The compiler prefers exact size matches over merely compatible ones. Thus, in the following case, the region pointed to by `p` is reused for `r`:

```neut
free(p, 16);
let q = malloc(8);
let r = malloc(16);
cont

// ↓

let q = malloc(8);
let r = p;
cont
```

### Free-Malloc Canceling and Branching

This optimization works across branches. For example, consider the following:

```neut
// (an `insert` function in bubble sort)
define insert(v: int, xs: int-list) -> int-list {
  match xs {
  | Nil =>
    // ...
  | Cons(y, ys) =>           // (X)
    if gt-int(v, y) {
      Cons(y, insert(v, ys)) // (Y)
    } else {
      Cons(v, Cons(y, ys))   // (Z)
    }
  }
}
```

At point `(X)`, `free` against `xs` is required. However, this `free` can be canceled since suitable `malloc`s can be found in all the reachable branches (here, `(Y)` and `(Z)`). Thus, in the code above, the deallocation of `xs` at `(X)` is removed, and the memory region of `xs` is reused at `(Y)` and `(Z)`, resulting in an in-place update of `xs`.

On the other hand, consider rewriting the code above into something like the following:

```neut
define foo(v: int, xs: int-list) -> int-list {
  match xs {
  | Nil =>
    // ...
  | Cons(y, ys) =>         // (X')
    if gt-int(v, y) {
      Nil                  // (Y')
    } else {
      Cons(v, Cons(y, ys)) // (Z')
    }
  }
}
```

At this point, the `free` against `xs` at `(X')` can't be optimized away since there is a branch (namely, `(Y')`) that doesn't perform a suitable `malloc`.

The same rule is also applied at branch joins. If a later `malloc` appears after a branch, and each reachable branch frees a suitable region before reaching the join, Neut can pass those freed regions through the join and reuse them for that later `malloc`. Unreachable branches do not prevent this optimization.

## Malloc-Free Canceling

Neut also performs the opposite optimization. If a region allocated by `malloc` does not escape and is eventually deallocated by `free`, the compiler replaces that heap allocation with a stack allocation.

As a simple example, consider the following code:

```neut
define foo() -> int {
  let ptr = malloc(8);
  store-int(42, ptr);
  let value = load-int(ptr);
  free(ptr);
  value
}
```

After optimization, this behaves like the following pseudocode:

```neut
define foo() -> int {
  let ptr = alloca(8);
  store-int(42, ptr);
  let value = load-int(ptr);
  value
}
```

That is, the compiler removes the `malloc`/`free` pair and uses a stack slot instead.

This optimization can also work through branch joins. If each reachable branch creates a temporary allocation and the joined result is later freed without escaping, those allocations are candidates for stack allocation.

## Order of Memory Optimizations

Malloc-free canceling is applied before free-malloc canceling. For example, consider the following pseudocode:

```neut
let tmp = malloc(8);
store-int(42, tmp);
let n = load-int(tmp);
free(tmp, 8);
free(old, 16);
let result = malloc(16);
cont
```

Malloc-free canceling first rewrites `tmp` into a stack slot:

```neut
let tmp = alloca(8);
store-int(42, tmp);
let n = load-int(tmp);
free(old, 16);
let result = malloc(16);
cont
```

Then, free-malloc canceling rewrites the later allocation so that `result` uses the region pointed to by `old`:

```neut
let tmp = alloca(8);
store-int(42, tmp);
let n = load-int(tmp);
let result = old;
cont
```

These optimizations are applied within each definition. They do not directly cancel a `malloc` in one definition with a `free` in another definition. This is why destination-passing style can matter: it can move the relevant allocation and deallocation into the same definition.

## Name Resolution

### Resolving Names

A name reference has one of the following forms:

```text
body.path
module.path::source.path::body.path
```

`this` denotes the identity path to the current module:

```neut
and                        // locally available bare name
logic.and                  // dotted name rooted in the local name environment
this::bool::and            // `and` in this module's source/bool.nt
core::bool::and            // `and` in core's source/bool.nt
```

`this` is the written form of an identity path in every chunk. It can be inserted at any point without changing the path:

```text
this.core = core.this = core
this.bool.this = bool
this::this.bool.this::this.and.this = this::bool::and
```

Because `this` denotes identity paths, it is reserved as a source segment, namespace name, definition name, import alias, and local binder.

An identity-only chunk makes the root of that chunk explicit. This distinguishes the following prefix regions:

| Prefix | Region |
| --- | --- |
| `this` | the current module and its dependency paths |
| `this::this` | every source in the current module |
| `this::a.b` | `source/a/b.nt` and sources below `source/a/b/` |
| `this::a.b::this` | only `source/a/b.nt` |
| `this::a.b::ns` | namespace `ns` and its children in `source/a/b.nt` |

The local name environment contains lexical bindings, earlier names in the current file, import aliases, imported namespaces, and namespace views created with `as`.

When compiling a module, the compiler reads the field `dependency` in `module.ens` and uses correspondences like the following:

```neut
// dependency alias => (the digest of the library)
core => "jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o"
foo-module => "JEpjuzZ0rlqxiVuCnD000jEKIA_Y6ku1L3J139h3M6Q"
bar-module => "zptXghmyD5druBl8kx2Qrei6O6fDsKCA7z2KoHp1aqA"
...
```

The compiler then resolves module paths as follows:

```text
core::bool::and

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o::bool::and

--------------

foo-module.bar-module::path.to.some.file::my-function

↓

(the digest of foo-module's public dependency bar-module)::path.to.some.file::my-function

--------------

foo-module::path.to.some.file::my-function

↓

JEpjuzZ0rlqxiVuCnD000jEKIA_Y6ku1L3J139h3M6Q::path.to.some.file::my-function

--------------

...
```

Module path segments after the first one must be dependency aliases of the previous module that are visible from the module where the path is written. Visibility is governed by the prefix-local rule below.

### Prefix-Local Names

An `_`-prefixed segment restricts a name to a required prefix. Take the part before the deepest `_`-prefixed segment and remove the separator immediately before that segment:

| Name | Required prefix | Available from |
| --- | --- | --- |
| `this::a.b::f` | none | anywhere |
| `this::_a.b::f` | `this` | files below `source/` |
| `this::a._b::f` | `this::a` | `source/a.nt` and files below `source/a/` |
| `this::a.b::_f` | `this::a.b` | `source/a/b.nt` and files below `source/a/b/` |
| `this::a.b::ns._f` | `this::a.b::ns` | namespace `ns` and its children in `source/a/b.nt` |

Prefix comparison still respects the `::` boundary. Thus, namespace `ns` in `source/a/b.nt` belongs below `this::a.b::ns`, while `source/a/b/ns.nt` belongs below `this::a.b.ns`; the two remain distinct.

Dependency aliases follow the same `_` rule. A name under `foo._http::client::request` is restricted by `_http`.

## Leading Bars and Trailing Commas

### Comma-Separated Sequences (And-Sequences)

Every comma-separated sequence like `a, b, c` can have a trailing comma like `a, b, c,`.

If a comma-separated sequence has a trailing comma, the sequence is formatted vertically by the built-in formatter.

### Bar-Separated Sequences (Or-Sequences)

Every bar-separated sequence like `a | b | c` can have a leading bar like `| a | b | c`.

If a bar-separated sequence has a leading bar, the sequence is formatted vertically by the built-in formatter.

## Compiler Configuration

The behavior of the compiler can be adjusted using the following environment variables:

| Environment Variable      | Meaning                       |
| ------------------------- | ----------------------------- |
| `NEUT_CLANG`              | the command to call `clang`   |
| `NEUT_CORE_MODULE_DIGEST` | the digest of the core module |
| `NEUT_CORE_MODULE_URL`    | the URL of the core module    |

The default values are as follows:

| Environment Variable      | Default Value                 |
| ------------------------- | ----------------------------- |
| `NEUT_CLANG`              | `clang`                       |
| `NEUT_CORE_MODULE_DIGEST` | (undefined; you must set one) |
| `NEUT_CORE_MODULE_URL`    | (undefined; you must set one) |

## Other Basic Facts

- Neut is call-by-value
- Neut is impure
- The type of `main` must be `() -> unit`
- The compiler has built-in references to names under `core`
- Syntactic constructs like `List::[1, 2, 3]` depend on functions in `core`
