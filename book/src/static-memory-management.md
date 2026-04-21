# Static Memory Management

Here, we'll see how memory is managed in Neut. We'll also see some important optimizations.

## Table of Contents

- [Copying and Discarding Values](#copying-and-discarding-values)
- [Optimization: Free-Malloc Canceling](#optimization-free-malloc-canceling)
- [Optimization: Malloc-Free Canceling](#optimization-malloc-free-canceling)
- [Destination-Passing Style](#destination-passing-style)
- [Optimization: Avoiding Unnecessary Copies](#optimization-avoiding-unnecessary-copies)

## Copying and Discarding Values

### Inserting `COPY` and `DISCARD`

In Neut, the content of a variable is copied if the variable is used more than once. For example, consider the following code:

```neut
// before compilation (pseudo code)
define foo(xs: list(int)) -> list(int) {
  let ys = xs; // using `xs` (1)
  let zs = xs; // using `xs` (2)
  some-func(ys);
  other-func(zs);
  xs // using `xs` (3)
}
```

In the code above, `xs` is used three times. Therefore, its content is copied twice:

```neut
// after compilation (pseudo-code)
define foo(xs: list(int)) -> list(int) {
  let xs1 = COPY(list(int), xs);
  let xs2 = COPY(list(int), xs);
  let ys = xs1;
  let zs = xs2;
  some-func(ys);
  other-func(zs);
  xs
}
```

Also, the content of a variable is discarded if the variable isn't used. For example, consider the following code:

```neut
// before compilation
define bar(xs: list(int)) -> unit {
  Unit
}
```

In the code above, `xs` isn't used. Therefore, its content is discarded:

```neut
// after compilation (pseudo-code)
define bar(xs: list(int)) -> unit {
  DISCARD(list(int), xs);
  Unit
}
```

Ignoring the arguments to `COPY`, this translation ensures that each variable occurs linearly (i.e., exactly once). This forms the basis of memory management in Neut.

If you're interested in how Neut implements this translation, see [On Executing Types](./on-executing-types.md).

### Avoiding Unintentional Copies

To avoid unintentional copies, the compiler requires the `!` prefix on a variable name when a copy is needed. For example, consider the following code:

```neut
// `string` is provided by the core library.
define make-pair(t: string) -> pair(string, string) {
  Pair(t, t)
}
```

The compiler rejects this code because the variable `t` is used twice without being defined with the `!` prefix.

You can satisfy the compiler by defining `t` as `!t`:

```neut
define make-pair(!t: string) -> pair(string, string) {
  Pair(t, t)
}
```

The `!` prefix is unnecessary if the variable can be copied for free. For example, consider the following code:

```neut
define make-pair(x: int) -> pair(int, int) {
  Pair(x, x)
}
```

The compiler accepts this code since we can "copy" integers for free (by reusing the same value).

## Optimization: Free-Malloc Canceling

The compiler exploits Neut's static nature to reuse memory. Consider the following code:

```neut
data int-list {
| Int-Nil
| Int-Cons(int, int-list)
}

// [1, 5, 9] => [2, 6, 10]
define increment(xs: int-list) -> int-list {
  match xs {
  | Int-Nil =>
    Int-Nil
  | Int-Cons(y, ys) =>
    let foo = add-int(y, 1);
    let bar = increment(ys);
    Int-Cons(foo, bar)
  }
}
```

The naive behavior of the `Cons` clause in the `match` would be something like the following:

1. Extract `y` and `ys` from `Cons(y, ys)`
2. `free` the outer tuple of `Cons(y, ys)`
3. Calculate `foo` and `bar`
4. Allocate a memory region using `malloc` to represent the tuple of `Cons(foo, bar)`
5. Store the calculated values to the pointer and return it

Now, note that:

- the outer tuple of `Cons(y, ys)` will never be used after extracting its contents, and that
- the outer tuples of `Cons(y, ys)` and `Cons(foo, bar)` have the same size.

Using this knowledge, the compiler translates the code so that it reuses the memory region of `Cons(y, ys)`. More specifically, this `Cons` clause behaves as follows:

1. Obtain `y` and `ys` from `Cons(y, ys)`
2. Calculate `foo` and `bar`
3. Store the calculated values to `Cons(y, ys)`

In other words, when a `free` is required, the compiler looks for a `malloc` in the continuation that is the same size and optimizes away such a pair if one exists. The resulting assembly code thus performs in-place updates.

## Optimization: Malloc-Free Canceling

Neut also performs the opposite optimization. If a region allocated by `malloc` does not escape and is eventually deallocated by `free`, the compiler replaces that heap allocation with a stack allocation.

For example, consider the following code:

```neut
define foo() -> int {
  let ptr = malloc(8);
  store-int(42, ptr);
  let value = load-int(ptr);
  free(ptr);
  value
}
```

In the code above, the region created by `malloc` never escapes from `foo`. Therefore, the compiler can replace it with a stack slot:

```neut
// after optimization (pseudo-code)
define foo() -> int {
  let ptr = alloca(8);
  store-int(42, ptr);
  load-int(ptr)
}
```

## Destination-Passing Style

Malloc-free canceling is especially useful when combined with destination-passing style.

For example, consider the following code:

```neut
define foo(x: int) -> either(int, bool) {
  if eq-int(x, 0) {
    Left(42)
  } else {
    Right(True)
  }
}

define use-foo() -> unit {
  match foo(10) {
  | Left(x) =>
    cont1
  | Right(y) =>
    cont2
  }
}
```

Since `foo` returns its result using the ordinary arrow `->`, the result has to be allocated inside `foo` itself:

```neut
// after compilation (pseudo-code)
define foo(x: int) -> either(int, bool) {
  if eq-int(x, 0) {
    let tmp = malloc(..);
    // initialize `tmp := Left(42)`
    tmp
  } else {
    let tmp = malloc(..);
    // initialize `tmp := Right(True)`
    tmp
  }
}

define use-foo() -> unit {
  let buf = foo(10);
  match tag(buf) {
  | 0 =>
    let x = extract-from-left(buf);
    free(buf);
    cont1
  | _ =>
    let y = extract-from-right(buf);
    free(buf);
    cont2
  }
}
```

In this form, malloc-free canceling does not help much. The memory region created in `foo` escapes from `foo`, so `foo` itself cannot replace that allocation with a stack slot.

To improve this situation, Neut provides destination-passing style. We write such functions using `->>` (and `=>>` for anonymous functions). A function written in this style can still be called in the usual way, but its compiled code receives the result destination from the caller.

Rewriting `foo` in this style, we get:

```neut
define foo(x: int) ->> either(int, bool) {
  if eq-int(x, 0) {
    Left(42)
  } else {
    Right(True)
  }
}

define use-foo() -> unit {
  match foo(10) {
  | Left(x) =>
    cont1
  | Right(y) =>
    cont2
  }
}
```

This behaves roughly as follows after compilation:

```neut
// after compilation (pseudo-code)
define foo(dest: pointer, x: int) -> void {
  if eq-int(x, 0) {
    let tmp = malloc(..);
    // initialize `tmp := Left(42)`
    copy(dest, tmp);
    free(tmp)
  } else {
    let tmp = malloc(..);
    // initialize `tmp := Right(True)`
    copy(dest, tmp);
    free(tmp)
  }
}

define use-foo() -> unit {
  let buf = malloc(..);
  foo(buf, 10);
  match tag(buf) {
  | 0 =>
    let x = extract-from-left(buf);
    free(buf);
    cont1
  | _ =>
    let y = extract-from-right(buf);
    free(buf);
    cont2
  }
}
```

Now the crucial `malloc`/`free` pairs no longer cross the function boundary. Because of that, malloc-free canceling can optimize them away:

```neut
// after malloc-free canceling (pseudo-code)
define foo(dest: pointer, x: int) -> void {
  if eq-int(x, 0) {
    let tmp = alloca(..);
    // initialize `tmp := Left(42)`
    copy(dest, tmp)
  } else {
    let tmp = alloca(..);
    // initialize `tmp := Right(True)`
    copy(dest, tmp)
  }
}

define use-foo() -> unit {
  let buf = alloca(..);
  foo(buf, 10);
  match tag(buf) {
  | 0 =>
    let x = extract-from-left(buf);
    cont1
  | _ =>
    let y = extract-from-right(buf);
    cont2
  }
}
```

In this way, destination-passing style turns a returned value into caller-managed storage, making malloc-free canceling effective in cases where the ordinary arrow `->` would force a heap allocation.

<div class="info-block">

For simplicity, the code above keeps `tmp = alloca(..)` and `copy(dest, tmp)` separate. In practice, LLVM often optimizes this into ordinary assignments.

</div>

## Optimization: Avoiding Unnecessary Copies

### Observing Excessive Copies

Suppose we've defined a function `length` as follows:

```neut
define length(xs: list(int)) -> int {
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}
```

Now, consider the following code:

```neut
define use-length(!xs: list(int)) -> unit {
  let len = length(xs);
  some-function(len, xs)
}
```

Note that the variable `xs` is used twice. This means that the content of `xs` is copied just to calculate its length. This is wasteful, and worse, this pattern isn't rare. We need a way to avoid it.

### Introducing Noema Types

Fortunately, there is a remedy for this kind of situation.

For any type `t`, Neut has a type `&t`. We'll call this type the noema type of `t`. We'll call a term `e` a noema if the type of `e` is a noema type.

Unlike ordinary terms, a noema isn't discarded or copied even when used non-linearly. Also, Neut has primitives to read contents from noemata without consuming them. By using these facts, we can avoid the problem above.

Let's see how we can use noemata, rewriting `use-length` and `length`.

### Creating a Noema

We can create a noema using `on`:

```neut
define use-length(xs: list(int)) -> unit {
  // xs: list(int)
  let len on xs =
    // xs: &list(int)
    length(xs);
  // xs: list(int)
  some-function(len, xs)
}
```

`on` takes a comma-separated list of variables. The variables in the list (`xs` in this example) are cast to noema types in the body of the `let`, and cast back to the original types in the continuation.

Conceptually, `on` can be seen as the following syntactic sugar:

```neut
let v on x = e;
cont

// ↓ desugar

let x = unsafe-cast(a, &a, x); // cast: `a` ~> `&a`
let v = e;                     // (use `&a` in `e`)
let x = unsafe-cast(&a, a, x); // uncast: `&a` ~> `a`
cont
```

`on` has to satisfy a certain condition. Consider the following code:

```neut
// xs: list(int)
// ...
let ys on xs = xs;
let _ = xs; // (*)
cont
```

Since `xs` is discarded at `(*)`, using `ys` in `cont` should result in use-after-free. To prevent this kind of behavior, the compiler rejects code that might contain any noema in the result of `on`. In this case, since the type of `ys` is `&list(int)`, the compiler rejects this code.

<div class="info-block">

This condition may look a bit artificial at first. On the next page, however, we'll see that it can be understood via modal logic.

</div>

### Using a Noema: Pattern Matching

If `t` is an ADT, you can view the content of a value `e: &t` using `case`:

```neut
define length(xs: &list(int)) -> int {
  case xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}
```

`case` is similar to `match`. The difference is that, unlike `match`, `case` doesn't perform `free` on its arguments. You can think of `case` as a read-only version of `match`.

Also, note that the newly-bound variables in `case` are wrapped in `&(_)`. In the code above, for example, the type of `ys` is not `list(int)`, but `&list(int)`.

Now, we have new implementations of `length` and `use-length`:

```neut
define length(xs: &list(int)) -> int {
  case xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}

define use-length(xs: list(int)) -> unit {
  let len on xs = length(xs);
  some-function(len, xs)
}
```

The code doesn't copy `xs` anymore, as you can see from the fact that it no longer contains `!`.

### Using a Noema: Embodying

You can also create a value of type `a` from a value of type `&a`, as follows:

```neut
define make-pair<a>(x: &a) -> pair(a, a) {
  Pair(*x, *x)
}
```

By writing `*e`, you can copy the content of the noema `e`, keeping the content intact.
