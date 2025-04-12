# Static Memory Management

Here, we'll see how memory is managed in Neut. We'll also see two important optimizations.

## Table of Contents

- [Copying and Discarding Values](#copying-and-discarding-values)
- [Optimization: Reusing Memory](#optimization-reusing-memory)
- [Optimization: Avoiding Unnecessary Copies](#optimization-avoiding-unnecessary-copies)

## Copying and Discarding Values

### Inserting `COPY` and `DISCARD`

In Neut, the content of a variable is copied if the variable is used more than once. For example, consider the following code:

```neut
// before compilation (pseudo code)
define foo(xs: list(int)): list(int) {
  let ys = xs in // using `xs` (1)
  let zs = xs in // using `xs` (2)
  some-func(ys);
  other-func(zs);
  xs // using `xs` (3)
}
```

In the above code, `xs` is used three times. Thus, its content is copied twice:

```neut
// after compilation (pseudo-code)
define foo(xs: list(int)): list(int) {
  let xs1 = COPY(list(int), xs) in
  let xs2 = COPY(list(int), xs) in
  let ys = xs1 in
  let zs = xs2 in
  some-func(ys);
  other-func(zs);
  xs
}
```

Also, the content of a variable is discarded if the variable isn't used. For example, consider the following code:

```neut
// before compilation
define bar(xs: list(int)): unit {
  Unit
}
```

In the above code, `xs` isn't used. Thus, its content is discarded:

```neut
// after compilation (pseudo-code)
define bar(xs: list(int)): unit {
  DISCARD(list(int), xs);
  Unit
}
```

This translation ensures that each variable occurs exactly once, ignoring the arguments to `COPY`. This underpins memory management in Neut.

If you're interested in how Neut implements this translation, please see [How to Execute Types](./how-to-execute-types.md).

### Avoiding Unintentional Copies

To avoid unintentional copies, the compiler requires the `!` prefix on a variable name when a copy is needed. For example, consider the following code:

```neut
define make-pair(t: text): pair(text, text) {
  Pair(t, t)
}
```

The compiler rejects this code because the variable `t` is used twice without the `!` prefix.

You can satisfy the compiler by renaming `t` to `!t`:

```neut
define make-pair(!t: text): pair(text, text) {
  Pair(!t, !t)
}
```

The `!` prefix is unnecessary if the variable can be copied for free. For example, consider the following code:

```neut
define make-pair(x: int): pair(int, int) {
  Pair(x, x)
}
```

The compiler accepts this code since we can "copy" integers for free (by using the same value twice).

## Optimization: Reusing Memory

The compiler exploits Neut's static nature to reuse memory. Consider the following code:

```neut
data int-list {
| Nil
| Cons(int, int-list)
}

// [1, 5, 9] => [2, 6, 10]
define increment(xs: int-list): int-list {
  match xs {
  | Nil =>
    Nil
  | Cons(y, ys) =>
    let foo = add-int(y, 1) in
    let bar = increment(ys) in
    Cons(foo, bar)
  }
}
```

The naive behavior of the `Cons` clause in the `match` would be something like the following:

1. Extract `y` and `ys` from `Cons(y, ys)`
2. `free` the outer tuple of `Cons(y, ys)`
3. Calculate `foo` and `bar`
4. Allocate memory region using `malloc` to represent the tuple of `Cons(foo, bar)`
5. Store the calculated values to the pointer and return it

Now, note that:

- the outer tuple of `Cons(y, ys)` will never be used after extracting its contents, and that
- the outer tuples of `Cons(y, ys)` and `Cons(foo, bar)` have the same size.

Using this knowledge, the compiler translates given code so that it reuses the memory region of `Cons(y, ys)`. More specifically, this `Cons` clause behaves as follows:

1. Obtain `y` and `ys` from `Cons(y, ys)`
2. Calculate `foo` and `bar`
3. Store the calculated values to `Cons(y, ys)`

In other words, when a `free` is required, the compiler looks for a `malloc` in the continuation that is the same size and optimizes away such a pair if one exists. The resulting assembly code thus performs in-place updates.

## Optimization: Avoiding Unnecessary Copies

### Observing Excessive Copies

Suppose we've defined a function `length` as follows:

```neut
define length(xs: list(int)): int {
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
define use-length(!xs: list(int)): unit {
  let len = length(!xs) in
  some-function(len, !xs)
}
```

Note that the variable `!xs` is used twice. This means that the content of `!xs` is copied just to calculate its length. This is of course a tragedy. Worse, this kind of procedure isn't rare. We need some kind of loophole.

Luckily, Neut has a remedy for this kind of situation, as we'll see below.

### Introducing Noema Types

For any type `t`, Neut has a type `&t`. We'll call this type the noema type of `t`. We'll call a term `e` a noema if the type of `e` is a noema type.

Unlike ordinary terms, a noema isn't discarded or copied even when used non-linearly. Also, Neut has primitives to read contents from noemata without consuming them. By utilizing these facts, we can avoid the disaster we have just seen.

Let's see how we can use noemata, rewriting `use-length` and `length`.

### Creating a Noema

We can create a noema using `on`:

```neut
define use-length(xs: list(int)): unit {
  // xs: list(int)
  let len on xs =
    // xs: &list(int)
    length(xs)
  in
  // xs: list(int)
  some-function(len, xs)
}
```

`on` takes a comma-separated list of variables. The variables in the list (`xs` in this example) are cast to noema types in the body of the `let`, and cast back to the original types in the continuation.

Conceptually, `on` can be seen as the following syntax sugar:

```neut
let v on x = e in
cont

// ↓ desugar

let x = unsafe-cast(a, &a, x) in // cast: `a` ~> `&a`
let v = e in                     // (use `&a` in `e`)
let x = unsafe-cast(&a, a, x) in // uncast: `&a` ~> `a`
cont
```


### Using a Noema: Pattern Matching

If `t` is an ADT, you can view the content of a value `e: &t` using `case`:

```neut
define length(xs: &list(int)): int {
  case xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}
```

`case` is similar to `match`. The difference is that, unlike `match`, `case` doesn't perform `free` on its arguments. You can think of `case` as a read-only version of `match`.

Also, note that the newly-bound variables in `case` are wrapped in `&(_)`. In the above code, for example, the type of `ys` is not `list(int)`, but `&list(int)`.

Now, we have new implementations of `length` and `use-length`:

```neut
define length(xs: &list(int)): int {
  case xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, length(ys))
  }
}

define use-length(xs: list(int)): unit {
  let len on xs = length(xs) in
  some-function(len, xs)
}
```

The code doesn't copy `xs` anymore, as you can see from the fact that it doesn't contain `!`.

### Using a Noema: Embodying

Incidentally, you can create a value of type `a` from a value of type `&a`, as follows:

```neut
define make-pair<a>(x: &a): pair(a, a) {
  Pair(*x, *x)
}
```

By writing `*e`, you can copy the content of the noema `e`, keeping the content intact.

<!-- ## Additional Notes -->

<!-- ### Free Variables in a Local Recursion -->

<!-- The `!` prefix is also required when using a free variable in a term-level `define`: -->

<!-- ```neut -->
<!-- define multi-print(!message: text): unit { -->
<!--   let f = -->
<!--     define self(counter: int): unit { -->
<!--       if ge-int(counter, 10) { -->
<!--         Unit -->
<!--       } else { -->
<!--         // `!message` is a free variable of `self` -->
<!--         printf("message: {}\n", [!message]); -->
<!--         self(add-int(counter, 1)) -->
<!--       } -->
<!--     } -->
<!--   in -->
<!--   f(0) -->
<!-- } -->
<!-- ``` -->

<!-- This is because free variables in a term-level `define` are copied during recursion. Seeing how the above code is compiled might be illuminating: -->

<!-- ```neut -->
<!-- // `self` is now closed thanks to the new parameter `!m` (lambda lifting) -->
<!-- define self(counter: int, !m: text): unit { -->
<!--   if ge-int(counter, 10) { -->
<!--     Unit -->
<!--   } else { -->
<!--     // note that `!m` is used twice -->
<!--     printf("message: {}\n", [!m]); -->
<!--     self(add-int(counter, 1), !m) -->
<!--   } -->
<!-- } -->

<!-- define multi-print(!message: text): unit { -->
<!--   let f = -->
<!--     function (counter: int) { -->
<!--       self(counter, !message) -->
<!--     } -->
<!--   in -->
<!--   f(0) -->
<!-- } -->
<!-- ``` -->

<!-- ### Copying Immediate Values -->

<!-- Technically speaking, these discarding/copying operations also happen when the variable is an immediate value like an integer: -->

<!-- ```neut -->
<!-- define buz(x: int): unit { -->
<!--   Unit -->
<!-- } -->

<!-- ↓ -->

<!-- // pseudo-code -->
<!-- define bar(x: int): unit { -->
<!--   let _ = Discard-VALUE(int, x) in -->
<!--   Unit -->
<!-- } -->
<!-- ``` -->

<!-- In practice, however, discarding/copying operations on immediate values are optimized away. -->
