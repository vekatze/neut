# Static Memory Management

The following two mechanisms underpin memory management in Neut:

- The compiler translates given code so that each variable is used exactly once
- The compiler ensures that each variable's content is deallocated after it is used

Below, we'll see how both of them work. After that, we'll also see some important optimizations.

## Table of Contents

- [Linearizing Variable Occurrences](#variables-and-memory)
- [Consuming Values](#consuming-values)
- [Optimization: Avoiding Unnecessary Copies](#optimization-avoiding-unnecessary-copies)
- [Optimization: Reusing Memory](#optimization-reusing-memory)
- [Additional Notes](#parallel-computation)

## Linearizing Variable Occurrences

### Copying and Discarding Variables

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

In the above code, `xs` is used three times, so its content is copied twice:

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

In the above code, `xs` isn't used, so its content is discarded:

```neut
// after compilation (pseudo-code)
define bar(xs: list(int)): unit {
  DISCARD(list(int), xs);
  Unit
}
```

This translation ensures that each variable is used exactly once (excluding the arguments to `COPY`).

If you're interested in how Neut implements this translation, please see [How to Execute Types](./how-to-execute-types.md).

### Avoiding Unintentional Copies

To avoid unintentional copies, the compiler requires the `!` prefix on a variable name when a copy is needed. For example, consider the following code:

```neut
define make-pair(xs: list(int)): pair(list(int), list(int)) {
  Pair(xs, xs)
}
```

The compiler rejects this code because `xs` is used twice without the `!` prefix.

You can satisfy the compiler by renaming `xs` to `!xs`:

```neut
define make-pair(!xs: list(int)): pair(list(int), list(int)) {
  Pair(!xs, !xs)
}
```

## Consuming Values

In Neut, any consumed part of each variable's contents is deallocated after the variable is used. For example, consider the following code:

```neut
define foo(value: either(int, int)): int {
  match xs {
  | Left(x) =>
    x
  | Right(y) =>
    y
  }
}
```

The `match` in the code uses the variable `xs`.


Now, suppose we pass, say, `Left(1)` to `foo`.

(..)


## Optimization: Avoiding Unnecessary Copies

### The Tragedy of Excessive Copying

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

Then, consider the following code:

```neut
define use-length(!xs: list(int)): unit {
  let len = length(!xs) in // use `length` to calculate the length of `!xs`
  some-function(len, !xs)  // .. then use `len` and `!xs`
}
```

Note that the variable `!xs` is used twice. Thus, that the content of `!xs` is copied just to calculate its length. This is of course a tragedy.

Luckily, Neut has a remedy for this kind of situation, as we'll see below.

### Introducing Noema Types

For any type `t`, Neut has a type `&t`. We'll call this the noema type of `t`. We'll call a term `e` a noema if the type of `e` is a noema type.

Unlike ordinary terms, _a noema isn't discarded or copied even when used non-linearly_. By exploiting this behavior, we can avoid the disaster we have just seen.

### Creating a Noema

Let's see how noemata can be used to avoid excessive copying. Firstly, we create a noema using `let-on`.

```neut
define use-length(xs: list(int)): unit {
  let len on xs =
    // xs: &list(int)
    length(xs)
  in
  // xs: list(int)
  some-function(len, xs)
}
```

`on` takes a comma-separated list of variables. Variables specified there are then cast to a noema in the body of the `let` and cast back to non-noetic values in its continuation.

`let-on` is conceptually the following syntax sugar:

```neut
let result on x = e in
cont

// ↓ desugar

let x = unsafe-cast(a, &a, x) in // cast: `a` ~> `&a`
let result = e in                // (use `&a`)
let x = unsafe-cast(&a, a, x) in // uncast: `&a` ~> `a`
cont
```


### Using a Noema: Pattern Matching

Let's see how it works. We first redefine `length`. If the type `t` is an ADT type, you can inspect its content using `case`:

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

The main difference between `case` and `match` is that `case` doesn't perform `free` against its arguments. Because of that, this new `length` doesn't consume `xs`.

Also, note that the newly-bound variables in `case` are automatically wrapped with `&(_)`. For example, in the above example, the type of `ys` is not `list(int)`, but `&list(int)`.

The `use-length` then becomes as follows:

```neut
define use-length(xs: list(int)): unit {
  let len on xs = length(xs) in
  some-function(len, xs)
}
```

Now the content of `xs` isn't copied anymore.

### Using a Noema: Embodying

You can also create a value of type `a` from a value of type `&a`, as follows:

```neut
define make-pair-from-noema<a>(x: &a): pair(a, a) {
  Pair(*x, *x)
}
```

By writing `*e`, you can copy the content of the noema `e` along the type `a`, keeping the content intact.

## Optimization: Reusing Memory

Let's see another aspect of Neut's memory management. The compiler can sometimes optimize away memory allocation thanks to its static nature. Consider the following code:

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
  | Cons(x, rest) => // ← "the `Cons` clause"
    let foo = add-int(x, 1) in
    let bar = increment(rest) in
    Cons(foo, bar)
  }
}
```

The expected behavior of the `Cons` clause would be something like the following:

1. obtain `x` and `rest` from `xs`
2. `free` the outer tuple of `xs`
3. calculate `foo (= add-int(x, 1))` and `bar (= increment(rest))`
4. allocate memory region using `malloc` to represent `Cons(foo, bar)`
5. store the calculated values to the pointer and return it

However, the compiler knows the following two facts during compilation:

- The size of outer tuples of `Cons(x, rest)` and `Cons(foo, bar)` are the same
- The outer tuple of `Cons(x, rest)` will never be used after extracting its contents

Thanks to this knowledge, the compiler can optimize away a pair of `free` and `malloc`, as follows:

1. obtain `x` and `rest` from `xs`
2. calculate `foo (= add-int(x, 1))` and `bar (= increment(rest))`
3. store the calculated values to `xs` (overwrite)

When a `free` is required, the compiler looks for a `malloc` in the continuation that is the same size and optimizes away such a pair if one exists. The resulting assembly code thus performs in-place updates.

## Additional Notes

### Free Variables in a Local Recursion

The `!` is also required when using a free variable in a term-level `define`:

```neut
define multi-print(!message: text): unit {
  let f =
    define self(counter: int): unit {
      if ge-int(counter, 10) {
        Unit
      } else {
        // `!message` is a free variable of `self`
        printf("message: {}\n", [!message]);
        self(add-int(counter, 1))
      }
    }
  in
  f(0)
}
```

This is because free variables in a term-level `define` are copied during recursion. Seeing how the above code is compiled might be illuminating:

```neut
// `self` is now closed thanks to the new parameter `!m` (lambda lifting)
define self(counter: int, !m: text): unit {
  if ge-int(counter, 10) {
    Unit
  } else {
    // 💫 note that `!m` is used twice
    printf("message: {}\n", [!m]);
    self(add-int(counter, 1), !m)
  }
}

define multi-print(!message: text): unit {
  let f =
    function (counter: int) {
      self(counter, !message)
    }
  in
  f(0)
}
```

### Copying Immediate Values

Technically speaking, these discarding/copying operations also happen when the variable is an immediate value like an integer:

```neut
define buz(x: int): unit {
  Unit
}

↓


// pseudo-code
define bar(x: int): unit {
  let _ = DISCARD-VALUE(int, x) in
  Unit
}
```

In practice, however, discarding/copying operations on immediate values are optimized away.

### Copying Values For Free

The prefix `!` is unnecessary if the variable can be copied for free. For example, the following code will typecheck:

```neut
define make-pair(x: int): pair(int, int) {
  Pair(x, x)
}
```

because we can "copy" integers for free (by simply using the same `x` twice).
