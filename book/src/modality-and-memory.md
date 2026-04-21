# Modality and Memory

Here, we'll see how to interact with the box modality `+`, which provides a way to work with layers in Neut. We'll then see that both `on` and `*e` can be understood as syntactic sugar over this modality.

## Table of Contents

- [Layers and the Box Modality](#layers-and-the-box-modality)
- [More Tools for Boxes](#more-tools-for-boxes)
- [Desugaring the Two Operations](#desugaring-the-two-operations)
- [Additional Notes](#additional-notes)

## Layers and the Box Modality

In Neut, each type `a` has a corresponding type `+a`. This type provides a way to work with _layers_, which are similar to lifetimes in other languages.

Below, we'll first introduce the concept of layers, and then see how to use `+a`.

### Layers and Variables

Every term in Neut has an integer called a layer. Conceptually, a layer can be seen as the level at which a piece of data lives.

The body of a `define` starts at layer 0:

```neut
define foo() -> () -> unit {
  // here is layer 0
  () => {
    // here is also layer 0
    Unit
  }
}
```

A variable defined at layer n can only be used at the same layer. For example, the following code is invalid because the variable `x` is defined at layer 0 but used at layer 3:

```neut
define bar() -> unit {
  // here is layer 0
  let x = Unit; // ← `x` is defined at layer 0

  ... // ← some layer operations here

  // layer 3 (for example)
  let v2 =
    x; // ← Error: `x` is used at layer 3 (≠ 0)
  ...
}

```

Only modality-related operations can change layers, as we'll see below.

### Creating Boxes

To create a term of type `+a`, use `box`:

```neut
define use-box(x: &int, y: &bool, z: &string) -> +pair(int, bool) {
  // here is layer 0
  // free variables:
  // - x: &int
  // - y: &bool
  // - z: &string
  box x, y {
    // here is layer -1 (== layer(outer) - 1)
    // free variables:
    // - x: int
    // - y: bool
    // - (z is unavailable here because of layer mismatch)
    Pair(x, y)
  }
}
```

Some notes on `box`:

- The type of `xi` in `box x1, ..., xn {e}` must be of the form `&ai`.
- Given `xi: &ai`, the type of `xi` in the body of `box` is `ai`.

`box` behaves as follows:

```neut
box x1, ..., xn { e }

↓ // (compile)

let x1 = COPY(a1, x1);
...
let xn = COPY(an, xn);
e
```

You can omit the sequence `x1, ..., xn` entirely if no variables need to be copied.

### Using Boxes

To use a term of type `+a`, use `letbox`:

```neut
// `string` is provided by the core library.
define use-letbox(x: int, y: bool, z: string) -> int {
  // here is layer 0
  // free variables:
  // - x: int
  // - y: bool
  // - z: string
  letbox extracted-value = {
    // here is layer 1 (== layer(outer) + 1)
    // (x, y, and z are unavailable here because of layer mismatch)
    box {
      // here is layer 0
      // free variables:
      // - x: int
      // - y: bool
      // - z: string
      x
    }
  };
  // here is layer 0
  // free variables:
  // - x: int
  // - y: bool
  // - z: string
  extracted-value // == x
}

```

Operationally, `letbox` is the same as `let`:

```neut
letbox v = e1;
e2

↓ // (compile)

let v  = e1;
e2
```

## More Tools for Boxes

### Using Boxes Without Changing the Current Layer

Sometimes you want to use a term of type `+a` without shifting your current layer. For this, Neut provides `letbox-T`, which keeps you in the same layer:

```neut
define use-letbox-T(x: int, y: bool) -> int {
  // here is layer 0
  letbox-T value on x, y = {
    // here is layer 0 (== layer(outer))
    box {42}
  };
  // here is layer 0
  value // == 42
}
```

`letbox-T` can, for example, be used to write functions of type `(+a) -> a` as follows:

```neut
define axiom-T<a>(x: +a) -> a {
  letbox-T tmp = x;
  tmp
}
```

If you tried to use `letbox` instead, you'd get an error because it would result in a layer mismatch.

### A Shortcut for Creating Boxes

We can, for example, construct a `+bool` from a `bool` as follows:

```neut
define box-bool(b: bool) -> +bool {
  match b {
  | True  => box {True}
  | False => box {False}
  }
}
```

To streamline this kind of mechanical step, Neut provides `lift`:

```neut
define box-bool(b: bool) -> +bool {
  lift {b} // `lift` casts `bool` into `+bool`
}
```

Not all types can be cast using `lift`. Specifically, it can't be used on any type that contains:

- a type of the form `&a`
- a type of the form `(a1, ..., an) -> b`
- a type variable
- an ADT that can contain any of the above

If you can get `+t` by lifting `e: t`, you can get the same type using `box` instead. In this sense, `lift` is a shortcut for creating boxes.

## Desugaring the Two Operations

We've seen the two constructs `on` and `*e`. Although they may look a bit artificial at first, they are in fact straightforward applications of the box modality.

### Desugar: Borrowing

We can now desugar `on` as follows:

```neut
let x on y, z = e1;
e2

↓ // desugar

letbox-T x on y, z = lift {e1};
e2
```

This explains why the result type of `on` has to be restricted to some extent: the restriction comes from `lift`.

### Desugar: Embodying

Using the `axiom-T` we defined above, we can also desugar `*e` as follows:

```neut
*e

↓ // desugar

let x = e;
axiom-T(box x {x})
```

## Additional Notes

### Layers and Free Variables

There is one more rule that must be satisfied for memory safety. If a function is defined at layer `n`, then any free variable `x` in the function must satisfy `layer(x) <= n`.

Without this rule, you could do something like the following:

```neut
define joker() -> () -> unit {
  // layer 0
  let xs: list(int) = make-list();
  letbox-T f on xs = {
    // layer 0
    box {
      // layer -1
      () => { // ★
        letbox k = {
          // layer 0
          let len = length(xs);
          box {Unit}
        };
        Unit
      }
    }
  };
  f // function with xs: &list(int) as a free variable
  // FREE(xs)
}

define main() -> unit {
  let f = joker();
  f(); // xs used after freed here
}
```

This example would wrongly allow a function at layer 0 (`★`) to keep a reference to data (`xs`) that, after the outer `letbox` completes, could be deallocated, leading to a use-after-free scenario in the body of the main function. Hence, Neut's layer rules prohibit capturing a higher-layer variable in a lower-layer function.

### A More Concrete Example: `+` and Callbacks

Compared with `&`, it may be a little less obvious when `+` becomes useful. One such case is the following helper, which reads bytes from a file and passes them to a decoding function:

```neut
data error {
| Error(message: string)
}

// `f` only inspects the input, so it takes `&binary`, not `binary`
define decode-from-file<a>(f: (&binary) -> +either(error, a)) -> either(error, a) {
  let bytes = read-from-file("path/to/file");
  letbox-T result on bytes = f(bytes);
  result
}
```

Here, `+` lets the function passed to `decode-from-file` compute a result from borrowed input while still making that result available safely on the outer layer.

Without `+`, one might try to write the following:

```neut
define keep-bytes(arg: &binary) -> either(error, &binary) {
  Right(arg)
}

define decode-from-file<a>(f: (&binary) -> either(error, a)) -> either(error, a) {
  let bytes = read-from-file("path/to/file");
  let result on bytes = f(bytes);
  // `bytes` is freed here
  result
}

define main() -> unit {
  let result = decode-from-file(keep-bytes);
  match result {
  | Left(_) =>
    Unit
  | Right(bytes) =>
    write-to-file("path/to/out", bin-to-hex(bytes))
  }
}
```

This won't compile because `let result on bytes = ...` is desugared using `lift`, and `lift` does not allow result types that still mention a free type variable such as `a`.

If it did compile, the following would happen inside `main`:

1. `decode-from-file(keep-bytes)` evaluates to `Right(bytes)`, where `bytes` is a reference to the local variable `bytes` in `decode-from-file`
2. `decode-from-file` returns and frees that local variable
3. `main` uses the dangling reference, causing a use-after-free

Thus, the version without `+` doesn't work.

The `+` in the result type asserts that the value produced by `f` remains valid on the outer layer. In this way, `+` lets us write a borrowing-based API without forcing the callback result to stay trapped inside the borrowing scope.
