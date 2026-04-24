# Modal Metaprogramming

Here, we'll see how to interact with the code type `'a`, which enables metaprogramming.

## Table of Contents

- [Stages and the Code Modality](#stages-and-the-code-modality)
- [Top-Level Meta Functions](#top-level-meta-functions)
- [Compile-Time Primitives](#compile-time-primitives)
- [More Tools for Code](#more-tools-for-code)

## Stages and the Code Modality

Neut provides primitives that can only be used at compile time. By using them, we can perform special computations that are not available in ordinary runtime code.

To describe such compile-time parts of a program, Neut has the code type `'a` and the concept of _stages_. They can be understood in much the same way as `+a` and layers.

### Creating and Using Code

Given a term `e: a`, we can construct a term `quote {e}: 'a` as follows:

```neut
// stage n + 1
quote {
  // stage n
  e
}
```

Conversely, given a term `e: 'a`, we can construct a term `unquote {e}: a` as follows:

```neut
// stage n
unquote {
  // stage n + 1
  e
}
```

Every term in Neut has an integer called a stage. A variable defined at stage n can only be used at the same stage. For example, the following code is invalid because the variable `x` is defined at stage 0 but used at stage -1:

```neut
define bar() -> 'int {
  // here is stage 0 (The body of a `define` starts at stage 0)
  let x = 42; // ← `x` is defined at stage 0
  quote {
    // here is stage -1
    x // ← Error: `x` is used at stage -1 (≠ 0)
  }
}
```

You can use `quote` and `unquote` as follows:

```neut
define use-code-1() -> int {
  // stage 0
  unquote {
    // stage 1
    quote {
      // stage 0
      10
    }
  }
}

define use-code-2(x: 'int) -> 'int {
  // stage 0
  quote {
    // stage -1
    unquote {
      // stage 0
      x
    }
  }
}
```

## Top-Level Meta Functions

The language has two statements for top-level meta functions: `inline-meta` and `define-meta`.

### `inline-meta`

`inline-meta` is the simpler one. This is basically the same as `inline` except that it starts at stage 1, not 0:

```neut
inline-meta duplicate(x: 'int) -> 'pair(int, int) {
  // stage 1
  quote {
    // stage 0
    let y = unquote {x};
    Pair(y, y)
  }
}
```

You can also define recursive functions using `inline-meta`:

```neut
inline-meta calculate-length<a>(xs: list(a)) -> int {
  match xs {
  | Nil =>
    0
  | Cons(_, ys) =>
    add-int(1, calculate-length(ys))
  }
}
```

The body of `inline-meta` is reduced only when the body of `define` or `inline` is evaluated.

### `define-meta`

`define-meta` is useful when generating code recursively.

A typical situation is that a meta function inspects a type at compile time and generates code by recursively handling its component types (pseudocode):

```neut
define-meta print-data<a>(x: 'a) -> 'unit {
  match magic inspect-type(a) {
  | List(t) =>
    quote {
      ..
      print-data(unquote {..}) // recursively generate code for `t`
      print-data(unquote {..}) // recursively generate code for `list(t)`
      ..
    }
  | _ =>
    ..
  }
}
```

Naive compile-time evaluation may diverge in such a situation: while generating code for some type, the meta function may need to generate code for the same type again.

`define-meta` avoids this by first specializing itself to its type arguments and memoizing the result. This memoization is performed on a per-file basis. When the same type arguments appear again, the memoized function is reused.

To make this work, `define-meta` requires every explicit parameter to have a code type:

```neut
// valid
define-meta good<a>(x: 'a) -> 'int {
  ..
}

// invalid
define-meta bad<a>(x: a) -> 'int {
  ..
}
```

Like `inline-meta`, its body starts at stage 1.

### Calling Meta Functions

You can call a meta function by writing `foo::(bar, baz)`. This is the same as:

```neut
unquote {foo(quote {bar}, quote {baz})}
```

For example, the following two definitions mean the same thing:

```neut
define use-meta() -> pair(int, int) {
  duplicate::(10)
}

define use-meta() -> pair(int, int) {
  unquote {duplicate(quote {10})}
}
```

## Compile-Time Primitives

Neut provides some compile-time primitives that can only be used at stage 1 or above, including:

- `magic inspect-type(a)`, which inspects the type `a` and returns a structured value
- `magic compile-error(message)`, which immediately causes a compile-time error

Using these primitives, we can write, for example, functions that behave differently depending on the given types, which would be hard to achieve in ordinary runtime code.

For example, consider the following meta function:

```neut
define-meta eq-simple<a>(x: '&a, y: '&a) -> 'bool {
  let k = magic inspect-type(a);
  match k {
  | Int64 =>
    quote {
      let x = magic cast(&a, int64, unquote {x});
      let y = magic cast(&a, int64, unquote {y});
      eq-int64(x, y)
    }
  | Rune =>
    quote {
      let x = magic cast(&a, rune, unquote {x});
      let y = magic cast(&a, rune, unquote {y});
      eq-rune(x, y)
    }
  | _ =>
    magic compile-error("eq-simple doesn't support the given type")
  }
}
```

Here, `eq-simple` inspects `a` at compile time and chooses the implementation accordingly.

Thus, when we write

```neut
define use-eq(x: &int, y: &int) -> bool {
  eq-simple::(x, y)
}
```

the compiler can generate integer equality specialized to `int`, rather than a generic runtime dispatcher.

## More Tools for Code

### Embedding Compile-Time Results into Code

Sometimes you want to embed a compile-time calculation result into code without changing the current stage. For this, Neut provides `promote`:

```neut
define-meta make-message<a>() -> 'unit {
  let t = magic show-type(a);
  quote {
    print(unquote {promote {t}});
    Unit
  }
}
```

`promote` is a variant of `quote`. The difference lies in the fact that `promote` doesn't shift stages:

```neut
// stage n
promote {
  // stage n
  ..
}
```

`promote` is useful when a compile-time primitive returns a value that should later appear in runtime code.
