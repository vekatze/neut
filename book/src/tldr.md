# Lorem Ipsum

Neut is an experimental programming language.

An experiment to reconcile the *λ-calculus* with *static memory management*.

## How does the Language Look Like?

```neut
// algebraic data types
enum list(a: tau) {
- nil()
- cons(a, list(a))
}

define length-with-nonsense<a>(xs: list(a)): i64 {
  let s = some-function(foo)
  let other-function =
    lambda(x: A, y: B, z: c) {
      do-something(x, y, z)
    }
  print("hello")
  match xs {
  - nil() =>
    0
  - cons(y, ys) =>
    add-i64(y, length-with-nonsense(ys))
  }
}

// &list(A) is a "noetic" type, you'll see later
define tail<a>(xs: &list(a)): option(&list(a)) {
  case xs {
  - nil() =>
    none()
  - cons(y, ys) =>
    some(ys)
  }
}
```

## Installation?

```sh
wget http://github.com/neut-mac-x86_64 ~/.local/bin/
```

```sh
wget http://github.com/neut-linux-x86_64 ~/.local/bin/
```

## How is it interesting?

The following two key features should make it interesting:

- Static memory management 🌟
- Lambda calculus without limitations 🌟

## Static Memory Management, but How?

*Neut translates a type into a function* that knows how to copy/discard the terms of the type.

If you need more, see the [next chapter](/chapter_1.html). You can also press the right arrow key.

## Just Show Me How to Use It

See this tutorial.

## Then... What are Drawbacks?

See here. Spoiler: I need your help.
