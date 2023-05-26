# Noetic Programming

In this section, we'll see how to avoid undesirable copying/discarding. We'll do something like borrowing in other languages.

## The Type of a Noema

For any type `A` in Neut, you can construct a new type `&A`, the noema type of `A`. We'll call a value a noema if the type of the value is of the form `&A`.

The internal representation of a noema of type `&A` is the same as a value with type `A`.

The point of a noema lies in the fact that *a noema isn't consumed even after it is used*. Let's see how it behaves.

## Creating a Noema

A noema can be created using `let-on`:

```neut
define sample(xs: list(a)): int {
  let result on xs = {
    let foo = xs // foo: &list(a)
    1
  }
  let bar = xs // foo: list(a)
  2
}
```

In the `body` of `let result on x1, ..., xn = body`, the type of `xi` is cast into its noetic version. Using this, we can introduce a noema.

For memory safety, the `result` cannot contain any noetic value (this is statically checked by the compiler); You can therefore use the noetic version of `xi` only in the `body`. For example, the following code should result in an error:

```neut
  let result on xs = {
    let foo = xs // foo: &list(a)
    foo // error: a term of the following type might be noetic: &list(a)
  }
  ...
```

## Avoiding Unnecessary Copying

A noema isn't consumed even after it is used. Let's take the following code for example:

```neut
  let result on xs = {
    let foo = xs // foo: &list(a)
    let buz = xs // foo: &list(a)
    1
  }
  cont
```

The code above clearly uses `xs` twice in the body of `let-on`. However, since the type of `xs` is noetic, the `xs` isn't copied. The same `xs` is used in both of the occurrences and then the `xs` is, for example, discarded in `cont`.

By using a noema, we can perform something like "borrowing" in other languages.

## Using a Noema: Viewing Its Contents

The content of a noema can be viewed, using `case`:

```neut
define length[a](xs: &list(a)): int {
  case xs {
  - [] =>
    0
  - y :: ys =>
    add-int(1, length(ys))
  }
}
```

Here, the `case` is the noetic variant of `match`; It does the same as `match` except that it doesn't free the outer tuple of given value (`xs` in this case).

The variables that are bound by a `case` are cast to be noetic. For example, the `y` in the example above is not of type `a`, but of type `&a`. The `ys` is not of type `list(a)`, but of type `&list(a)`. Since the type of `y` is noetic, `y` isn't discarded even though it isn't used.

## Using a Noema: Actualizing

You can actualize a noema using `*e`:

```neut
define sum-of-list(xs: &list(int)): int {
  case xs {
  - [] =>
    0
  - y :: ys =>
    add-int(*y, sum-of-list(ys)) // using *e
  }
}
```

`*e` copies a noema along its inner type. For example, since the `y` above is of type `&int`, `*y` is a `y`'s new copy of type `int`.

## The Type of a Static Text

The fact that a noema isn't copied/discarded can be exploited to represent a static text. In Neut, the type of a static text is `&text`:

```neut
"Hello world!\n": &text
```

Some text operations are defined on this noetic type. For example, the core library defines `print: &text -> top`. Thus the following is a well-typed code:

```neut
define test(): top {
  print("Hello world!\n")
}
```

Yes, hello world.
