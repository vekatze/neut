# Noetic Programming

In this section, we'll see how to avoid undesirable copying/discarding. We'll do something like borrowing in other languages.

## The Type of a Noema

For any type `A` in Neut, you can construct a new type `&A`, the noema type of `A`. We'll call a value a noema if the type of the value is of the form `&A`. The internal representaiton of a noema of type `&A` is exactly the same as a value with type `A`.

The point of a noema lies in the fact that *a noema isn't consumed even after it is used*. Let's see how it behaves.

## Creating a Noema

A noema can be created using `let-on`:

```neut
define sample(xs: list(a)): i64 {
  let result on xs = {
    let foo = xs // foo: &list(a)
    1
  }
  let bar = xs // foo: list(a)
  2
}
```

In the `body` of `let result on x1, ..., xn = body`, the type of `xi` is casted into its noetic variant. Using this, we can introduce a noema.

The `result` cannot depend on any noetic value (this is statically checked by the compiler); You can therefore use noetic variant of `xi` only in `body`. For example, the following code should result in an error:

```neut
  let result on xs = {
    let foo = xs // foo: &list(a)
    foo // error: a term of the following type might be noetic: &list(a)
  }
  ...
```

## What's the Point of a Noema?

A noema isn't consumed even after it is used. Let's take the following code for example:

```neut
  let result on xs = {
    let foo = xs // foo: &list(a)
    let buz = xs // foo: &list(a)
    1
  }
  ...cont...
```

The code above clearly uses `xs` twice in the body of `let-on`. However, since the type of `xs` noetic, the `xs` isn't copied. The same `xs` is used in both of the occurrences, and then the `xs` is, for example, discarded in `...cont...`.

By using a noema, we can something like "borrowing" in Neut.

## Using a Noema: Inspection

The content of a noema can be inspected, using `case`:

```neut
define length[a](xs: &list(a)): i64 {
  case xs {
  - [] =>
    0
  - y :< ys =>
    add-i64(1, length(ys))
  }
}
```

The `case` is the noetic variant of `match`; It inspects a variant value without consuming it.

The variables that are bound by a `case` is casted to be noetic. For example, the `y` in the example above is not of type `a`, but of type `&a`. The `ys` is not of type `list(a)`, but of type `&list(a)`. Since the type of `y` is noetic, the `y` isn't discarded even though it isn't used.

## Using a Noema: Incarnation

You can incarnate a noema using `!e`:

```neut
define sum-of-list(xs: &list(i64)): i64 {
  case xs {
  - [] =>
    0
  - y :< ys =>
    add-i64(!y, sum-of-list(ys)) // using !e
  }
}
```

`!e` copies a noema along its inner type. For example, the `y` above is of type `&i64`. Thus, `!y` creates a new copy of `i64` by copying the noema `y` along its inner type `i64`.

## The Type of a Static Text

Since a noema isn't copied/discarded, it can be exploited to represent a static text. In Neut, the type of a static text is `&text`:

```neut
"hello, world": &text
```

Some text operations are defined on this noetic type. For example, the core library defines `print: &text -> top`. Thus the following is a well-typed code:

```neut
define test(): top {
  print("hello, world!\n")
}
```

By defining functions not on physical types but on noetic types, some operations can be realized in an efficient manner.
