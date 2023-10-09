# Noetic Programming

In this section, we'll see how to avoid undesirable copying/discarding. We'll do something like borrowing in other languages.

## The Type of a Noema

For any type `a` in Neut, you can construct a new type `&a`, the noema type of `a`. We'll call a value a noema if the type of the value is of the form `&a`.

The internal representation of a noema of type `&a` is the same as a value with type `a`.

The point of a noema lies in the fact that *a noema isn't copied/discarded even when used non-linearly*. Let's see how it behaves.

## Creating a Noema

A noema can be created using `let-on`:

```neut
define sample(xs: list(a)): int {
  let result on xs =
    let foo = xs in // foo: &list(a)
    1
  in
  let bar = xs in // foo: list(a)
  2
}
```

In the `body` of `let result on x1, ..., xn = body`, the type of `xi` is cast into its noetic variant. Using this, we can create a noema.

It might be illuminating to see that `let-on` is essentially the following syntax sugar:

```neut
let len on xs = e in
cont

// ↓ desugar

let xs = unsafe-cast(a, &a, xs) in // cast: `a` ~> `&a`
let len = e in                     // (use `&a`)
let xs = unsafe-cast(&a, a, xs) in // uncast: `&a` ~> `a`
cont
```

For memory safety, the `result` cannot contain any noemata. For example, the following code results in an error:

```neut
let result on xs =
  let foo = xs in // foo: &list(a)
  foo // error: a term of the following type might be noetic: &list(a)
in
...
```

Also note that, since a function can contain a noema, the type of `result` can't be like `a -> b`.

## Avoiding Unnecessary Copying

A noema isn't copied even when used multiple times. Let's take the following code for example:

```neut
  let result on xs =
    let foo = xs in // foo: &list(a)
    let bar = xs in // bar: &list(a)
    1
  in
  cont
```

The code above clearly uses `xs` twice in the body of `let-on`. However, since the type of `xs` is noetic, the `xs` isn't copied. The same `xs` is used in both of the occurrences.

By using a noema, we can perform something like "borrowing" in other languages.

## Using a Noema: Viewing Its Contents

The content of a noema can be viewed using `&match`:

```neut
define length(a: tau, xs: &list(a)): int {
  &match xs {
  - [] =>
    0
  - y :: ys =>
    add-int(1, length(a, ys))
  }
}
```

Here, the `&match` is the noetic variant of `match`; It does the same as `match` except that it doesn't free the outer tuple of the given value (`xs` in this &match).

The variables that are bound by a `&match` are cast to be noetic. For example, the `y` in the example above is not of type `a`, but of type `&a`. The `ys` is not of type `list(a)`, but of type `&list(a)`. Since the type of `y` is noetic, `y` isn't discarded even though it isn't used.

## Using a Noema: Incarnation

You can embody a noema using `!e`:

```neut
define sum-of-list(xs: &list(int)): int {
  &match xs {
  - [] =>
    0
  - y :: ys =>
    add-int(!y, sum-of-list(ys)) // ← a use of `!e`
  }
}
```

`!e` copies a noema along its inner type. For example, since the `y` above is of type `&int`, `!y: int` is a `y`'s new copy of type `int` (which is the same as `y` in this case, since `y` is an immediate).

## The Type of a Static Text

The fact that a noema isn't copied/discarded can be exploited to represent a static text. In Neut, the type of static text is `&text`:

```neut
"Hello world!\n": &text
```

Some text operations are defined on this noetic type. For example, the core library defines `print: &text -> unit`. Thus the following is a well-typed code:

```neut
define main(): unit {
  print("Hello world!\n")
}
```

Our kawaii hello world.
