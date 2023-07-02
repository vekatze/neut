# Exponentials in Action

## Exponential for Immediates

Immediates like integers or floats can be used multiple times safely since they don't need malloc/free. This fact is reflected by their corresponding exponentials. The types of integers and floats are translated into something like the below:

```neut
define exp-immediate(selector, v) {
  if selector == 0 {
    0 // do nothing against `v` (no `free` is necessary)
  } else {
    v // just use the original one as a copy
  }
}
```

It might be worth noting that, since these fake exponentials are inlined at compile time, there's no runtime cost. Inlining is omitted here for explanatory purposes.

## Exponential for The Type of Types

Neut is a dependently-typed programming language. This means that the type of type, `tau`, can also be used as an ordinary value:

```neut
let t = tau in
let a = f(t) in
let b = g(t) in
...
```

This means that we need to translate this `tau` into a function, too. It might sound difficult, but it's actually easy. Since a type is translated into a closed function as we've seen, types like `int` or `int -> bool` are lowered to pointers to the function, which is nothing but an immediate. `tau` is therefore translated into the pointer to `exp-immediate` that we've just seen.

## Exponential for Closures

A resource exponential can also be constructed for closures. Although this is one of the most interesting points of Neut, since it is a bit complicated, I put it in the appendix. See [the Chapter in the Appendix](./executing-the-function-type.md) if you're interested in it.

## Exponential for ADTs

ADTs like the below also have resource exponentials, of course:

```neut
data list(a) {
- Nil
- Cons(a, list(a))
}
```

The first thing to note is that, since an exponential is a closed function, the values of an ADT must be able to be copied/discarded using a closed function. This means that the information about `a` in `list(a)` must be contained in the values.

That is, for example, the internal representation of `Nil` is something like below:

```neut
(a, 0)
```

where the `0` is the discriminant for `Nil`. Also, that of `Cons(10, xs)` is:

```neut
(a, 1, 10, xs)
```

where the `1` is the discriminant for `Cons`.

With that in mind, the actual exponential for `list(a)` will be something like the below (A bit lengthy; Skip it and just read the succeeding note if you aren't really interested in details):

```neut
define exp-list(selector, v) {
  if selector == 0 {
    let d = get-discriminant(v) in
    if d == 0 {
      // discard Nil
      free(v)
    } else {
      // discard Cons
      let a = v[0] in
      let cons-head = v[1] in
      let cons-tail = v[2] in
      free(v);
      let () = a(0, v[1]) in // ← discard the head of cons using v[0]
      exp-list(0, v[2])
    }
  } else {
    let d = get-discriminant(v) in
    if d == 0 {
      // copy Nil
      let ptr = malloc({2-words}) in
      let a = v[0] in
      store(a, ptr[0]);
      store(d, ptr[1]);
      ptr
    } else {
      // copy Cons
      let ptr = malloc({4-words}) in
      let a = v[0] in
      let cons-head-copy = a(1, v[2]) in // ← copy the head of cons using v[0]
      let cons-tail-copy = exp-list(1, v[3]) in
      store(a, ptr[0]);
      store(d, ptr[1]);
      store(cons-head-copy, ptr[2]);
      store(cons-tail-copy, ptr[3]);
      ptr
    }
  }
}
```

The point here is that *the type information in a value is loaded at runtime and used to copy/discard values*. This is the main point of dependent types in Neut. Normally, I believe you would expect something like safe array indexing or theorem proving when you hear "This language has dependent types". In Neut, however, the focus of dependent types is on resource management.

## A Tragedy in the Physical World

...So we have found a way to linearize the language. Does it finish the story? Unfortunately not, as in every storyline. Suppose we have a list `xs`, and want to choose what to do by its length:

```neut
let xs: list(int) = [1, 2, 3] in
let len = length(int, xs) in
if cond(len) {
  print("hey")
} else {
  print("yo")
};
foo(xs)
```

where the `length` is defined as follows:

```neut
define length(a: tau, xs: list(a)): int {
  match xs {
  - [] =>
    0
  - y :: ys =>
    add-int(1, length(a, ys))
  }
}
```

Here, the `match` is for the usual pattern matching; It inspects its arguments (`xs`), and performs branching according to them. After branching, it extracts the content of a value (`y` and `ys` if the latter branch is chosen, for example), and then calls `free` against the outer tuple of `xs`. Then evaluates the body of the branch (`add-int(1, length(ys))` in this case).

What causes a problem here is the fact that the `xs` is used non-linearly in the code above. The first occurrence is at `length(xs)`, and the second one is at `foo(xs)`. This means the code above is translated into something like the below:

```neut
let xs: list(int) = [1, 2, 3] in
let xs-copy = exp-list(1, xs) in   // copy the original list
let len = length(int, xs-copy) in  // ... and use it to get its length
if cond(len) {
  print("hey")
} else {
  print("yo")
};
foo(xs)
```

That is, the whole list `xs`, including all the elements, is copied *just to obtain its length*. This is nothing but a tragedy. Worse, this kind of procedure is pervasive. The list one isn't special at all. We need something like a loophole, or every wish is crushed into pieces.

This is the topic covered in the next section. We'll see some "noetic" operations there.
