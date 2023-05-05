# Exponentials in Action

## Exponential for Immediates

Immediates like integers or floats can be used multiple times safely since they don't need malloc/free. This fact is reflected by their corresponding exponentials. Both `i64` and `f64` (and other immediate types) are translated into something like the below:

```neut
define exp-immediate(selector, v) {
  if selector == 0 {
    0
  } else {
    v // just use the original one as a copy
  }
}
```

It might be worth noting that, since these fake exponentials are inlined at compile time, there's no runtime cost. Inlining is omitted here for explanatory purposes.

## Exponential for Types

Neut is a dependently-typed programming language. This means that types like `i64: tau` can also be used as ordinary values:

```neut
let t = i64
let a = f(t)
let b = g(t)
...
```

where the `tau` is the type of the types.

This means that we need to translate the `tau` into a function. It might sound difficult, but it's easy in reality. Since a type is translated into a closed function as we've seen, things like `i64` or `text -> bool` are lowered to pointers to the function at runtime, which is nothing but an immediate. Types are therefore translated into the pointer to `exp-immediate` that we've just seen.

## Exponential for Closures

A type-exponential can also be constructed for closures. Although this is one of the most interesting points of Neut, since it could be a bit complicated, I put it in the appendix. See [the Chapter in the Appendix](./executing-the-function-type.md) if you're interested in it.

## Exponential for Variant Types

Variant types like the below also have exponentials, of course:

```neut
variant list(a) {
- Nil
- Cons(a, list(a))
}
```

There is one caveat here, however. Since an exponential is a closed function, the values of a variant type must be able to be copied/discarded using a closed function. This means that the information about `a` in `list(a)` must be contained in the values.

That is, for example, the internal representation of `Nil` is something like below:

```neut
(address-of-the-exponential-a, 0)
```

where the `0` is the discriminant for `Nil`. Also, that of `Cons(10, xs)` is:

```neut
(address-of-the-exponential-a, 1, 10, xs)
```

where the `1` is the discriminant for `Cons`.

With that in mind, the actual exponential for `list(a)` will be something like the below (A bit lengthy; Skip it and just read the succeeding note if you aren't really interested in details):

```neut
define exp-list(selector, v) {
  if selector == 0 {
    let d = get-discriminant(v)
    if d == 0 {
      // discard Nil
      free(v)
    } else {
      // discard Cons
      let a = v[0]
      let cons-head = v[1]
      let cons-tail = v[2]
      free(v)
      let () = a(0, v[1]) // ← discard the head of cons using v[0]
      exp-list(0, v[2])
    }
  } else {
    let d = get-discriminant(v)
    if d == 0 {
      // copy Nil
      let ptr = malloc({2-words})
      let a = v[0]
      store(ptr[0], a)
      store(ptr[1], d)
      ptr
    } else {
      // copy Cons
      let ptr = malloc({4-words})
      let a = v[0]
      let cons-head-copy = a(1, v[2]) // ← copy the head of cons using v[0]
      let cons-tail-copy = exp-list(1, v[3])
      store(ptr[0], a)
      store(ptr[1], d)
      store(ptr[2], cons-head-copy)
      store(ptr[3], cons-tail-copy)
      ptr
    }
  }
}
```

The point here is that *the type information in a value is loaded at runtime and used to copy/discard values*. This is the main point of dependent types in Neut. Normally, I believe you would expect something like safe array indexing or theorem proving when you hear "This language has dependent types". In Neut, however, the focus of dependent types is on resource management.

## A Tragedy in the Physical World

...So we have found a way to linearize the language. Does it finish the story? Unfortunately not, as in every storyline. Suppose we have a list `xs`, and want to choose what to do by its length:

```neut
let xs: list(i64) = [1, 2, 3]
let len = length(xs)
if cond(len) {
  print("hey")
} else {
  print("yo")
}
foo(xs)
```

What causes a problem is the fact that the `xs` is used non-linearly in the code above. The first occurrence is at `length(xs)`, and the second one is at `foo(xs)`. This means the code above is translated into something like the below:

```neut
let xs: list(i64) = [1, 2, 3]
let xs-copy = exp-list(1, xs) // copy the original list
let len = length(xs-copy) // ... and use it to get its length
if cond(len) {
  print("hey")
} else {
  print("yo")
}
foo(xs)
```

That is, the whole list `xs`, including all the elements, is copied *just to obtain its length*. This is nothing but a tragedy. Worse, this kind of procedure is pervasive. The list one isn't special at all. We need something like a loophole, or every wish is crushed into pieces.

This is the topic covered in the next section. We'll see some "noetic" operations there.
