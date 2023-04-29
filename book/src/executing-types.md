# Executing Types

## Types as Exponential

Here, we'll see how a type can be used to copy/discard the terms of the type. Let's take a simple variant type for example:

```neut
variant item {
- New(i64, i64)
}
```

Suppose that the internal representation of `New(10, 20)` is `(10, 20)`.

In this case, the type `item` is translated into a closed function like below:

```neut
define exp-item(action-selector, v) {
  if action-selector == 0 {
    // deallocate the outer tuple of `v`
    free(v)
  } else {
    // copy `v`
    // allocate memory regions
    let ptrX = malloc({2-words})
    let ptrY = malloc({2-words})
    // get values from `v`
    let v1 = v[0]
    let v2 = v[1]
    // deallocate original `v`
    free(v)
    // store values to new pointers
    store(ptrX[0], v1)
    store(ptrY[0], v1)
    store(ptrX[1], v2)
    store(ptrY[1], v2)
    // ... and return
    (ptrX, ptrY)
  }
}
```

Thas is, `exp-item(action-selector, v)` discards `v` if `action-selector` is 0. Otherwise, `exp-item` consumes `v`, creates two new copies, and then returns the tuple of them `(ptrX, ptrY)`.

More generally, the type `A` is translated into something like below:

```neut
// note: exp-A cannot contain any free variables
define exp-A(action-selector, v) {
  if action-selector == 0 {
    // a proceduce that discards v
  } else {
    // a procedure that creates two copies of `v`, by consuming the original `v`
  }
}
```

(Note: Obviously, the copying part is doing avoidable memory allocations/deallocations. We'll soon see that things are optimized in the actual implementation)

Incidentally, it might also be illuminating to see how a term that contains `item` is compiled:

```neut
let my-type = item
print("hello")
cont

// ↓ compile

let my-type = exp-item // lowered to a pointer to `exp-item`
print("hello")
cont

// ... also, the function below is added to the resulting LLVM IR
define exp-item(action-selector, v) {
  // (... the definition that we've just seen ...)
}
```

A new function for the type is created, and the type itself is lowered to a pointer to the function.

## Example Behavior of Types

This `exp-item` is utilized when a variable is used relevantly (more than once) as follows:

```neut
let x = New(10, 20)
let a = foo(x)
let b = bar(x)
let c = buz(x)
cont(a, b, c)

// ↓

let x = New(10, 20)
let (x1, tmp) = exp-item(1, x) // copy `x` by passing 1 as `action-selector`
let (x2, x3) = exp-item(1, tmp)
let a = foo(x1)
let b = bar(x2)
let c = buz(x3)
cont(a, b, c)
```

`exp-item` is also utilized when a variable is used affinely (unused) as follows:

```neut
let x = New(10, 20)
print("hello")

// ↓

let x = New(10, 20)
let () = exp-item(0, x) // discard `x` by passing 1 as `action-selector`
print("hello")
```

In both of the code above, by inserting `exp-item`, the use of variable `x` is linearized, which is what we wanted. After this translation, the language is now linear. Thus we can enjoy the power of the static memory management in a linear language now; A value is consumed when it is used.

This copying/discarding procedure happens immediately after a variable is defined. This is a source of the memory predictability of Neut.

## Actual Faster Behavior

The behavior of an exponential is a bit different in the actual implementation. A variable is actually copied as follows:

```neut
let x = New(10, 20)
let a = foo(x)
let b = bar(x)
let c = buz(x)
cont(a, b, c)

// ↓

let x = New(10, 20)
let x1 = exp-item(1, x)
let x2 = exp-item(1, x)
let x3 = exp-item(1, x)
let a = foo(x1)
let b = bar(x2)
let c = buz(x3)
cont(a, b, c)
```

That is, the copying part of `exp-item` is reformulated so that it returns only a clone. This is to avoid creating unnecessary tuples. Although the resulting code contains multiple use of `x` now, this is safe because we reformulate `exp-item` so that it won't deallocate `x`.

All in all, the actual implementation of `exp-item` will be something like below:

```neut
define exp-item(action-selector, v) {
  if action-selector == 0 {                       // same
    // discard `v`                                // same
    free(v)                                       // same
  } else {                                        // same
    // copy `v`, keeping the original `v` intact  // changed
    let ptr = malloc({2-words})                   // changed
    let v1 = v[0]                                 // changed
    let v2 = v[1]                                 // changed
    ptr                                           // changed
  }
}
```

Or, more generally, the type `A` is translated into something like below:

```neut
// note: exp-A cannot contain any free variables
define exp-A(action-selector, v) {
  if action-selector == 0 {
    // a proceduce that discards v
  } else {
    // a procedure that creates a copy of `v`, keeping original `v` intact
  }
}
```

## Exponential for Immediates

Immediates like integers or floats can be used multiple times safely since they don't need malloc/free. That is, they actually don't need to be copied/discarded. This fact is reflected by their corresponding exponentials. Both `i64` and `f64` (and other immediate types) are translated into something like below:

```neut
define exp-immediate(action-selector, v) {
  if action-selector == 0 {
    0
  } else {
    v // just use the original one as a copy
  }
}
```

It might be worth noting that, since these fake exponentials are inlined at compile time, there's no runtime cost. For explanation purpose, inlining is omitted here.

## Exponential for Types

Neut is a dependently-typed programming language. This means that types like `i64: tau` can also be used as ordinary values:

```neut
let t = i64
let a = f(t)
let b = g(t)
...
```

where the `tau` is the type of types.

This means that we need to translate the `tau` into a function. You may be wondering how, but this is actually fairly easy. Since a type is translated into a closed function as we've seen, things like `i64` or `text -> bool` are lowered to pointers to the function at runtime, which is nothing but an immediate. Types are therefore translated into the pointer to `exp-immediate` that we've just seen.

## Exponential for Closures

This type exponential can also be constructed for closures. Although this is one of the most interesting point of Neut, since it could be a bit complicated, I put it in the appendix. See [the Chapter in the Appendix](./executing-the-function-type.md) if you're interested in it.

## Exporential for Variant Types

Variant types like below also have exponentials, of course:

```neut
variant list(a) {
- Nil
- Cons(a, list(a))
}
```

There is one caveat here, however. Since an exponential is a *closed* function, the values of a variant type must be able to be copied/discarded using a closed function. This means that the information of `a` in `list(a)` must be contained in the values.

That is, for example, the internal representation of `Nil` is something like below:

```neut
(address-of-the-exponential-a, 0)
```

where the `0` is the discriminant for `Nil`. Also, that of `Cons(10, xs)` is:

```neut
(address-of-the-exponential-a, 1, 10, xs)
```

where the `1` is the discriminant for `Cons`.

With that in mind, the actual exponential for `list(a)` will be something like below (A bit lengthy; Skip it and just read the succeeding note if you aren't really interested in details):

```neut
define exp-list(action-selector, v) {
  if action-selector == 0 {
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

What causes a problem is the fact that the `xs` is used non-linearly in the code above. The first occurrence is at `length(xs)`, and the second one is at `foo(xs)`. This means the code above is translated into something like this:

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

That is, the whole list `xs`, including all the elements, is copied *just to obtain its length*. This is nothing but a tragedy. Worse, this kind of procedure is pervasive. The list one isn't special at all. Flowers wither. The sky falls. We need something like a loophole, or every wish is crushed into pieces.

This is the topic covered in the next section. We'll see some "noetic" operations there.
