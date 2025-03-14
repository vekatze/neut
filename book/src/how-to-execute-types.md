# How to Execute Types

## Table of Contents

- [Types as Closed Functions](#types-as-closed-functions)
- [Example Behavior of Types](#example-behavior-of-types)
- [Immediate Types](#immediate-types)
- [Polymorphic Types](#polymorphic-types)
- [Algebraic Data Types](#algebraic-data-types)
- [Advanced: Function Types](#advanced-function-types)

## Types as Closed Functions

Here, we'll see how a type is translated into a function that discards/copies the terms of the type. To see the basic idea, let's take a simple ADT for example:

```neut
data item {
| New(int, int)
}
```

The internal representation of `New(10, 20)` is something like the below:

```neut
New(10, 20)

// ↓ (compile)

let ptr = malloc({2-words}) in
store(ptr[0], 10); // ptr[0] := 10
store(ptr[1], 20); // ptr[1] := 20
ptr
```

### Discarding/Copying a Value

Now, let's see how to discard and copy the values of the type `item`.

A value `v` of type `item` can be discarded as follows:

```neut
free(v)
```

The `v` can be copied as follows:

```neut
// copy `v`, keeping the original `v` intact
let ptr = malloc({2-words}) in
store(ptr[0], v[0]); // ptr[0] := v[0]
store(ptr[1], v[1]); // ptr[1] := v[1]
ptr
```

Not a big deal, right?

### Combining Discarding/Copying Functions

Using the two procedures above, we can construct a closed function that discards and copies the values of the type `item`:

```neut
define exp-item(selector, v) {
  if selector == 0 {
    // discard
    free(v)
  } else {
    // copy `v`
    let ptr = malloc({2-words}) in
    store(ptr[0], v[0]);
    store(ptr[1], v[1]);
    ptr
  }
}
```

`exp-item(selector, v)` discards `v` if `selector` is 0. Otherwise, this function creates a copy of `v` and then returns it, keeping the original `v` intact.

The type `item` is compiled into a pointer to this function.

More generally, a type `a` is translated into a pointer to a closed function like below:

```neut
define exp-a(selector, v) {
  if selector == 0 {
    // a proceduce that discards `v`
  } else {
    // a procedure that copies `v` (keeping the original `v` intact)
  }
}
```

We'll call such a closed function a resource exponential of `a`.

## Example Behavior of Types

This `exp-item` is called when a variable isn't used:

```neut
let x = New(10, 20) in
print("hello") // `x` isn't used

// ↓ (compile)

let x = New(10, 20) in
let _ = exp-item(0, x) in // discard `x` by passing 0 as `selector`
print("hello")
```

This `exp-item` is also called when a variable is used more than twice:

```neut
let x = New(10, 20) in
let a = foo(x) in // first use of `x`
let b = bar(x) in // second use of `x`
cont(a, b)

// ↓ (compile)

let x = New(10, 20) in
let x-copy = exp-item(1, x) in // copy `x` by passing 1 as `selector`
let a = foo(x-copy) in
let b = bar(x) in
cont(a, b)
```

<div class="info-block">

This discarding/copying procedure happens _immediately after a variable is defined_.

</div>

## Immediate Types

Immediates like integers or floats don't have to be discarded or copied since they don't rely on memory-related operations like `malloc` or `free`. This fact is reflected in the resulting function that `int` or `float` are translated into:

```neut
define base.#.imm(selector, value) {
  if selector == 0 {
    0 // "discarding" doesn't have to do anything
  } else {
    value // "copying" simply reuses the original value
  }
}
```

Immediate types are compiled into this function. Noema types like `&list(int)` are also translated into this function.

Uses of `base.#.imm` like `base.#.imm(1, some-value)` are optimized away by inlining.

<div class="info-block">

A type is compiled into a pointer to a closed function. This means that types are immediate values. Because of that, the type of types (`type`) is also compiled into `base.#.imm`.

</div>

## Polymorphic Types

Let's see how polymorphic values are copied. Consider the following code:

```neut
define foo(a: type, x: a): pair(a, a) {
  Pair(x, x)
}
```

The code uses the variable `x` twice. Thus, this `x` must be copied according to the type `a`.

This can be done because the internal representation of `a` is a function that can discard and copy the values of type `a`. Thus, the above code is compiled into something like the below:

```neut
define foo(a: type, x: a): pair(a, a) {
  let x-clone = a(1, x) in
  Pair(x, x-clone)
}
```

Thus, we can discard and copy values of polymorphic types.

## Algebraic Data Types

ADTs like the below also have resource exponentials, of course:

```neut
data list(a) {
| Nil
| Cons(a, list(a))
}
```

The first thing to note is that the values of an ADT must be able to be discarded/copied using a closed function (since all the types in Neut are compiled into closed functions). This means the information about `a` in `list(a)` must be contained in the values.

That is, for example, the internal representation of `Nil` is something like below:

```neut
(a, 0)
```

Here, the `0` is the discriminant for `Nil`. Also, that of `Cons(10, xs)` is:

```neut
(a, 1, 10, xs)
```

Here, the `1` is the discriminant for `Cons`.

With that in mind, the resource exponential of `list(a)` will be something like the below (A bit lengthy; Skip it and just read the succeeding note if you aren't that interested in details):

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
      store(ptr[0], a);
      store(ptr[1], d);
      ptr
    } else {
      // copy Cons
      let ptr = malloc({4-words}) in
      let a = v[0] in
      let cons-head-copy = a(1, v[2]) in // ← copy the head of cons using v[0]
      let cons-tail-copy = exp-list(1, v[3]) in
      store(ptr[0], a);
      store(ptr[1], d);
      store(ptr[2], cons-head-copy);
      store(ptr[3], cons-tail-copy);
      ptr
    }
  }
}
```

The point is that _the type information in a value is loaded at runtime and used to discard/copy values_. This utilization of types is the main point of first-class types in Neut.

The main part of this section is now over. What follows is for curious cats.

## Advanced: Function Types

We'll see how function types like `(int) -> bool` are translated.

Suppose we have a function like the below:

```neut
define foo(a: type): int {
  let x: int = 10 in
  let y = type in
  let f =
    function (z: a) {  // lambda function
      let foo = x in   // ← x is a free var of this lambda
      let bar = y in   // ← y is also a free var of this lambda
      let buz = z in
      bar
    }
  in
  0
}
```

Let's see how the `lambda` inside the function is compiled.

### Extracting a Closed Chain From a Lambda

First, the compiler collects all the free variables in the lambda. Here, the compiler also collects all the free variables in the types of the free variables. Thus, in this case, the compiler constructs a list like below:

```neut
[a, x, y, z]
```

This list is "closed" in the following sense. Consider annotating all the variables in the list by their variables, like below:

```neut
[a: type, x: int, y: type, z: a]
```

This list is closed in that the term

```neut
function (a: type, x: int, y: type, z: a) {
  Unit
}
```

contains no free variables. We'll call a list like this a closed chain.

### Closure Conversion

We'll use this closed chain to compile a lambda. The internal representation of a closure for the lambda will be a 3-word tuple like the following:

```text
(Σ (a: type, x: int, y: type). a , (a, x, y, z), LABEL-TO-FUNCTION-DEFINITION)
 -----------------------------   ------------
 the type of the environment     the closed chain (i.e. environment)
```

This is more or less the usual closure conversion, except that we now have the types of the free variables in the closure.

### Discarding/Copying a Closure

Knowing its internal representation, we can now discard/copy a closure. To copy a closure, we can do the following:

```neut
// copy a closure `cls`

let env-type = cls[0] in // get the type of the environment
let env      = cls[1] in // get the pointer to the environment
let label    = cls[2] in // get the label to the function

let env-clone = env-type(1, env) in // copy the environment using the type of it

// allocate new memory region for our new closure
let new-ptr = malloc(mul-int(3, word-size)) in

// store cloned values
store(new-ptr[0], env-type);  // remember that a type is an immediate
store(new-ptr[1], env-clone);
store(new-ptr[2], label);     // note that a label is an immediate

new-ptr // ... and return the new closure
```

Discarding a closure can also be done with the same idea: discard the environment using the type information in the closure.

### Translating a Function Type

This leads us to translate the function type as follows:

```neut
(x1: A1, ..., xn: An) -> B

// ↓

define base.#.cls(action-selector, cls) {
  if action-selector == 0 {
    // discard

    // discard the environment using the type of it
    let env-type = cls[0] in
    let env      = cls[1] in
    env-type(0, env);

    // discard the tuple of the closure
    free(cls)
  } else {
    // copy

    // get the original values
    let env-type = cls[0] in
    let env      = cls[1] in
    let label    = cls[2] in

    // copy the environment using the type of it
    let env-clone = env-type(1, env) in

    let new-ptr = malloc(mul-int(3, word-size)) in
    // copy the original values
    store(new-ptr[0], env-type);
    store(new-ptr[1], env-clone);
    store(new-ptr[2], label);

    // ... and return the new closure
    new-ptr
  }
}
```

<div class="info-block">

Every function type is translated into this same `base.#.cls`, no matter its argument types and the result types.

</div>
