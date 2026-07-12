# On Executing Types

## Table of Contents

- [Types as Closed Functions](#types-as-closed-functions)
- [Example Behavior of Types](#example-behavior-of-types)
- [Immediate Types](#immediate-types)
- [Polymorphic Types](#polymorphic-types)
- [Algebraic Data Types](#algebraic-data-types)
- [Advanced: Function Types](#advanced-function-types)

## Types as Closed Functions

Here, we'll see how a type is translated into a function that discards, copies, and reports the size of values of the type. To see the basic idea, let's take a simple ADT as an example:

```neut
data item {
| New(int, int)
}
```

The internal representation of `New(10, 20)` is something like the following:

```neut
New(10, 20)

// ↓ (compile)

let ptr = malloc({2-words});
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

The value `v` can be copied into newly allocated owned storage as follows:

```neut
// copy `v`, keeping the original `v` intact
let ptr = malloc({2-words});
store(ptr[0], v[0]); // ptr[0] := v[0]
store(ptr[1], v[1]); // ptr[1] := v[1]
ptr
```

### Combining Discarding/Copying Functions

Using the procedures above, we can construct a closed function that discards, copies, and reports the size of values of the type `item`. The copy operation either returns an owned copy or writes into caller-supplied storage:

```neut
define exp-item(selector, v, extra) {
  if selector == 0 {
    // discard
    if extra == 1 {
      free(v)
    }
  } else if selector == 1 {
    // copy
    let ptr =
      if extra == null {
        malloc({2-words})
      } else {
        extra
      };
    store(ptr[0], v[0]);
    store(ptr[1], v[1]);
    ptr
  } else {
    // size
    {2-words}
  }
}
```

`exp-item(selector, v, extra)` discards `v` if `selector` is 0, copies `v` if `selector` is 1, and reports the size of `item` if `selector` is 2. The `extra` argument controls whether discard releases outer storage and whether copy allocates owned storage or writes into caller-supplied storage. The return value of non-null-destination copy is ignored.

The type `item` is compiled into a pointer to this function.

More generally, a type `a` is translated into a pointer to a closed function like the following:

```neut
define exp-a(selector, v, extra) {
  if selector == 0 {
    // a procedure that destroys `v`; `extra` decides whether to release outer storage
  } else if selector == 1 {
    // a procedure that copies `v`; `extra` selects owned or caller-supplied storage
  } else {
    // a procedure that reports the size of values of type `a`
  }
}
```

We'll call such a closed function a resource exponential of `a`.

## Example Behavior of Types

This `exp-item` is called when a variable isn't used:

```neut
let x = New(10, 20);
print("hello") // `x` isn't used

// ↓ (compile)

let x = New(10, 20);
let _ = exp-item(0, x, 1); // discard `x` by passing 0 as `selector`
print("hello")
```

This `exp-item` is also called when a variable is used more than once:

```neut
let x = New(10, 20);
let a = foo(x); // first use of `x`
let b = bar(x); // second use of `x`
cont(a, b)

// ↓ (compile)

let x = New(10, 20);
let x-copy = exp-item(1, x, null); // copy `x` by passing 1 as `selector`
let a = foo(x-copy);
let b = bar(x);
cont(a, b)
```

<div class="info-block">

This discarding/copying procedure happens _immediately after a variable is defined_.

</div>

## Immediate Types

Immediates like integers or floats don't have to be discarded or copied since they don't rely on memory-related operations like `malloc` or `free`. This fact is reflected in the function that `int` and `float` are translated into:

```neut
define base::#.imm(selector, value, extra) {
  if selector == 0 {
    0 // "discarding" doesn't have to do anything
  } else if selector == 1 {
    value // "copying" simply reuses the original value
  } else {
    -1 // immediates don't have fixed-size placed storage of their own
  }
}
```

Immediates report a negative size. A type with a negative size never uses `extra`: it owns no releasable storage to discard, and is never copied into caller-supplied storage.

Immediate types are compiled into this function. Noema types like `&list(int)` are also translated into this function.

Uses of `base::#.imm` like `base::#.imm(1, some-value, null)` are optimized away by inlining.

<div class="info-block">

A type is compiled into a pointer to a closed function. This means that types are immediate values. Because of that, the type of types (`type`) is also compiled into `base::#.imm`.

</div>

## Polymorphic Types

Let's see how polymorphic values are copied. Consider the following code:

```neut
define foo<a>(x: a) -> pair(a, a) {
  Pair(x, x)
}
```

The code uses the variable `x` twice. Thus, this `x` must be copied according to the type `a`.

This can be done because the internal representation of `a` is a function that can discard, copy, and report the size of values of type `a`. Thus, the above code is compiled into something like the following:

```neut
define foo<a>(x: a) -> pair(a, a) {
  let x-clone = a(1, x, null);
  Pair(x, x-clone)
}
```

Thus, we can discard and copy values of polymorphic types.

## Algebraic Data Types

ADTs also have resource exponentials, of course. Consider the following type:

```neut
data list(a) {
| Nil
| Cons(a, list(a))
}
```

For `list(a)`, every value occupies 4 words: a discriminant, one word for `a`, and two words for the largest payload (`Cons(a, list(a))`). For example, `Nil` and `Cons(10, xs)` are represented as follows:

```neut
(0, a, _, _)   // Nil: `0` is the discriminant; the trailing words are unused
(1, a, 10, xs) // Cons(10, xs): `1` is the discriminant
```

`list(a)` is therefore compiled into something like the following (a bit lengthy; you can skip to the following note if you prefer):

```neut
define exp-list(selector, v, extra) {
  if selector == 0 {
    let d = get-discriminant(v);
    if d == 0 {
      // discard Nil
      if extra == 1 {
        free(v)
      }
    } else {
      // discard Cons
      let a = v[1];
      let cons-head = v[2];
      let cons-tail = v[3];
      let _ = a(0, cons-head, 1); // ← discard the head of cons using v[1]
      exp-list(0, cons-tail, 1);
      if extra == 1 {
        free(v)
      }
    }
  } else if selector == 1 {
    let d = get-discriminant(v);
    let ptr =
      if extra == null {
        malloc({4-words})
      } else {
        extra
      };
    if d == 0 {
      // copy Nil
      let a = v[1];
      store(ptr[0], d);
      store(ptr[1], a);
      // ptr[2] and ptr[3] stay uninitialized
      ptr
    } else {
      // copy Cons
      let a = v[1];
      let cons-head-copy = a(1, v[2], null); // ← copy the head of cons using v[1]
      let cons-tail-copy = exp-list(1, v[3], null);
      store(ptr[0], d);
      store(ptr[1], a);
      store(ptr[2], cons-head-copy);
      store(ptr[3], cons-tail-copy);
      ptr
    }
  } else {
    {4-words}
  }
}
```

The point is that the type information in a value is loaded at runtime and used to discard/copy values. The same resource exponential also reports the fixed size used by the type. Even when a constructor carries fewer fields, the resource exponential still works on the fixed-size layout, leaving the unused slots untouched.

## Advanced: Function Types

We'll see how function types like `(int) -> bool` are translated.

Suppose we have a function like the following:

```neut
define foo<a>(x: a) -> int {
  let y = Unit;
  let f =
    (z: int) => {
      let foo = x; // ← x is a free var of this lambda
      let bar = y; // ← y is also a free var of this lambda
      let baz = z;
      bar
    };
  0
}
```

Let's see how the lambda abstraction is compiled.

### Extracting a Closed Chain From a Lambda

First, the compiler collects all the free variables in the lambda. Here, the compiler also collects all the free variables in the types of those free variables. Thus, in this case, the compiler constructs a typed list like the following:

```neut
[a: type, x: a, y: unit]
```

Let's write this as a sequence `x1: t1, ..., xn: tn`. We call such a sequence a closed chain if each type `ti` mentions only earlier variables, that is,

```text
FreeVars(ti) ⊆ {x1, ..., x{i-1}}
```

for every `i`.

In the example above, this condition holds because:

- `FreeVars(type) = ∅ ⊆ ∅`
- `FreeVars(a) = {a} ⊆ {a}`
- `FreeVars(unit) = ∅ ⊆ {a, x}`

### Closure Conversion

We'll use this closed chain to compile a lambda. The internal representation of a closure for the lambda will be a 3-word tuple like the following:

```text
(ENVIRONMENT-TYPE, (a, x, y), LABEL-TO-FUNCTION-DEFINITION)
                   ---------
                   the closed chain (i.e. environment)
```

This is more or less the usual closure conversion, except that we now have the types of the free variables in the closure.

### Discarding/Copying a Closure

Knowing its internal representation, we can now discard/copy a closure. To make an owned copy of a closure, we can do the following:

```neut
// copy a closure `cls`

let env-type = cls[0]; // get the type of the environment
let env      = cls[1]; // get the pointer to the environment
let label    = cls[2]; // get the label to the function

let env-clone = env-type(1, env, null); // copy the environment using its type

// allocate new memory region for our new closure
let new-ptr = malloc(mul-int(3, word-size));

// store cloned values
store(new-ptr[0], env-type);  // remember that a type is an immediate
store(new-ptr[1], env-clone);
store(new-ptr[2], label);     // note that a label is an immediate

new-ptr // ... and return the new closure
```

Discarding a closure can also be done using the same idea: discard the environment using the type information in the closure.

### Translating a Function Type

This leads us to translate the function type as follows:

```neut
(x1: A1, ..., xn: An) -> B

// ↓

define base::#.cls(action-selector, cls, extra) {
  if action-selector == 0 {
    // discard

    // discard the environment using its type
    let env-type = cls[0];
    let env      = cls[1];
    env-type(0, env, 1);

    // discard the tuple of the closure
    if extra == 1 {
      free(cls)
    }
  } else if action-selector == 1 {
    // copy

    // get the original values
    let env-type = cls[0];
    let env      = cls[1];
    let label    = cls[2];

    // copy the environment using its type
    let env-clone = env-type(1, env, null);

    let new-ptr =
      if extra == null {
        malloc(mul-int(3, word-size))
      } else {
        extra
      };
    // copy the original values
    store(new-ptr[0], env-type);
    store(new-ptr[1], env-clone);
    store(new-ptr[2], label);

    new-ptr
  } else {
    mul-int(3, word-size)
  }
}
```

<div class="info-block">

Every function type is translated into this same `base::#.cls`, regardless of its argument and result types.

</div>
