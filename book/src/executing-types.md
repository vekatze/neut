# Executing Types

Here, we'll see how a type is translated into a function that copies/discards the terms of the type. To see the basic idea, let's take a simple ADT for example:

```neut
data item {
- New(int, int)
}
```

Suppose that the internal representation of `New(10, 20)` is something like below:

```neut
New(10, 20)

// ↓ (compile)

let v = malloc({2-words}) in
store(10, v[0]);
store(20, v[1]);
v
```

We'll see how the type `item` is translated.

### Copying/Discarding a Value

First of all, in this case, a value `v` of type `item` can be discarded as follows:

```neut
free(v)
```

That is, we just have to apply `free` to the pointer. Also, the `v` can be copied as follows:

```neut
// get values from `v`
let v1 = v[0] in
let v2 = v[1] in

// deallocate original `v`
free(v);

// allocate memory regions to store new values
let v-copy-x = malloc({2-words}) in
let v-copy-y = malloc({2-words}) in

// store values to the new pointers
store(v1, v-copy-x[0]);
store(v1, v-copy-y[0]);
store(v2, v-copy-x[1]);
store(v2, v-copy-y[1]);

// ... and return
(v-copy-x, v-copy-y)
```

### Combining Copying/Discarding functions

Using the two procedures above, the type `item` is translated into a function like below:

```neut
define exp-item(selector, v) {
  if selector == 0 {
    // discard
    free(v)
  } else {
    // copy
    let v1 = v[0] in
    let v2 = v[1] in
    free(v);
    let ptr-x = malloc({2-words}) in
    let ptr-y = malloc({2-words}) in
    store(v1, ptr-x[0]);
    store(v1, ptr-y[0]);
    store(v2, ptr-x[1]);
    store(v2, ptr-y[1]);
    (ptr-x, ptr-y)
  }
}
```

`exp-item(selector, v)` discards `v` if `selector` is 0. Otherwise, `exp-item` consumes `v`, creates two new copies, and then returns them as a tuple.

Also, an `item` in source file is compiled into a pointer to `exp-item`.

More generally, a type `a` is translated into something like below:

```neut
define exp-a(selector, v) {
  if selector == 0 {
    // a proceduce that discards v
  } else {
    // a procedure that copies `v`, consuming the original `v`
  }
}
```

We'll call such a function as the resource exponential of the type `a`.

### Example Behavior of Types

This `exp-item` is called when a variable is used relevantly (= more than once) as follows:

```neut
let x = New(10, 20) in
let a = foo(x) in // first use of `x`
let b = bar(x) in // second use of `x`
let c = buz(x) in // third use of `x`
cont(a, b, c)

// ↓

let x = New(10, 20) in
let (x1, tmp) = exp-item(1, x) in // copy `x` by passing 1 as `selector`
let (x2, x3) = exp-item(1, tmp) in
let a = foo(x1) in
let b = bar(x2) in
let c = buz(x3) in
cont(a, b, c)
```

`exp-item` is also called when a variable is used affinely (= unused) as follows:

```neut
let x = New(10, 20) in
print("hello") // `x` isn't used

// ↓

let x = New(10, 20) in
let () = exp-item(0, x) in // discard `x` by passing 0 as `selector`
print("hello")
```

Note that the variable `x` is used linearly after translation. Every variable in Neut is linearized in this way so that we can now enjoy the power of linearity.

This copying/discarding procedure happens *immediately after a variable is defined*. This is also a source of the memory predictability of Neut.

## The Real (Faster) Behavior

The behavior of an exponential is a bit different in the real implementation. There, a variable is copied as follows:

```neut
let x = New(10, 20) in
let a = foo(x) in // first use of `x`
let b = bar(x) in // second use of `x`
cont(a, b)

// ↓

let x = New(10, 20) in
let x-copy = exp-item(1, x) in
let a = foo(x-copy) in
let b = bar(x) in
cont(a, b)
```

That is, the copying part of `exp-item` is reformulated so that it returns only a clone. This avoids creating unnecessary tuples. Although the resulting code contains multiple uses of `x` now, this is safe because we reformulate `exp-item` so that it won't deallocate `x`.

All in all, the actual implementation of `exp-item` will be something like the below:

```neut
define exp-item(selector, v) {
  if selector == 0 {
    // discard `v`
    free(v)
  } else {
    // copy `v`, keeping the original `v` intact
    let ptr = malloc({2-words}) in
    let v1 = v[0] in
    let v2 = v[1] in
    ptr
  }
}
```

Or, more generally, type `A` is translated into something like below:

```neut
// note: exp-A cannot contain any free variables
define exp-A(selector, v) {
  if selector == 0 {
    // a proceduce that discards v
  } else {
    // a procedure that creates a copy of `v`, keeping original `v` intact
  }
}
```
