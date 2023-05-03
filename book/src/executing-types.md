# Executing Types

## Types as Exponential

Here, we'll see how a type can be used to copy/discard the terms of the type. Let's take a simple variant type for example to see the basic idea:

```neut
variant item {
- New(i64, i64)
}
```

Suppose that the internal representation of `New(10, 20)` is `(10, 20)`.

### Copying/Discarding a Value

In this case, a value `v` of type `item` can be discarded as follows:

```neut
free(v)
```

That is, we just have to apply `free` to the pointer. Also, the `v` can be copied as follows:

```neut
// get values from `v`
let v1 = v[0]
let v2 = v[1]

// deallocate original `v`
free(v)

// allocate memory regions to store new values
let v-copy-x = malloc({2-words})
let v-copy-y = malloc({2-words})

// store values to new pointers
store(v-copy-x[0], v1)
store(v-copy-y[0], v1)
store(v-copy-x[1], v2)
store(v-copy-y[1], v2)

// ... and return
(v-copy-x, v-copy-y)
```

### Combining Copying/Discarding functions

The type `item` combines both of the two like below:

```neut
define exp-item(selector, v) {
  if selector == 0 {
    free(v)
  } else {
    let v1 = v[0]
    let v2 = v[1]
    free(v)
    let ptr-x = malloc({2-words})
    let ptr-y = malloc({2-words})
    store(ptr-x[0], v1)
    store(ptr-y[0], v1)
    store(ptr-x[1], v2)
    store(ptr-y[1], v2)
    (ptr-x, ptr-y)
  }
}
```

Thas is, `exp-item(selector, v)` discards `v` if `selector` is 0. Otherwise, `exp-item` consumes `v`, creates two new copies, and then returns them as a tuple.

Also, `item` in original source files are compiled into pointers to `exp-item`.

More generally, the type `a` is translated into something like below:

```neut
define exp-a(selector, v) {
  if selector == 0 {
    // a proceduce that discards v
  } else {
    // a procedure that copies `v`, consuming the original `v`
  }
}
```

<!-- (Note: Obviously, the copying part is doing avoidable memory allocations/deallocations. We'll soon see that things are optimized in the actual implementation) -->

<!-- Incidentally, it might also be illuminating to see how a term that contains `item` is compiled: -->

<!-- ```neut -->
<!-- let my-type = item -->
<!-- print("hello") -->
<!-- cont -->

<!-- // ↓ compile -->

<!-- let my-type = exp-item // lowered to a pointer to `exp-item` -->
<!-- print("hello") -->
<!-- cont -->

<!-- // ... also, the function below is added to the resulting LLVM IR -->
<!-- define exp-item(selector, v) { -->
<!--   // (... the definition that we've just seen ...) -->
<!-- } -->
<!-- ``` -->

<!-- A new function for the type is created, and the type itself is lowered to a pointer to the function. -->

### Example Behavior of Types

This `exp-item` is utilized when a variable is used relevantly (more than once) as follows:

```neut
let x = New(10, 20)
let a = foo(x)
let b = bar(x)
let c = buz(x)
cont(a, b, c)

// ↓

let x = New(10, 20)
let (x1, tmp) = exp-item(1, x) // copy `x` by passing 1 as `selector`
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
let () = exp-item(0, x) // discard `x` by passing 0 as `selector`
print("hello")
```

Note that the variable `x` is used lineary after translation. Every variable in Neut is linearized in this way. Thus, we can now enjoy the power of a linearity; *A value is simply consumed when it is used*.

This *copying/discarding procedure happens immediately after a variable is defined*. This is also a source of the memory predictability of Neut.

## Actual Faster Behavior

The behavior of an exponential is a bit different in the actual implementation. A variable is actually copied as follows:

```neut
let x = New(10, 20)
let a = foo(x)
let b = bar(x)
cont(a, b)

// ↓

let x = New(10, 20)
let x-copy = exp-item(1, x)
let a = foo(x-copy)
let b = bar(x)
cont(a, b)
```

That is, the copying part of `exp-item` is reformulated so that it returns only a clone. This avoids creating unnecessary tuples. Although the resulting code contains multiple use of `x` now, this is safe because we reformulate `exp-item` so that it won't deallocate `x`.

All in all, the actual implementation of `exp-item` will be something like below:

```neut
define exp-item(selector, v) {
  if selector == 0 {
    // discard `v`
    free(v)
  } else {
    // copy `v`, keeping the original `v` intact
    let ptr = malloc({2-words})
    let v1 = v[0]
    let v2 = v[1]
    ptr
  }
}
```

Or, more generally, the type `A` is translated into something like below:

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
