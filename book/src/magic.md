# Magic

In Neut, you can cast some magic spells, as follows:

```neut
// arbitrary cast
let v-casted = magic cast(old-type, new-type, v)

// calling external function
let ptr = magic external(malloc, 3) // allocate 3 bytes

// store a value to memory
magic store(i64, ptr, 10)

// load a value from memory
let v = magic load(i64, ptr)

// system call
let a = magic syscall(SYSCALL-NUM, arg-1, ..., arg-n)

...
```

`magic` can be exploited to realize, for example, platform-dependent behaviors.

## Notes on Types

Except for `cast`, the resulting type of a `magic` is not specified. Thus, you often need to annotate types like the below:

```neut
let result: int = magic syscall(12345, arg-1, arg-2)
(...)
```

## On the Syntax of Store and Load

The first arguments of `store` and `load` must be `low-type`, where:

```neut
low-type ::= integer-type (= i1, i2, i3, ..., i64)
           | float-type (= f16, f32, f64)
           | *low-type // pointer-type
```

The actual behaviors of `store` and `load` are the same as that of [LLVM](https://llvm.org/docs/LangRef.html).
