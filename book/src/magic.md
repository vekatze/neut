# Magic

In Neut, you can cast some magic spells, as follows:

```neut
// arbitrary cast
let v-casted = magic cast(old-type, new-type, v)

// calling external function
let ptr = magic external(malloc, 3) // allocate 3 bytes

// store a value to memory (the `store` in LLVM)
magic store(i64, 10, ptr)

// load a value from memory (the `load` in LLVM)
let v = magic load(i64, ptr)
```

`magic` can be exploited to realize, for example, platform-dependent behaviors.

When you call an external function using `magic external`, you must declare its type beforehand:

```neut
import {...}

export {...}

declare {
- arc4random_uniform(i32): i32
- func-name(arg-type-1, ..., arg-type-n): result-type
}
```

## Notes on Types

Except for `cast`, the resulting type of a `magic` is not specified. Thus, you often need to annotate types like the below:

```neut
let result: int = magic syscall(12345: int, arg-1, arg-2)
(...)
```

## On the Syntax of Store and Load

The first arguments of `store` and `load` must be `low-type`, where:

```neut
low-type ::= integer-type (= i1, i2, i3, ..., i64)
           | float-type (= f16, f32, f64)
           | pointer // opaque pointer type in LLVM
```

The actual behaviors of `store` and `load` are the same as that of [LLVM](https://llvm.org/docs/LangRef.html).
