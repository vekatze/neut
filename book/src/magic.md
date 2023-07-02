# Magic

In Neut, you can cast some magic spells, as follows:

```neut
// arbitrary cast
let v-casted = magic cast(old-type, new-type, v) in
cont

// calling external function
let ptr = magic external(malloc, 3) in // allocate 3 bytes
cont

// store a value to memory (the `store` in LLVM)
magic store(int64, 10, ptr);
cont

// load a value from memory (the `load` in LLVM)
let v = magic load(int64, ptr) in
cont
```

`magic` can be exploited to realize, for example, platform-dependent behaviors.

When you call an external function using `magic external`, you must declare its type beforehand:

```neut
import {...}

declare {
- arc4random_uniform(int32): int32
- func-name(arg-type-1, ..., arg-type-n): result-type
}
```

## Notes on Types

Except for `cast`, the resulting type of a `magic` is unknown. Thus, you often need to annotate types like the below:

```neut
let result: int = magic load(int64, ptr) in
cont
```

## On the Syntax of Store and Load

The first arguments of `store` and `load` must be `low-type`, where:

```neut
low-type ::= integer-type (= int1, int2, int3, ..., int64)
           | float-type (= float16, float32, float64)
           | pointer // LLVM's opaque pointer type
```

You'll also use this `low-type` when you declare an external function.
