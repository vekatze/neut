# Resource Type

You can define a type by specifying what should happen when copying/discarding a type. This can be done using `resource`:

```neut
resource type-name {
- lambda (value: i64) {
    // discard `value: type-name` here
    0 // return any i64; this is just a placeholder, and has no significance
  }
- lambda (pointer: i64) {
    // copy `pointer: type-name` here
    copied-pointer // ... then return the address of newly-created value
  }
}
```

When compiled, `type-name(0, v)` calls the former function against `v`. `type-name(1, v)` calls the latter function against `v`.

## Example: Noisy Integers

For example, the following is a definition of a "boxed" integer type, with some noisy messages;

```neut
resource boxed-int {
- lambda (v: i64) {
    print("discarded!\n") // print a message for a try
    magic external(free, v)
    0
  }
- lambda (v: i64) {
    let orig-value: i64 = magic load(i64, v)
    let new-ptr: i64 = magic external(malloc, 1: i64)
    magic store(i64, new-ptr, orig-value)
    v
  }
}

// provide a way to introduce new boxed integer
define create-new-boxed-int(x: i64): boxed-int {
  let new-ptr = magic external(malloc, 1)
  magic store(new-ptr, x)
  magic cast(i64, boxed-int, new-ptr)
}
```

A value of type `boxed-int` prints `"discarded!\n"` when the value is discarded.

Resource types can be used to implement some low-level types. Indeed, `i8-array`, `text`, and `vector` in the core library are defined using `resource`.
