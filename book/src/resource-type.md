# Resource Type

You can define a type by specifying what should happen when copying/discarding a type. This can be done using `resource`:

```neut
resource type-name {
- (pointer: int) => {
    // discard `pointer: type-name` here
    0 // return any int; this is just a placeholder, and has no significance
  }
- (pointer: int) => {
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
- (v: int) => {
    print("discarded!\n") // print a message for a try
    free(v)
    0
  }
- (v: int) => {
    let orig-value = load-int(v)
    let new-ptr = malloc(1)
    magic store(int, new-ptr, orig-value)
    new-ptr
  }
}

// provide a way to introduce new boxed integer
define create-new-boxed-int(x: int): boxed-int {
  let new-ptr = malloc(1)
  store-int(new-ptr, x)
  magic cast(int, boxed-int, new-ptr)
}
```

A value of type `boxed-int` prints `"discarded!\n"` when the value is discarded.

Resource types can be used to implement some low-level types. Indeed, `i8-array`, `text`, and `vector` in the core library are defined using `resource`.
