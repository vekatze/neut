# Executing a Function Type

Here, we'll see how a closure is compiled in Neut. Then, we'll see how a function type is translated.

## Compiling a Lambda

Suppose we have a function like the below:

```neut
define foo(a: tau): int {
  let x: int = 10
  let y = tau
  let f =
    (z: a) => {   // lambda function
      let foo = x // ← x is a free var of this lambda
      let bar = y // ← y is also a free var of this lambda
      let buz = z
      bar
    }
  0
}
```

Let's see how the `lambda` inside the function is compiled.

### Extracting a Closed Chain From a Lambda

First of all, the compiler collects all the free variables in the lambda. Here, the compiler also collects all the free variables in the types of the free variables. Thus, in this case, the compiler constructs a list like below:

```neut
[a, x, y, z]
```

Note that this list is "closed". That is, consider annotating all the variables in the list by their variables, like below:

```neut
[a: tau, x: int, y: tau, z: a]
```

This list is closed in that the term

```neut
(a: tau, x: int, y: tau, z: a) => { Unit }
```

doesn't contain any free variables. We'll call a list like this a closed chain.

### Closure Conversion

We'll use this closed chain to compile a lambda. The internal representation of a closure for the lambda will be a 3-word tuple like below:

```text
(Σ (a: tau, x: int, y: tau). a , (a, x, y, z), LABEL-TO-FUNCTION-DEFINITION)
 -----------------------------   ------------
 the type of the environment     the closed chain (i.e. environment)
```

This is more or less the usual closure conversion, except that we now have the type of the environment in the closure.


## Compiling a Function Type

### Copying/Discarding a Closure

Using that type information, we can now copy/discard a closure. For example, to copy a closure, we can do the following:

```neut
// copy a closure `cls`

let env-type = cls[0] // get the type of the environment
let env      = cls[1] // get the pointer to the environment
let label    = cls[2] // get the label to the function

let env-clone = env-type(1, env) // copy the environment using the type of it

// allocate new memory region for our new closure
let new-ptr = malloc(mul-int(3, word-size))

// store cloned values
store(env-type, new-ptr[0])  // remember that a type is an immediate
store(env-clone, new-ptr[1])
store(label, new-ptr[2])     // note that a label is an immediate

new-ptr // ... and return the new closure
```

Discarding a closure can also be done with the same idea: discard the environment using the type information in the closure.

### Translating a Function Type

This leads us to translate the function type as follows:


```neut
(x1: A1, ..., xn: An) -> B

// ↓

define exp-closure(action-selector, cls) {
  if action-selector == 0 {
    // discard

    // discard the environment using the type of it
    let env-type = cls[0]
    let env      = cls[1]
    env-type(0, env)

    // discard the tuple of the closure
    free(cls)
  } else {
    // copy

    // get the original values
    let env-type = cls[0]
    let env      = cls[1]
    let label    = cls[2]

    // copy the environment using the type of it
    let env-clone = env-type(1, env)

    let new-ptr = malloc(mul-int(3, word-size))
    // copy the original values
    store(env-type, new-ptr[0])
    store(env-clone, new-ptr[1])
    store(label, new-ptr[2])

    // ... and return the new closure
    new-ptr
  }
}
```

Note that every function type is translated into the same function no matter what its argument types and the result type are.
