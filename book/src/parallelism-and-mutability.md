# Parallelism and Mutability

Here, we'll see how parallelism in Neut works. Parallel control flows can communicate messages using channels. We'll also see how these channels can be used as mutable variables.

## Detaching/Attaching Control Flows

Neut has a feature for parallelism. It is a thin layer over pthread, and works like async/await:

```neut
let f1: flow(int) =
  detach { // like async
    print("fA")
    1
  }
let f2: flow(int) =
  detach { // like async
    print("fb")
    2
  }
let v1 = attach f1 // like await
let v2 = attach f2 // like await
print("hey")
```

When `detach { ... }` is executed, a new thread is created and the content of `detach` is executed in the thread; `detach` detaches the specified control flow from the current control flow.

The type of `detach { e }` is `flow(a)` if the type of `e` is `a`. Other languages might call this type a promise or a future.

`attach e` is like await. `attach` waits for the control flow `e` to complete, and get its resulting value. Thus, the type of `attach e` is `a` when `e: flow(a)`.

## Communication Between Control Flows

Flows can send/receive values using channels. The channels in Neut are similar to those of Go.

You can create a channel using `let-on`, and send/receive values using those channels.

```neut
let ch0 = new-channel(int)
let ch1 = new-channel(int)
// channels as queues
let result on ch0, ch1 = {
  let f =
    detach {
      let message0 = receive(ch0)
      send(ch1, add-int(message0, 1))
      message0
    }
  let g =
    detach {
      let message1 = receive(ch1)
      add-int(message1, 1)
    }
  send(ch0, 0)
  let v1 = attach f // v1 == 1
  let v2 = attach g // v2 == 2
  print("hey")
}
// ... cont ...
```

The type of a channel is `channel(a)`, where the `a` is the type of values that are sent/received. You'll typically use the noema of a channel because both `send` and `receive` expect the noema of a channel.

<!-- A channel isn't copied/discarded even if it is used non-linearly; A channel, including its content, is discarded after its `let-on`. -->

You can send a value into a channel using `send`, and receive one using `receive`.

A channel internally has a queue, and `send` stores a value to that queue.

When you call `receive`, if the queue isn't empty, the first element of the queue is extracted (the element is deleted from the queue). Otherwise, `receive` blocks until a value is sent to the queue.

As mentioned above, the channels in Neut are similar to those of Go (and indeed inspired by Go to some extent), but actually, the main inspiration was from [Par Means Parallel: Multiplicative Linear Logic Proofs as Concurrent Functional Programs](https://dl.acm.org/doi/10.1145/3371086).

## Mutable Variables

`channel(a)` can be used as a basis for mutable variables. The idea is to create a channel that is always of length 1. The type `cell(a)` is a wrapper of such a channel:

```neut
define sample(): int {
  let xs: list(int) = []

  // create a new cell
  let xs-cell = new-cell(xs)

  // create a noema of a cell
  let result on xs-cell = {
    // mutate the cell (add an element)
    mutate(xs-cell, (xs) => { 1 :: xs })

    // get the length of the list in the cell
    let len1 = borrow(xs-cell, (xs) => { length(xs) })
    // (len1 == 1)

    // mutate again
    mutate(xs-cell, (xs) => { 2 :: xs })

    // get the length of the list in the cell, again
    let len2 = borrow(xs-cell, (xs) => { length(xs) })
    // (len2 == 2)

    ...
  }
  ...
}
```

Here, the type of related wrapper functions are:

```neut
// create a new channel
new-cell[a](x: a): cell(a)

// mutate the content of a cell
mutate[a](ch: &cell(a), f: a -> a): top

// borrow the content of a cell and do something
borrow[a, b](ch: &cell(a), f: &a -> b): b

// clone the content of a cell
clone[a](ch: &cell(a)): a
```

The definition of, for example, `mutate` is like the below:

```neut
define mutate[a](ch: &cell(a), f: a -> a): top {
  let ch = magic.cast(&cell(a), &channel(a), ch)
  let v = receive(ch)
  send(ch, f(v))
}
```
