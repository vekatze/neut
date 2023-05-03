# Parallelism and Mutability

Here, we'll see how parallelism in Neut works. Parallel control flows can communicate messages using channels. We'll also see how these channels can be used as mutable variables.

## Detaching/Attaching Control Flows

Neut has a feature for parallelism. It is a thin layer over pthread, and works like async/await:

```neut
let f1: flow(i64) = detach { // like async
  print("fA")
  1
}
let f2: flow(i64) = detach { // like async
  print("fb")
  2
}
let v1 = attach f1 // like await
let v2 = attach f2 // like await
print("hey")
```

When `detach { ... }` is executed, a new thread is created and the content of `detach` is executed in the thread; `detach` detaches the specified control flow from the current control flow.

The type of `detach { e }` is `flow(a)` if the type of `e` is `a`. Other languages might call this type a promise or a future.

`attach e` is like await. `attach` waits the control flow `e` to complete, and get its resulting value. Thus, the type of `attach e` is `a` when `e: flow(a)`.

## Communication Between Control Flows

Flows can send/receive values using channels. The channels in Neut resemble to those of Go.

You can create a channel using `let-on`, and send/receive values using those channels.

```neut
// channels as queues
let result on channel c0, channel c1 = {
  let f = detach {
    let message0 = receive c0
    send c1 <= add-i64(message0, 1)
    message0
  }
  let g = detach {
    let message1 = receive c1
    add-i64(message1, 1)
  }
  send c0 <= 0
  let v1 = attach f // v1 == 1
  let v2 = attach g // v2 == 2
  print("hey")
}
// ... cont ...
```

The type of a channel is `channel(a)`, where the `a` is the type of values that are sent/received. A channel isn't copied/discarded even if it is used non-linearly; A channel, including its content, is discarded after its `let-on`.

You can send a value into a channel using `send`, and receive a value from a channel using `receive`. A channel internally has a queue, and `send` stores a value to that queue.

When you call `receive`, if the queue isn't empty, the first element of the queue is popped. Otherwise, `receive` blocks until a value is pushed to the queue.

As mentioned above, the channels in Neut resemble to those of Go (and indeed inspired by Go to some extent), but actually the main inspiration was from [Par Means Parallell: Multiplicative Linear Logic Proofs as Concurrent Functional Programs](https://dl.acm.org/doi/10.1145/3371086).

## Mutable Variables

`channel(a)` can be used as a mutable version of `a`. A mutation function can be defined as follows, conceptually:

```neut
define mutate[a](c: channel(a), f: a -> a): top {
  let v = receive c
  send c <= f(v)
  Unit
}
```

This `mutate` can then be used like below:

```neut
let result on channel mut-xs = {
  send mut-xs <= [1] // write
  mutate(mut-xs, lambda (xs) {
    2 :< xs
  })
  mutate(mut-xs, lambda (xs) {
    3 :< xs
  })
  let v = receive(xs) // read
  v // == [3, 2, 1]
}
```
