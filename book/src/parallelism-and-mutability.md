# Parallelism and Mutability

## Detaching/Attaching Control Flows

Neut has built-in support for parallelism. It is a thin layer over pthread, and works like async/await:

```neut
let f1: flow(int) =
  detach { // like async
    print("fA");
    1
  }
in
let f2: flow(int) =
  detach { // like async
    print("fb");
    2
  }
in
let v1 = attach f1 in // like await
let v2 = attach f2 in // like await
print("hey")
```

`detach` is like async in other languages. `detach` detaches the specified control flow from the current control flow. More specifically, when `detach { ... }` is executed, a new thread is created and the content of `detach` is executed in the thread.

The type of `detach { e }` is `flow(a)` if `e: a`. Other languages might call this type a promise or a future.

`attach e` is like await. `attach` waits for the control flow `e` to complete, and gets its resulting value. The type of `attach e` is `a` if `e: flow(a)`.

## Communication Between Control Flows

Flows can send/receive values using channels. A channel in Neut is similar to that of Go.

You can create a channel using `new-channel`, and send/receive values using those channels.

```neut
let ch0 = new-channel(int) in
let ch1 = new-channel(int) in
// channels as queues
let result on ch0, ch1 =
  let f =
    detach {
      let message0 = receive(_, ch0) in // receive value from ch0
      send(int, ch1, add-int(message0, 1)); // send value to ch1
      message0
    }
  in
  let g =
    detach {
      let message1 = receive(_, ch1) in // receive value from ch1
      add-int(message1, 1)
    }
  in
  send(int, ch0, 0); // send value to ch0
  let v1 = attach f in
  let v2 = attach g in
  print("hey")
in
// ... cont ...
```

The type of a channel is `channel(a)`, where the `a` is the type of values that are sent/received. You'll typically use the noema of a channel because both `send` and `receive` expect the noema of a channel.

You can send a value into a channel using `send`, and receive one using `receive`.

A channel internally has a queue, and `send` stores a value to that queue.

When you call `receive`, if the queue isn't empty, the first element of the queue is extracted (the element is deleted from the queue). Otherwise, `receive` blocks until a value is sent to the queue.

Incidentally, as mentioned above, a channel in Neut is similar to that of Go (and indeed inspired by Go to some extent), but actually, the main inspiration was from [Par Means Parallel: Multiplicative Linear Logic Proofs as Concurrent Functional Programs](https://dl.acm.org/doi/10.1145/3371086).

## Mutable Variables

`channel(a)` can be used as a basis for mutable variables. The idea is to create a channel that is always of length 1. The type `cell(a)` is a wrapper of such a channel:

```neut
define sample(): int {
  let xs: list(int) = [] in

  // create a new cell using `new-cell`
  let xs-cell = new-cell(list(int), xs) in

  // create a noema of a cell
  let result on xs-cell =
    // mutate the cell using `mutate` (add an element)
    mutate(_, xs-cell, (xs) => { 1 :: xs });

    // peek the content of a cell using `borrow`
    let len1 = borrow(_, _, xs-cell, (xs) => { length(xs) }) in
    // (len1 == 1)

    // mutate again
    mutate(_, xs-cell, (xs) => { 2 :: xs });

    // get the length of the list in the cell, again
    let len2 = borrow(_, _, xs-cell, (xs) => { length(xs) }) in
    // (len2 == 2)

    ...
  in
  ...
}
```

Here, the type of related wrapper functions are:

```neut
// create a new channel
new-cell(a: tau, x: a): cell(a)

// mutate the content of a cell
mutate(a: tau, ch: *cell(a), f: a -> a): top

// borrow the content of a cell and do something
borrow(a: tau, b: tau, ch: *cell(a), f: *a -> b): b

// clone the content of a cell
clone(a: tau, ch: *cell(a)): a
```

The definition of, for example, `mutate` is like the below:

```neut
define mutate(a: tau, ch: *cell(a), f: a -> a): top {
  let ch = magic.cast(*cell(a), *channel(a), ch) in
  let v = receive(a, ch) in
  send(a, ch, f(v))
}
```
