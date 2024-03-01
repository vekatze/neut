# Functional Programming

What's interesting about Neut resides in its resource management. Towards that, here we'll briefly see its aspect as a functional programming language.

I don't really know what "functional" means, though.

The code in this section can be obtained by:

```sh
git clone https://github.com/vekatze/XXX.git
```

Here I'll assume that you are somewhat familiar with other functional programming languages like Haskell, OCaml, or F#. You'll be confused if you don't know what ADT means, for example.

## What You'll Learn Here

We'll explore Neut's basic aspects as a functional programming language. Namely:

- How to _create_ a module (== library == crate == package == whatever = ..)
- How to _build_ a module
- How to _execute_ a module
- How to _add_ dependencies
- How to _publish_ your module

## Creating a Module

### Initialization

Run `neut create sample`. This command creates a new directory `./sample/` and files inside the directory. This is a module in Neut.

The command `create` creates a sample project that does "hello world". This can be built and executed as follows:

```sh
cd path/to/sample
neut build --execute # => "Hello, world!" (might take seconds for the first time)
```

Let's see what's a module in Neut like.

### Structure

The content should be something like the below:

```text
sample/
├── build/
│  └── ...
├── source/
│  └── sample.nt
└── module.ens
```

The directory `build` is the directory where object files (binary files) are put in. You won't have to go into the directory for daily use.

The directory `source` is the directory where source files are put in. This will be our focus.

The file `module.ens` contains meta information of this project like dependencies.

<div class="info-block">

Directories like `build` or `source` can be changed to somewhere else using `module.ens`. See [Modules](./modules.md) for more information.

</div>

### module.ens

The content of `module.ens` should be something like the below:

```ens
{
  target {
    sample "sample.nt"
  }
  dependency {
    core {
      digest "ocDmPr9kkTZJMkJnYpZGrX8-skEB0YUCls5HeWSb7r8"
      mirror [
        "https://github.com/.../X-Y-Z.tar.zst"
      ]
      enable-preset true
    }
  }
}
```

`target` specifies the name and the entry point of resulting executables. In the case above, `neut build` will create an executable file `sample` by compiling sources using the `main` function in `sample.nt` as the entry point.

`dependency` specifies external dependencies. Since our project doesn't do much, the only dependency is `core`, which is the same as "prelude" in other languages.

### Source files

Let's see the content of `sample.nt`:

```neut
// sample.nt

define main(): unit {
  print("Hello, world!\n") // `print` is defined in `core`
}
```

This defines a function `main` that returns a value of type `unit`. This function just prints `"Hello, world!\n"` and returns.

Here, the `unit` is the same as the `()` in Haskell. That is, `unit` is an ADT that contains only one value `Unit`. The explicit definition of `unit` is as follows

```neut
data unit {
- Unit
}

// The Haskell equivalent of the above is (ignoring variable naming conventions):
//   data unit =
//     Unit
```

Let's see how development experience works. Edit the code into the following:

```neut
// sample.nt

define main(): unit {
  print-int(42) // `print-int` is also defined in `core`
}
```

and build the project:

```sh
neut build --execute # => 42
```

You can also obtain the resulting binary:

```sh
neut build --install ./bin # creates a directory `bin` and put the resulting binary there
./bin/sample # => 42
```

### Defining a function

Of course, you can define a function:

```neut
// sample.nt

define get-int(): int {
  42
}

define main(): unit {
  print-int(get-int())
}
```

A function can take arguments:

```neut
// sample.nt

define increment(x: int): int {
  add-int(x, 1)
}

define my-add(x: int, y: int): int {
  add-int(x, y)
}

define main(): unit {
  print-int(my-add(10, increment(10))) // # => 21
}
```

Top-level items like `define` are called **statements**.

<div class="info-block">

Like F#, statements in Neut are order-sensitive. Because of that, if you were to define `main` before `my-add`, the program won't compile. For forward references, you'll have to explicitly declare names beforehand using a statement called `nominal`, which we'll see later.

</div>

## Publishing Your Module

I'd like to tell you now that you already have a function `my-add` that can be used by others. Let's try publishing it.

You can create an archive of the current module using `neut archive`:

```sh
neut archive 0-1
ls ./archive # => 0-1.tar.zst
```

`neut archive` creates the directory `archive` at the root of your module. This command also creates an archive for the current module.

The name of a module archive must be something like `0-1`, `2-3-8`, `1-2-3-4-5-6`.

Names of archives are interpreted as semantic versions to save compatibility relations. For example, if you create an archive `1-2-3`, and then `1-2-4`, the `1-2-4` is treated as a minor update of `1-2-3`.

This tarball can be controlled with your version control system like Git and pushed to the remote repository, as usual:

```sh
# the usual git thing

pwd # => path/to/sample/

git init
git remote add origin git@github.com:YOUR_NAME/YOUR_REPO_NAME.git
git push origin master

git commit --allow-empty -m "initial commit"
git add module.ens build/ source/ archive/ # ← `archive/` is added here
git commit -m "whatever"
```

The tarball is now controlled by GitHub in this case. This tarball can then be used as a dependency, as described in the next section.

## Adding Another Module to Your Module

`neut get` can be used to add external dependencies. Given that the `0-1.tar.zst` that we created in the previous section is pushed to GitHub as in the previous section, the tarball can be used as follows:

```sh
pwd # => path/to/sample/
neut create new-item # let's create a new module
cd new-item

# ↓ add the previous module to our `new-item`
neut get some-name https://github.com/YOUR_NAME/YOUR_REPO_NAME/raw/main/archive/0-1.tar.zst
```

(For convenience, you can use this URL instead of creating a repository just to test this behavior)

The command `neut get` fetches the tarball from the specified URL and adds it to the current module.

The information of the newly-added module is saved to `module.ens`:

```ens
{
  target {
    new-item "new-item.nt"
  }
  dependency {
    core { .. }
    // ↓ HERE
    some-name {
      digest ".."
      mirror [
        "https://github.com/YOUR_NAME/YOUR_REPO_NAME/raw/main/archive/0-1.tar.zst"
      ]
    }
  }
}
```

<div class="info-block">

In Neut, the name of a module is defined by its user, not by its author. In our running example, we developed our module under the name of `sample`, but the user added the module as `some-name`. Modules in Neut are distinguished by their digests.

</div>

<div class="info-block">

The identity of a module is defined only by the minor version equivalence. You can "update" your dependencies of version `0-2-4`, for example, by specifying the same module of version `0-2-5`, `0-2-6`, etc.

</div>

## Using Dependencies

These specified dependencies can then be used in your code, using `import`:

```neut
// new-item.nt

import {
- some-name.sample {my-add}
}

define main(): unit {
  print-int(my-add(10, 11)) // ← using `my-add`
}
```

Like `some-name.sample` in the example above, we can use `import` to specify files of a dependency that we want to use.

The first component of an element in `import` is our alias of the dependency. What follows is the relative path to the file from the root of the source directory of the dependency module.

Like `{my-add}` in the example above, every element of an `import` can optionally have a list of names. Names in these lists can then be used after `import`, as in the example above.

If you didn't specify `{my-add}`, you'd have to use the fully-qualified form of `my-add`:

```neut
// new-item.nt

import {
- some-name.sample // removed `{my-add}`
}

define main(): unit {
  // ↓ using the fully-qualified form of `my-add`
  print-int(some-name.sample.my-add(10, 11))
}
```

What if the file `sample.nt` isn't at the root of the source directory?

Suppose that the dependency `some-name` had contained a file `source/entity/item.nt`. In this case, the file `item.nt` can be imported from `new-item.nt` as follows:

```neut
import {
- some-name.entity.item
}
```

## Importing Files in the Current Module

We now know how to import files in other dependencies. Using the same idea, we can also (of course) import files in the current module.

Let's create a new file `new-item/source/foo/greet.nt` with the following content:

```neut
// greet.nt

define yo(): unit {
  print("Yo")
}
```

This file can then be used from `new-item/source/new-item.nt` as follows:

```neut
// new-item.nt

import {
- this.foo.greet {yo}
}

define main(): unit {
  yo()
}
```

That is, the name of the current module is always `this`.

`import` can import multiple files and multiple names at the same time, of course. For example, the following is a valid use of `import`:

```neut
import {
- this.foo.greet {yo}
- some-name.entity.item {add}
}
```

## Prefixed Import

We can also use so-called qualified imports as in Haskell. Let's remember the example of fully-qualified names:

```neut
// new-item.nt

import {
- some-name.sample // removed `{my-add}`
}

define main(): unit {
  // ↓ using the fully-qualified form of `my-add`
  print-int(some-name.sample.my-add(10, 11))
}
```

We'll rewrite this example into a "prefixed" form. Firstly, edit the `module.ens` as follows:

```ens
{
  prefix {                 //
    S "some-name.sample"   // ← alias: S -> some-name.sample
  }                        //
  target {
    new-item "new-item.nt"
  }
  dependency {..}
}
```

By registering aliases of files in `module.ens`, you'll be able to use prefixes as aliases for specified files.

We can now rewrite the `new-item.nt` as follows:

```neut
// new-item.nt

import {
- S // == some-name.sample
}

define main(): unit {
  print-int(S.my-add(10, 11))
}
```

<div class="info-block">

Unlike Haskell, these prefixes are defined per module, not per file. The prefixes of a file are consistent inside a module.

</div>

## Programming in Neut

Let's code in Neut. We'll see how to write basic programs in Neut here. We assume that you are already familiar with functional programming.

Let's create a new module `start` and edit `source/start.nt`:

```sh
neut create start
cd start
edit source/start.nt
```

As a simple example, we'll write an interpreter for the untyped lambda calculus.

### Binding Variables Using `let`

Rewrite `start.nt` into the below:

```neut
define hey(): unit {
  let x = "hello" in
  let y: int = 100 in
  let z: float = 3.8 in
  print("hey")
}

define main(): unit {
  hey()
}
```

Building and executing the module should output `hey` to the stdout.

Points:

- Functions can be defined using `define`
- Functions can be called by writing `f(e1, ..., en)`
- `let` can be used to define variables
- `let` can be nested
- The name `_` can be used to suppress the compiler

As you can see from the example above, `let` can be used to define variables.

You might have noticed that the compiler reports unused variables (`x`, `y`, and `z` in the example above). You can use the name `_` when defining variables to suppress those warnings:

```neut
define hey(): unit {
  let _ = "hello" in
  let _: int = 100 in
  let _: float = 3.8 in
  print("hey")
}
```

`let`s can be nested:

```neut
define hey(): unit {
  let x =
    let y: int = 100 in
    let z: float = 3.8 in
    "hello"
  in
  print(x) // => hello
}
```

`e1; e2` is a syntax sugar of `let _: unit = e1 in e2`:

```neut
define hey(): unit {
  print("a");
  print("b")
}

// ↓

define hey(): unit {
  let _ = print("a") in
  print("b")
}
```

### Calling Functions

Functions `f` can be called against arguments `e1`, ..., `en` by writing `f(e1, ..., en)`:

```neut
define my-func(x: int, y: int): int {
  add-int(x, y)
}

define use-my-func(): int {
  my-func(10, 20)
}
```

The syntax sugar `of` can be used to rewrite the above `use-my-func` into the below:

```neut
define use-my-func(): int {
  my-func of {
  - x = 10
  - y = 20
  }
}
```

### Defining ADTs

You can use the statement `data` to define ADTs:

```neut
data my-nat {
- My-Zero
- My-Succ(my-nat)
}
// Haskell Equivalent:
//   data my-nat
//     = My-Zero
//     | My-Succ my-nat


//------------


data my-list(a) {
- My-Nil
- My-Cons(a, my-list(a))
}
// Haskell Equivalent:
//   data my-list a
//     = My-Nil
//     | My-Cons a (my-list a)
```

Arguments in constructors can optionally have explicit names:

```neut
data config {
- Config(count: int, path: &text, status: my-status)
}
```

The syntax sugar `of` can be used to rewrite the above definition of `config` into:

```neut
data config {
- Config of {
  - count: int
  - path: &text
  - status: my-status
  }
}
```

```neut
data term {
- Var(text)
- Abs(text, term)
- App(term, term)
}
```

### Creating ADT Values

You can use constructors as usual functions. Add the following to `start.nt`:

```neut
define make-var(x: text): term {
  Var(x)
}
```

We've just used the constructor `Var` here.

### Using ADTs (i.e. Branching)

You can use `match` to deconstruct ADT values:

```neut
define sum(xs: my-list(int)): int {
  match xs {
  - My-Nil =>
    0
  - My-Cons(y, ys) =>
    add-int(y, sum(ys))
  }
}
```

Nested matching can also be used. Add the following to `source/reduce.nt`:

```neut
define reduce(t: term): term {
  match t {
  - App(Abs(x, t1), t2) =>
    subst([Pair(x, t2)], t1)
  }
}
```

You can use `if` as usual:

```neut
define factorial(n: int) {
  if le-int(n, 0) { // `le-int(n, 0)` means `n <= 0`
    1
  } else {
    mul-int(n, sub-int(n, 1)) //  n * (n - 1)
  }
}
```

The result of `if` can be

## What You've Learned Here

- `neut create MODULE_NAME` can be used to create a project

Let's go to the next, interesting part -- resource management.
