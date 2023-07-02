# Preliminaries

In this section, we'll see how to create, build, and run a module. We'll also see what will happen when a variable is used non-linearly.

## Working with a Module

### Creating a Module

Run the following command to create your first module:

```neut
neut create hello
cd hello
```

The module should contain a file `hello.nt` with the following content:

```neut
define main(): unit {
  print("Hello, world!\n")
}
```

Let's compile and execute it:

```sh
$ neut build --execute
# => Hello, world!
```


### Building a Module

At the root of your module, you should find `module.ens`. It should contain something like:

```neut
target = {
  // TARGET = "entry-point.nt"
  hello = "hello.nt"
  foo = "bar.nt"
  // ...
}
```

This defines the entry point for a target named `hello`. Every file declared here must define a function `main`, which in turn is used by the command `neut build TARGET`; This command starts compilation assuming that a function `main` is in `hello.nt`.

<!-- That is, the file `"hello.nt"` must define a function named `main`. -->

If you omit `TARGET`, `neut build` simply builds all the targets.

If you pass `--execute`, `neut build` runs the resulting executable after building it.

If you pass `--install DIR`, the resulting binary is copied into the directory `DIR`.

Incidentally, the paths of source files specified in `module.ens` are relative to `/source/`.

## Structure of a Source File

The basic structure of a source file is as follows:

```neut
// import other files
import {
- foo.bar
- this.buz
}

// define functions and types
// (the order of functions and types doesn't matter in Neut)

define func(arg-1: int): int {
  arg-1
}

// ...
```

## Linearization of Variables

In Neut, a variable is copied/discarded when used non-linearly. Consider the following code:

```neut
define foo(): list(int) {
  let xs = [1, 2] in
  // (X)
  let ys = xs in
  // (Y)
  let zs = xs in
  // (Z)
  c
}
```

In this example, since the variable `xs` is used three times, `xs` is copied twice. This will happen immediately after a variable is defined. In our example, `xs` is copied twice at `(X)`.

Also, since the variables `ys` and `zs` aren't used, the contents of them are discarded. Again, this will happen immediately after a variable is defined. In our running example, the content of `ys` is discarded at `(Y)`, and `zs` at `(Z)`.

Try to use variables linearly when possible for better performance. When you can't avoid non-linearity, you can try using a noema, which is explained later in this chapter. Before that, let's see how the non-noetic fragment of the language behaves.
