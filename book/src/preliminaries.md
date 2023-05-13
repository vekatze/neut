# Preliminaries

In this section, you'll see how to create, build, and run a module. we'll also see what will happen when a variable is used multiple times.

## Creating a Module

Run the following command to create your first module:

```neut
neut create hello
cd hello
```

The module should contain a file `hello.nt` with the following content:

```neut
define main(): int {
  0
}
```

Rewrite the file to something like the below:

```neut
define main(): int {
  print("Hi!\n")
  0
}
```

Then compile and run the module:

```sh
$ neut build --execute
# => Hi!
```


## Building a Module

At the root of your module, you should find `module.ens`. It should contain something like below:

```neut
target = {
  hello = "hello.nt"
}
```

This defines the entry point for a target named `hello`. The command `neut build TARGET` starts compilation assuming that the entry point is in `"hello.nt"`. That is, the file `"hello.nt"` must define a function named `main`.

If you omit `TARGET`, `neut build` simply builds all the targets.

If you pass `--execute`, `neut build` runs the resulting executable after building it.

Incidentally, the paths of source files specified in `module.ens` is relative to `/source/`.

## Structure of a Source File

The basic structure of a source file is as follows:

```neut
// import other files
import {
- foo.bar
- this.buz
}

// export names defined in this file
export {
- func
}

// ... and here comes the main section.
// We'll define functions and types here.

define func(arg-1: int): int {
  arg-1
}

// ...
```

The `import` and `export` are covered later. Here, we'll see basic programming contents, like defining functions, using recursions, etc.

Also, the order of functions doesn't matter in Neut.

## Linearization of Variable Usage

In Neut, a variable is copied/discarded when used non-linearly. Consider the following code:

```neut
define foo(): my-list(int) {
  let c = MyCons(1, MyCons(2, MyNil))
  // (X)
  let c1 = c
  // (Y)
  let c2 = c
  // (Z)
  c
}
```

In this example, since the variable `c` is used three times, `c` is copied twice. This will happen immediately after a variable is defined. In our example, the content of `c` is copied twice at `(X)`.

Also, since the variables `c1` and `c2` aren't used, the contents of them are discarded. Again, this will happen immediately after a variable is defined. In our running example, the content of `c1` is discarded at `(Y)`, and that of `c2` is discarded at `(Z)`.

Try to use variables as linearly as possible for better performance. When you can't avoid non-linearity, you'll use a noema, which is explained later in this chapter. Before that, let's see how the non-noetic fragment of the language behaves.
