# Functional Programming

What's interesting about Neut is its resource management. Towards that, here we'll briefly see its aspect as a functional programming language.

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

`neut create project-name`

Then explain its content (`module.ens` and `hello.nt`).

Add files and define functions, and then rewrite the `hello.nt`.

### Branching Using `if`

### Branching using `match`

### Defining and Using Functions

### Recursion

## Build & Execute

- `neut build`
- `neut build --execute`
- `neut build --install bin`

## Releasing your Module

- `neut archive x-y-z`
- `git add archive/x-y-z.tar.zst`
- `git push origin main`

## Adding an External Module

- `neut add ALIAS URL`

## What You've Learned Here

- `neut create MODULE_NAME` can be used to create a project

Let's go to the next, interesting part -- resource management.
