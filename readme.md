[![Tests on Linux](https://github.com/vekatze/neut/actions/workflows/linux.yaml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/linux.yaml)
[![Tests on macOS](https://github.com/vekatze/neut/actions/workflows/macos.yaml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/macos.yaml)
[![Book](https://github.com/vekatze/neut/actions/workflows/deploy-book.yml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/deploy-book.yml)

# Neut

Neut is a functional programming language with static memory management.

## Key Features

- Full λ-calculus support
- Predictable automatic memory management
- The absence of annotations to the type system when achieving both of the above

Neut doesn't use GCs or regions. Instead, it takes a type-directed approach to handle resources. It even uses the T-necessity operator to formulate borrowing.

## Basic Characteristics

- Call by value
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html) and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + T-necessity + fix - universe hierarchy
  - That is, the usual one in functional programming, but a bit generalized
- Built-in [LSP support](https://vekatze.github.io/neut/lovely-lsp-showcase.html)
- Built-in [rapid prototyping experience](https://vekatze.github.io/neut/rapid-prototyping.html) like scripting languages
- Built-in formatter like Go

## Documentation

Please see: https://vekatze.github.io/neut/
