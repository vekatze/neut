[![Tests on Linux](https://github.com/vekatze/neut/actions/workflows/linux.yaml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/linux.yaml)
[![Book](https://github.com/vekatze/neut/actions/workflows/deploy-book.yml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/deploy-book.yml)

# Neut

Neut is a dependently-typed programming language with static memory management.

## Key Features

- Full λ-calculus support
- Static memory management
- The absence of annotations to the type system when achieving both of the above

I believe the last one is particularly interesting, as it means Neut found memory predictability inside the usual λ-calculus.

## Basic Characteristics

- Call by value (i.e. non-lazy)
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - That is, the usual one in functional programming, but a bit generalized

## Documentation

Please see: https://vekatze.github.io/neut/
