[![Tests on Linux](https://github.com/vekatze/neut/actions/workflows/linux.yaml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/linux.yaml)
[![Tests on macOS](https://github.com/vekatze/neut/actions/workflows/macos.yaml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/macos.yaml)
[![Book](https://github.com/vekatze/neut/actions/workflows/deploy-book.yml/badge.svg)](https://github.com/vekatze/neut/actions/workflows/deploy-book.yml)

# Neut

Neut is a functional programming language with static memory management.

## Key Features

- Full λ-calculus support
- Predictable automatic memory management
- The ability to achieve both of the above without extra type annotations

Neut doesn't use a GC. Instead, it takes a type-directed approach to memory management.

## Basic Characteristics

- Call-by-value
- Impure
- Compiles to LLVM IR and native binaries
- The type system ≈ System Fω + ADT + recursion + box modality
  - That is, the usual one in functional programming, but a bit generalized
- Built-in [LSP support](https://vekatze.github.io/neut/lovely-lsp-showcase.html)
- Built-in [rapid prototyping support](https://vekatze.github.io/neut/rapid-prototyping.html) as found in scripting languages
- Built-in formatter

## Documentation

Please see: <https://vekatze.github.io/neut/>
