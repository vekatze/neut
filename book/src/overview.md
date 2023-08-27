# Neut Programming Language

Neut is a dependently-typed programming language with *static memory management*.

Its key features include:

<ul class="star-list">
  <li>Full λ-calculus support</li>
  <li>Static memory management</li>
  <li><em>The absence of annotations to the type system</em> when achieving both of the above</li>
</ul>

I believe the last one is particularly interesting, as it means Neut found memory predictability *inside* the usual λ-calculus.

## How Does it Basically Look Like?

Like below:

```neut
// algebraic data types (tau = the type of types)
data my-list(a: tau) {
- Nil
- Cons(a, my-list(a))
}

// a recursive function with pattern matching
define noisy-length(a: tau, xs: my-list(a)): int {
  match xs {
  - Nil =>
    0
  - Cons(_, ys) =>
    let my-message = "hey\n" in
    print(my-message);
    add-int(1, noisy-length(a, ys))
  }
}
```

## Static Memory Management — But How?

*Neut translates a type into a function* that knows how to copy/discard the values of the type. Using those functions, every variable is copied/discarded so that it is used exactly once.

For example, if a variable is used twice, conceptually, a translation like below will happen:

```neut
let xs: list(a) = [value-1, value-2] in
some-func(xs, xs) // `xs` is used twice

// ↓

let xs: list(a) = [value-1, value-2] in
let (xs1, xs2) = copy-list-a(xs) in // now `xs` is used exactly once
some-func(xs1, xs2)
```

If you need more, see [Chapter 2 (Main Ideas)](./main-ideas.md).

---

You may be wondering: *"So we need to, for example, copy the whole list just to get its length? Isn't it the end of the world?"*. This topic is covered in [Section 2.4 (Noetic Optimization)](./noetic-optimization.md). As written there, those redundant copyings can be avoided. The idea is to add a new type `*a`, the noema type of `a`, which is the same as `a` except that it isn't copied/discarded, and to utilize it like a reference of the great ST monad.

## Quickstart?

An example scenario:

```sh
# setting up the core module (i.e. standard library)
export NEUT_CORE_MODULE_URL="https://github.com/vekatze/neut-core/raw/main/release/0-2-3.tar.zst"
export NEUT_CORE_MODULE_DIGEST="VhteQ9Jrj--kYKIFRWZTEupc17d5U8sHhcdYE2vL7Ac="

# get the compiler (choose one)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-darwin
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-amd64-linux
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-linux

# ... and make it executable
chmod +x ~/.local/bin/neut

# let's create a sample project
neut create sample
cd sample
cat source/sample.nt
# => define main(): unit {
#      print("Hello, world!\n")
#    }

# build & execute it
neut build --execute
# => Hello, world!

# build it & copy the resulting binary to ./bin
neut build --install ./bin

# ... and execute it
./bin/sample
# => Hello, world!
```

To learn more about how to use the language, follow [Chapter 3 (Language Tutorial)](./language-tutorial.md).

## List of Other Basic Characteristics?

- Call by value (i.e. non-lazy)
- Impure
- Compiles to [LLVM IR](https://llvm.org/docs/LangRef.html), assembly, and binary
- The type system ≒ [CoC](https://en.wikipedia.org/wiki/Calculus_of_constructions) + [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) + fix - universe hierarchy
  - That is, the usual one in functional programming, but a bit generalized

## Anything Else?

You might also find the module system of Neut interesting. *It distinguishes modules using the digests (checksums) of tarballs* and defines module identities using version information. Although this is not the main point of Neut (and I'm ready to retract it immediately if necessary), it still might be of interest. For more, see [Chapter 4 (Module System)](./module-system.md).

Also, Neut includes a preliminary LSP server, which provides things like code completion, error reporting on save, etc. See [Chapter 5 (Development Environment)](./development-environment) for more.
