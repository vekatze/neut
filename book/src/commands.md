# Commands

The command `neut` has subcommands like `neut build`, `neut get`, etc. This section describes those subcommands.

## Table of Contents

- [Common Notes](#common-notes)
- [neut build](#neut-build)
- [neut check](#neut-check)
- [neut clean](#neut-clean)
- [neut archive](#neut-archive)
- [neut create](#neut-create)
- [neut get](#neut-get)
- [neut format-source](#neut-format-source)
- [neut format-ens](#neut-format-ens)
- [neut zen](#neut-zen)
- [neut lsp](#neut-lsp)
- [neut version](#neut-version)

## Common Notes

### Subcommands and Modules

Most of the subcommands of `neut` must be executed inside a module. If you execute such a subcommand outside a module, the command will emit an error like the one below:

```sh
neut build foo
#=> Error: Couldn't find a module file (Context: /Users/foo/Desktop)
```

Only the following subcommands can be used outside a module:

- `neut create`
- `neut version`

### Shared command line options:

Some subcommands share command line options. The list of them is as follows:

- `--no-color` can be used to turn off ANSI colors

## `neut build`

`neut build` builds the current module and creates executables. It also creates cache files of the source files for faster compilation.

`neut build TARGET` builds the target `TARGET` defined in `module.ens`. For example, suppose that the `module.ens` of a module contains the following section:

```ens
{
  // ..
  target {
    foo {
      main "foo.nt",
    },
    bar {
      main "item/bar.nt",
    },
  },
  // ..
}
```

In this case, running `neut build foo` creates the executable `foo` by building the current module, using the `main` in `foo.nt` as the entry point.

The resulting binaries are put inside the module's build directory. You might want to use the option `--install` to copy those binaries.

### Example

```sh
# creates a sample project
neut create hello
cd hello

# build and run
neut build hello --execute # => "Hello, world!"

# build the module, copy the resulting binary, and execute the binary
neut build hello --install ./bin
./bin/hello #=> "Hello, world!"
```

### `--execute`

If you pass `--execute` to `neut build`, the resulting binaries are executed after the build.

### `--install DIR`

If you pass `--install DIR` to `neut build`, the resulting binaries are copied to the specified directory.

### `--skip-link`

By default, `neut build` builds all the source files into object files and then links them all to create an executable. You can pass `--skip-link` to `neut build` to skip the last linking phase.

### `--emit`

You can emit LLVM IR by passing `--emit llvm` to `neut build`. In this case, you must also pass `--skip-link`.

### `--mode`

You can pass `--mode {develop,release}` like the below:

```sh
neut build my-app --mode release
```

If you don't specify `--mode`, the mode defaults to `develop`.

The value passed here can be obtained from source files by using `introspect`:

```neut
define foo(): unit {
  introspect build-mode {
  | release =>
    // ..
  | develop =>
    // ..
  }
}
```

## `neut check`

`neut check` type-checks all the files in the current module. It also creates cache files of the source files for faster compilation.

## `neut clean`

`neut clean` removes the cache files in the current module's `cache` directory.

More specifically, this command removes the following directory in the current module:

```text
(cache-directory)/build/(platform)/(compiler-version)/
```

An example of a removed directory:

```text
cache/build/arm64-darwin/compiler-0.8.0/
```

## `neut archive`

`neut archive VERSION` creates a module tarball that can be used by `neut get`.

### Notes on Versions

`VERSION` must be `X1-X2-..-Xn`, where all the integers are non-negative. For example, the following are valid versions:

- `1-0`
- `0-1-0`
- `2-1-3`
- `0-0-0-3`

When running `neut archive VERSION`, this command searches the archive directory to get all the compatible older versions. For example, suppose the archive directory contains the following files:

- `1-0.tar.zst`
- `1-1.tar.zst`
- `2-0.tar.zst`
- `2-1.tar.zst`

In this case, the command `neut archive 2-2` searches the `archive` directory and gets `2-0` and `2-1` as the older compatible versions of `2-2`. Here, these "older" versions are determined according to [Semantic Versioning](https://semver.org/).

This command then does the following:

1. Computes all the digests of these older compatible tarballs
2. Creates a new `module.ens` that contains the list of the older digests
3. Packs the ens file and the other required files to create a tarball `2-2.tar.zst`

This digest information inside `module.ens` of `2-2.tar.zst` should appear like the below:

```ens
{
  target {..},
  dependency {..},
  antecedent [
    "Bp8RulJ-XGTL9Eovre0yQupJpeS3lGNk8Q6QQYua7ag", // ← digest of 2-0.tar.zst
    "zptXghmyD5druBl8kx2Qrei6O6fDsKCA7z2KoHp1aqA", // ← digest of 2-1.tar.zst
  ],
}
```

This information is then used when resolving dependencies.

You can extract a resulting tarball to see that `antecedent` information is indeed there.

<div class="info-block">

The `module.ens` in your module isn't modified by `archive`. This subcommand creates a new `module.ens` and put it into the tarball.

</div>

### Using the Newest Compatible Version

Consider the following dependency relation:

- Module `A` depends on `B` and `C`.
- Module `B` depends on `D (version 1.1)`.
- Module `C` depends on `D (version 1.2)`.

```text
A ──> B ──> D (1.1)
│
└───> C ──> D (1.2)
```

Thanks to the `antecedent` information inside `D (1.2)`, the compiler can detect that `D (1.2)` is a newer compatible module of `D (1.1)`. The compiler utilizes this knowledge to rewrite the above relation into:

```text
A ──> B ──┐
│         │
└───> C ──┴──> D (1.2)
```

The compiler always uses the newest compatible modules in the dependency relation by this procedure.

<div class="info-block">

This rewriting won't work if the minor "compatible" version `D (1.2)` isn't compatible with `D (1.1)`. This incompatibility is a bug of the library `D (1.2)` and should be fixed by the module author.

</div>

### Utilizing the Compatibility Relation

As an exercise, suppose the following:

1. Serious bugs were discovered in `D (1.1)` and `D (1.2)`
2. The grave-look author of `D` released a bug-fix release `D (1.3)` in a hurry
3. The happy authors of `B` and `C` are on a honeymoon vacation in Hawaii

In this case, we don't have to wait for the authors of `B` and `C` to update their dependencies. Instead, we can just add `D (1.3)` to our module's dependency:

```text
A ──> B ──┐
│         │
├───> C ──┴──> D (1.2)
│
└────────────> D (1.3)
```

Then, the compiler automatically rewrites the above relation into:

```text
A ──> B ───┐
│          │
├───> C ───┤
│          │
└──────────┴──> D (1.3)
```

So we don't have to rain on their happy parade in Waikiki Beach.

## `neut create`

`neut create NAME` creates a directory `./NAME/` and adds files there to start a new project.

## `neut get`

`neut get ALIAS URL` fetches and builds external modules specified by `URL` and adds it to the current module as a dependency under the name `ALIAS`.

```sh
neut get some-name https://github.com/USER_NAME/REPO_NAME/raw/main/archive/0-1.tar.zst
```

Here, the `URL` must be the URL of an archive that was created by `neut archive`.

After executing `neut get`, the information of the new module is saved to `module.ens`:

```ens
{
  target {
    // ..
  },
  dependency {
    // ..
    // 🌟 (something like this is added automatically)
    some-name {
      digest "xNmQu6It81lGBy1sKvk5_jE4Qt8w8KgkVgGj0RBbbrk",
      mirror [
        "https://github.com/USER_NAME/REPO_NAME/raw/main/archive/0-1.tar.zst",
      ],
    },
    // ..
  },
}
```

The `digest` is the base64url-encoded checksum of the tarball.

The `mirror` is a list of URLs that can be used to obtain the tarball.

<div class="info-block">

Modules in Neut are distinguished by their digests.

</div>

`neut get` can be used to "update" dependencies. Suppose you specify an existing `ALIAS` when running `neut get ALIAS URL`. In this case, if the digest of the new tarball isn't the same as the existing one, the existing dependency in the ens file is replaced by the newer one. If the digest is the same as the existing one, the `URL` is added to the list `mirror`.

You can remove a dependency in the ens file to "remove" the module from your module.

You can import dependencies from source files. See the notes on `import` in [Statements](./statements.md#import) for information on how to use definitions in external dependencies.

## `neut format-source`

`neut format-source path/to/source/file.nt` formats specified source file and outputs the result to stdout.

### `--in-place`

When the option `--in-place` is set, `format-source` performs in-place update.

### `--minimize-imports`

When the option `--minimize-imports` is set, `format-source` removes all the unused items in `import {..}`.

## `neut format-ens`

`neut format-ens path/to/source/file.ens` formats specified ens file and outputs the result to stdout.

You can also pass `-` to receive input from stdin:

```sh
cat module.ens | neut format-ens -
```

### `--in-place`

When the option `--in-place` is set, `format-source` performs in-place update.

## `neut zen`

`neut zen path/to/source/file.nt` builds and executes specified file as if it were an entry point.

The subcommand `zen` treats the function `zen` inside the given file as its entry point (that is, `main`).

Thus, the type of the function `zen` must be `() -> unit`:

```neut
define zen(): unit {
  print("hello, world!\n")
}
```

This command is intended to be used for rapid prototyping.

Please see [Rapid Prototyping](./rapid-prototyping.md) to see `neut zen` in action.

### Zen Experience

Suppose that you created a new function deep inside your module.

You can technically create a test function for the function and check its behavior. Or modify the main function to call the new function just to see its behavior.

It might be, however, a bit cumbersome. We'd love to achieve rapid try-and-error cycles in certain circumstances.

You can use the command `zen` here. Suppose that a file `some-file.nt` contains a function `foo` that is defined as in the below:

```neut
define foo(x: int): int {
  do-complex-calculation(x)
}
```

The behavior of `foo` can be inspected rapidly by defining a function named `zen` in the file:

```neut
// the type of `zen` must be `() -> unit`
define zen(): unit {
  print-int(foo(10))
}
```

Then, execute the following command:

```sh
neut zen path/to/some-file.nt # => (the result of `foo(10)` is printed)
```

This can be done even if `some-file.nt` isn't an entrypoint of the module. You can think of functions named `zen` as alternative `main`s.

## `neut lsp`

`neut lsp` starts the LSP server. The LSP server has features like linting, jump to definition, etc. More specifically, the LSP server supports the following LSP capabilities:

- `textDocument/didOpen` (lint on open)
- `textDocument/didSave` (lint on save)
- `textDocument/completion` (complete)
- `textDocument/definition` (jump to definition)
- `textDocument/documentHighlight` (highlight symbols)
- `textDocument/references` (find references)
- `textDocument/formatting` (format)
- `textDocument/hover` (show the type of a symbol)

For more information, please see [Lovely LSP Showcase](./lovely-lsp-showcase.md) and [Editor Setup](./editor-setup.md).

## `neut version`

`neut version` prints the version of the compiler.

### Example

```sh
neut version
#=> X.Y.Z
```
