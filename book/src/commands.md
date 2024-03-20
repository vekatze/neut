# Commands

General options:

- `--no-color` can be used to disable ANSI colors

## `neut build`

`neut build` builds the current module and creates executables. It also creates cache files of the source files for faster compilation.

`neut build TARGET` builds all the targets defined in `module.ens`. For example, suppose that the `module.ens` of a module contains the following section:

```ens
{
  // ..
  target {
    foo "foo.nt"
    bar "item/bar.nt"
  }
  // ..
}
```

In this case, running `neut build foo` create the executable `foo` by building the current module, treating the `main` in `foo.nt` as the entry point.

If you omit the target and just write `neut build`, all the targets are built.

The resulting binaries are put inside the build directory of the module.

### Self-Contained Example

```sh
# creates a sample project
neut create hello
cd hello

# build and run
neut build --execute # => hello

# build the module, copy the resulting binary, and execute the binary
neut build --install ./bin
./bin/hello #=> hello
```

### `--execute`

If you pass `--execute` to `neut build`, the resulting binary is executed after the build.

### `--install DIR`

If you pass `--install DIR` to `neut build`, the resulting binaries are copied to the specified directory.

For example, if you're at the root of the module, `neut build --install ./bin` will copy the resulting binaries into `(module-root)/bin/`.

### `--skip-link`

By default, `neut build` builds all the source files into object files, and then links all of them to create an executable. You can pass `--skip-link` to `neut build` to skip the last linking phase.

### `--emit`

You can emit LLVM IR by passing `--emit llvm` to `neut build`. In this case, you must also pass `--skip-link`.

### `--clang-option ".."`

You can pass `--clang-option ".."` to `neut build` to pass additional clang options to the compiler.

Internally, the Neut compiler translates source files into LLVM IR files, and then calls clang to convert these IR files into object files (and then calls clang again to link all of them).

The options specified here are used when the Neut compiler internally calls clang.

For example, by passing `--clang-option "-fsanitize=address"`, you can use the address sanitizer of clang.

### `--mode`

You can pass `--mode {develop,release}`. By default this value is `develop`.

The value passed here can be obtained from source code by using `introspect`:

```neut
define foo(): unit {
  introspect build-mode {
  - release =>
    // ..
  - default =>
    // ..
  }
}
```

### `--end-of-entry`

`--end-of-entry ANY_STRING` can be used to add separator between compiler diagnostics. This is intended to be used with linter wrappers like flycheck.

For example, suppose we have this obviously ill-typed program:

```neut
define main(): int {
  tau
}
```

By running `neut build`, errors like the below are reported:

```text
/path/to/sample/source/hey.nt:1:8
error: expected:
         () -> unit
       found:
         () -> int64
/path/to/sample/source/hey.nt:2:3
error: expected:
         int64
       found:
         tau
```

On the other hand, by running `neut build --end-of-entry EOE`, the text `EOE` is inserted after each entries:

```text
/path/to/sample/source/hey.nt:1:8
error: expected:
         () -> unit
       found:
         () -> int64
EOE
/path/to/sample/source/hey.nt:2:3
error: expected:
         int64
       found:
         tau
EOE
```

These can be used to parse entries.

## `neut check`

`neut check` type-checks all the files in the current module.

### `--no-padding`

If `--no-padding` is set, compiler diagnostics are printed without padding spaces:

```text
/Users/vekatze/Desktop/hey/source/hey.nt:1:8
error: expected:
  () -> unit
found:
  () -> int64
/Users/vekatze/Desktop/hey/source/hey.nt:2:3
error: expected:
  int64
found:
  tau
```

## `neut clean`

`neut clean` removes the cache files in the current module's `build` directory.

## `neut archive`

`neut archive VERSION` creates a module tarball that can be used by `neut get`.

### Notes on Versions

`VERSION` must be of the form `X1-X2-..-Xn`, where all the integers are non-negative. For example, the followings are valid versions:

- `1-0`
- `0-1-0`
- `2-1-3`
- `0-0-0-3`

When running `neut archive VERSION`, this command searches the `archive` directory to get all the compatible older versions. For example, suppose the `archive` directory contains the following files:

- `1-0.tar.zst`
- `1-1.tar.zst`
- `2-0.tar.zst`
- `2-1.tar.zst`

In this case, the command `neut archive 2-2` searches the `archive` directory and get `2-0` and `2-1` as the older versions on `2-2`. Here, these "older versions" are determined along [Semantic Versioning](https://semver.org/).

This command then computes all the digests of these older compatible tarballs, inserts the list of digests into the `module.ens`, and packs the ens file and the other source files to create a tarball `2-2.tar.zst`. This digest information inside `module.ens` should appear like the below:

```ens
{
  target {..}
  dependency {..}
  antecedent [
    "Bp8RulJ-XGTL9Eovre0yQupJpeS3lGNk8Q6QQYua7ag" // ← digest of 2-0.tar.zst
    "zptXghmyD5druBl8kx2Qrei6O6fDsKCA7z2KoHp1aqA" // ← digest of 2-1.tar.zst
  ]
}
```

This information is then used when resolving dependencies.

### Using the Newest Compatible Version

Consider the following dependency relation:

- The module `A` depends on `B` and `C`.
- The module `B` depends on `D (version 1.1)`.
- The module `C` depends on `D (version 1.2)`.

```text
A ──> B ──> D (1.1)
│
└───> C ──> D (1.2)
```

Thanks to the `antecedent` information inside `D (1.2)`, the compiler can detect that the `D (1.2)` is a newer compatible alternative of `D (1.1)` during compilation. Using this information, the compiler rewrites the relation above into the below:

```text
A ──> B ──┐
│         │
└───> C ──┴──> D (1.2)
```

By this procedure, the compiler always uses the newest compatible alternatives of the dependency relation.

Of course, this rewriting won't work if the minor compatible version `D (1.2)` isn't compatible with `D (1.1)`. This is a bug of the library `D (1.2)`, and should be fixed by the module author.

### Utilizing the Compatibility Relation

As an exercise, suppose that:

- serious bugs were discovered in `D (1.1)` and `D (1.2)`,
- the grave look author of `D` released a bug-fix release `D (1.3)` in a hurry, and
- the happy authors of `B` and `C` are on a honeymoon vacation in Hawaii.

In this case, we don't have to wait the authors of `B` and `C` to update their dependencies. Instead, we can just add `D (1.3)` to our module's dependency:

```text
A ──> B ──┐
│         │
├───> C ──┴──> D (1.2)
│
└────────────> D (1.3)
```

Then the compiler translates the above relation into the below:

```text
A ──> B ───┐
│          │
├───> C ───┤
│          │
└──────────┴──> D (1.3)
```

So we don't have to rain on their happy parade in Waikiki beach.

## `neut create`

`neut create NAME` creates a directory `./NAME/` and add files so that the directory can be used as a Neut module.

## `neut get`

`neut get ALIAS URL` fetches and builds external modules specified by `URL`, and add it to the current module under the name `ALIAS`.

```sh
neut get some-name https://github.com/SOME_USER_NAME/REPO_NAME/raw/main/archive/0-1.tar.zst
```

This command fetches the tarball from the specified URL and adds it to the current module.

The information of the newly-added module is saved to `module.ens`:

```ens
{
  target {
    // ..
  }
  dependency {
    // ..
    // ↓ (something like this is added automatically)
    some-name {
      digest "xNmQu6It81lGBy1sKvk5_jE4Qt8w8KgkVgGj0RBbbrk"
      mirror [
        "https://github.com/SOME_USER_NAME/REPO_NAME/raw/main/archive/0-1.tar.zst"
      ]
    }
    // ..
  }
}
```

The `URL` in `neut get NAME URL` must be the url of an archive that is created by `neut archive`.

<div class="info-block">

In Neut, modules in Neut are distinguished by their digests, not by their names (aliases).

</div>

Suppose you specify an existing `ALIAS` when running `neut get ALIAS URL`. In this case, if the digest of the new tarball is the same as the older one, the `URL` is added to the list `mirror`. Otherwise, the existing dependency is replaced by the newer one.

You can simply remove the entry of an `ALIAS` to "remove" the module from your module.

Modules added here can then be used in source files. See [Modules](./modules.md) for information on how to use definitions in external dependencies.

## `neut format-source`

`neut format-source PATH/to/source/file.nt` formats given source file and outputs the result to stdout.

### `--in-place`

When the option `--in-place` is set, `format-source` rewrites given files in-place.

## `neut format-ens`

`neut format-source path/to/source/file.ens` formats given ens file and outputs the result to stdout.

### `--in-place`

When the option `--in-place` is set, `format-source` rewrites given files in-place.

## `neut zen`

`neut zen path/to/source/file.nt` builds and executes given file as if it were an entry point.

`zen` treats the function `zen` inside the given file as its entry point (that is, `main`).

This command is intended to be used for rapid prototyping.

## `neut lsp`

`neut lsp` starts the LSP server. The LSP server has basic features like linting, jump to definition, etc. For more information, see [here](./todo.md).

## `neut version`

`neut version` prints the version of the compiler.
