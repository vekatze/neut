# Modules

A directory (including all its children) is a _module_ if it contains a file named `module.ens`.

Below is a list of the configurations in `module.ens`.

The string literals in `module.ens` use the same escape sequences as [the string literals of Neut](./terms.md#strings).

## Table of Contents

- [target](#target)
- [zen](#zen)
- [dependency](#dependency)
- [archive](#archive)
- [cache](#cache)
- [source](#source)
- [foreign](#foreign)
- [extra-content](#extra-content)
- [static-file](#static-file)
- [preset](#preset)
- [universal](#universal)
- [inline-limit](#inline-limit)
- [antecedent](#antecedent)

## `target`

The field `target` defines the entry points of a module. It should look like the following:

```ens
{
  // ..
  target {
    TARGET-1 {
      main "path/to/source/file-1.nt",
    },
    // ..
    TARGET-N {
      main "path/to/source/file-n.nt",
    },
  },
  // ..
}
```

Suppose that your module has the following `target` in `module.ens`:

```ens
{
  // ..
  target {
    foo {
      main "foo.nt",
    },
    bar {
      main "item/yo.nt",
    },
  },
  // ..
}
```

In this case, your module has two targets: `foo` and `bar`. The entry point of `foo` is the `main` function in `(source-dir)/foo.nt`. The entry point of `bar` is the `main` function in `(source-dir)/item/yo.nt`.

The names in `target` can be specified when running `neut build`. For example, given the above definition of `target`, you can run `neut build foo`.

The names of targets are also used as the names of executables. For example, if you run `neut build foo --install ./bin/`, an executable named `foo` will be created under the directory `./bin/`.

The field `target` is optional. The default value of `target` is `{}`.

### `compile-option`

You can add `compile-option` to a target as follows:

```ens
{
  target {
    foo {
      main "foo.nt",
      // ↓ here
      compile-option [
        "-g",
        "-O0",
        "-fsanitize=address",
        "$SOME_ENV_VAR",
        "$(some-command arg)",
      ],
    },
  },
}
```

The compiler passes the options specified here to `clang` when compiling LLVM IRs into object files.

In `compile-option`, you can use environment variables and shell interpolations.

The field `compile-option` is optional. The default value of `compile-option` is `[]`.

### `link-option`

You can add `link-option` to a target as follows:

```ens
{
  target {
    foo {
      main "foo.nt",
      // ↓ here
      link-option [
        "-g",
        "-O0",
        "$SOME_ENV_VAR",
        "$(some-command)",
      ],
    },
  },
}
```

The compiler passes the options specified here to `clang` when linking object files.

In `link-option`, you can use environment variables and shell interpolations.

The field `link-option` is optional. The default value of `link-option` is `[]`.

### `build-option`

You can add `build-option` to a target as follows:

```ens
{
  target {
    foo {
      main "foo.nt",
      // ↓ here
      build-option [
        "$(pkg-config openssl --libs --cflags)",
        // ...
      ],
    },
  },
}
```

Adding an element to `build-option` is the same as adding the element to both `compile-option` and `link-option`.

In `build-option`, you can use environment variables and shell interpolations.

The options are expanded every time the target is built. If the expansion differs from the one of the previous build, the artifacts of the target are discarded and built again.

The field `build-option` is optional. The default value of `build-option` is `[]`.

### `allocator`

The field `allocator` specifies which allocator is used by the target:

```ens
{
  target {
    foo {
      main "foo.nt",
      allocator "mimalloc",
    },
  },
}
```

The following values are available:

- `"system"`: the system allocator
- `"mimalloc"`: mimalloc

The field `allocator` is optional. The default value of `allocator` is `"system"`.

### `platform`

The field `platform` specifies which hosts can run the build and which platform to build for.

```ens
{
  target {
    foo {
      main "foo.nt",
      platform "*/wasm32-wasip1",
    },
  },
}
```

The compiler runs on the host platform and builds the artifact for the target platform. A build pairs these two names as `HOST/TARGET`.

| Platform              | Capabilities                            |
| --------------------- | --------------------------------------- |
| `"arm64-darwin"`      | `thread`, `subprocess`                  |
| `"amd64-linux"`       | `thread`, `subprocess`                  |
| `"arm64-linux"`       | `thread`, `subprocess`                  |
| `"wasm32-wasip1"`     | none                                    |
| `"wasm32-wasip1-web"` | `javascript`                            |

The field takes a [platform pattern](#platform-patterns) described below.

The compiler matches the host name from left to right, trying the longest strings first for each * and each variable’s first occurrence. The first complete match supplies the variable values for the target part, which must expand to a valid platform name.

On `"wasm32-*"`, the stack in WebAssembly linear memory defaults to 8 MiB. This is the same size as the main thread's stack on the other platforms.

The field `platform` is optional. The default is `"${0}/${0}"`, which builds for the host.

#### Platform Patterns

In `HOST-PART/TARGET-PART`, the left part matches the host name and the right part matches the target name. Literal text matches itself.

Within each part, `*` and `${NAME}` match any string, including the empty string. All occurrences of the same variable must match the same string, even across the `/`.

| Pattern                         | Matches                                |
| ------------------------------- | -------------------------------------- |
| `"*/arm64-darwin"`              | builds for `arm64-darwin`              |
| `"*/*-linux"`                   | builds for Linux                       |
| `"*-linux/*"`                   | builds on Linux                        |
| `"${0}/${0}"`                   | builds for the host platform           |
| `"${arch}-linux/${arch}-linux"` | builds for the host platform on Linux  |
| `"*/*"`                         | all builds                             |

The compiler rejects a pattern that matches no pair of platform names.

### `execute`

The field `execute` specifies how `neut build TARGET --execute` runs the build artifact:

```ens
{
  target {
    foo {
      main "foo.nt",
      platform "*/wasm32-wasip1",
      execute "wasmtime {{executable}}",
    },
  },
}
```

The value is a command, run by a shell. Arguments passed after `--execute` are appended to it, each quoted.

In the field `execute`, you can use the following placeholders:

- `{{executable}}`: the path of the build artifact
- `{{module:MODULE-PATH}}`: the root directory of the module named by `MODULE-PATH`
- `{{neut}}`: the path of the running compiler
- `{{target}}`: the name of the target

A placeholder expands to one or more arguments, each quoted on its own.

`{{module:MODULE-PATH}}` takes a [module path](./basis.md#resolving-names), relative to the module in which the command is written:

- `{{module:this}}`: the module itself
- `{{module:core}}`: its dependency `core`
- `{{module:noa.core}}`: the `core` of its dependency `noa`

As with `import`, each step resolves to the [newest compatible version](./commands.md#using-the-newest-compatible-version) of the dependency.

The build artifact is placed in a directory named after the module configuration and the options of the target. The path of `{{executable}}` therefore changes when `module.ens` changes.

The field `execute` is optional. Without it, `--execute` runs the build artifact directly.

## `zen`

The field `zen` lists rules for `neut zen`.

```ens
{
  // ..
  zen [
    {
      platform "*/wasm32-wasip1-web",
      execute "{{module:this}}/run.sh {{executable}}",
    },
  ],
  // ..
}
```

Each rule requires a [`platform`](#platform). The compiler uses the first rule with a matching host part. The target part gives the target platform.

If no rule matches, the compiler uses the host platform and the default settings.

The LSP server also uses the selected platform. So does `neut check` if `--target` isn't specified.

The following fields work as they do in `target`.

- [`build-option`](#build-option)
- [`compile-option`](#compile-option)
- [`link-option`](#link-option)
- [`allocator`](#allocator)
- [`execute`](#execute)

The field `zen` is optional. The default is `[]`.

## `dependency`

The field `dependency` defines the dependencies of a module. It should look like the following:

```ens
{
  // ..
  dependency {
    foo {
      digest "(base64url-encoded sha256 checksum)",
      mirror [
        "URL-1",
        // ..
        "URL-N",
      ],
      enable-preset <true | false>, // ← optional field
    },
    // ..
    bar { .. },
  },
  // ..
}
```

The field `digest` specifies the checksum of the tarball of the dependency. The digest is a Base64URL-encoded SHA256 checksum of the tarball. This digest is the "real" name of this dependency and is used as an identifier.

The field `mirror` specifies a list of URLs from which the compiler can fetch the tarball. When fetching the tarball is necessary, the compiler tries these URLs from the beginning to the end.

The optional field `enable-preset` specifies whether to import the dependency's `preset` automatically, like the Prelude in other languages. For more information, see the explanation of `preset` in this section.

Dependency aliases are components of qualified names. For example, if module `foo` depends on `bar`, users of `foo` can write names like `foo.bar::some-file::some-type` when that module path is visible.

`this` is reserved for the identity path and can't be used as a dependency alias.

The usual `_`-prefix rule applies to dependency aliases as well. An alias that starts with `_` is useful for implementation-only dependencies.

The field `dependency` is optional. The default value of `dependency` is `{}`.

## `archive`

The field `archive` defines the path of the directory into which the subcommand `neut archive` stores tarballs. It should look like the following:

```ens
{
  // ..
  archive "my-archive",
  // ..
}
```

The field `archive` is optional. The default value of `archive` is `./archive/`.

## `cache`

The field `cache` defines the path of the directory in which to store object files, executables, dependencies, etc. It should look like the following:

```ens
{
  // ..
  cache ".cache",
  // ..
}
```

The field `cache` is optional. The default value of `cache` is `./cache/`.

### Links to Dependencies

Each dependency alias has a symbolic link under the module's cache directory, pointing to the dependency's root. With the default cache path:

```text
cache/alias/
├── core -> ../dependency/(digest of core)
└── util -> ../dependency/(digest of util)
```

Tools can use stable paths such as `cache/alias/core/source/list.nt`. Links select the newest compatible version, as [`{{module:MODULE-PATH}}`](#execute) does.

Commands that resolve dependencies, including `neut build` and `neut check`, refresh these links and remove obsolete aliases. Dependency modules maintain their own `alias/` directories in their caches.

## `source`

The field `source` defines the path to the directory where source files are stored. It should look like the following:

```ens
{
  // ..
  source ".",
  // ..
}
```

The field `source` is optional. The default value of `source` is `./source/`.

## `foreign`

The field `foreign` lists rules for compiling external source files.

```ens
{
  // ..
  foreign [
    {
      platform "*/*",
      input [
        "source/foo.c",
        "source/foo.h",
      ],
      output [
        "generated/",
      ],
      link [
        "generated/foo.o",
      ],
      script [
        "mkdir -p {{foreign}}/generated",
        "{{clang}} -c -flto=thin -O2 source/foo.c -o {{foreign}}/generated/foo.o",
        "cp source/foo.h {{foreign}}/generated/foo.h",
      ],
    },
  ],
}
```

Each rule requires a [platform pattern](#platform-patterns) in `platform`.

The compiler selects the first matching rule in each module. It uses the same `HOST/TARGET` pair for the current module and its dependencies. If no rule matches, the module adds no foreign build steps or link options.

### `input`

The field `input` specifies the list of external source files. The paths are relative to the root of the module.

`neut archive` includes the `input` files from every rule.

### `output`

The field `output` lists the files and directories produced by `script`, relative to the module's _foreign directory_.

If an entry ends with `/`, it is treated as a directory. Otherwise, it is treated as a file.

### `link`

The field `link` lists files to link alongside Neut's object files, relative to the foreign directory. Each file must be listed in `output` or lie inside a directory listed there.

### `transitive-link-option`

The field `transitive-link-option` supplies `clang` linker options for this module and its direct or indirect dependents.

The following rule uses `pkg-config` to get linker options for the host's `libpq`. The pattern `"${0}/${0}"` limits the rule to builds for the host platform.

```ens
{
  // ..
  foreign [
    {
      platform "${0}/${0}",
      // ..
      transitive-link-option [
        "$(pkg-config libpq --libs)",
      ],
    },
  ],
}
```

The options are passed after the object files, with dependencies' options before the module's own options.

### `script`

The field `script` specifies how to compile external source files. When running `neut build`, the compiler executes the specified commands immediately after resolving all the `import`s.

In the field `script`, you can use the following placeholders:

- `{{clang}}`: The `clang` used by the compiler, aimed at the platform of the target
- `{{module:MODULE-PATH}}`: The root directory of the module named by `MODULE-PATH`
- `{{foreign}}`: The foreign directory
- `{{neut}}`: The path of the running compiler

These expand as in `execute`: one or more arguments, each quoted. `{{clang}}` expands to several since it carries the options that select the platform, such as `-target` and the sysroot.

The compiler skips running `script` when all outputs are newer than all inputs. For a directory in `output`, this check includes the files inside it.

When running the `script`, the compiler sets the current working directory to the module's root directory.

### Notes

The field `foreign` is optional. The default is `[]`. The `input`, `output`, `link`, `transitive-link-option`, and `script` fields also default to `[]`.

An example of `foreign` can be found in the [core library](https://github.com/vekatze/neut-core/blob/main/module.ens).

<div class="info-block">

The compiler links the resulting foreign object files without any name mangling. You're strongly encouraged to prefix names in your foreign sources with your module name and the major version to avoid name collisions.

</div>

## `extra-content`

The field `extra-content` defines additional files or directories to include when running `neut archive`. It should look like the following:

```ens
{
  // ..
  extra-content [
    "README.md",
    "docs/",
  ],
  // ..
}
```

Each entry must be a path relative to the module root.

If an entry ends with `/`, it is treated as a directory. Otherwise, it is treated as a file.

The field `extra-content` is optional. The default value of `extra-content` is `[]`.

## `static-file`

The field `static-file` defines the list of static files that can be embedded into source files at compile time. It should look like the following:

```ens
{
  // ..
  static-file {
    some-file "relative/path/from/the/module/root/to/some-file.txt",
    other-file "relative/path/from/the/module/root/to/other-file.txt",
  },
  // ..
}
```

You can use the keys defined here in source files using `import` and `static`:

```neut
// foo.nt

import {
  // ..
  core::string {from-text},
  static-file {some-file, other-file},
  // ..
}

define use-some-file() -> unit {
  print(from-text(static some-file));
  print(from-text(static other-file))
}
```

After specifying a key for a static file in `import`, you can use `static` to embed the file's content into the source file at compile time. `static` is resolved as `text`, `blob`, `&string`, or `&binary` according to the surrounding type context; UTF-8 is required only when it resolves as `text` or `&string`. For the term-level details of `static`, see [Terms](./terms.md#static) and [Primitives](./primitives.md#primitive-types).

The compiler triggers recompilation when necessary by comparing the modification times of static resources and source files. In the code above, for example, the compiler recompiles `foo.nt` if you modify the content of `some-file.txt`.

The field `static-file` is optional. The default value of `static-file` is `{}`.

## `preset`

The field `preset` defines the list of names that must be imported implicitly when the module is used as a dependency. It should look like the following:

```ens
{
  // ..
  preset {
    foo ["my-func", "other-func"],
    item.bar ["baz", "qux"],
  },
  // ..
}
```

In the example above, the current module is expected to have the following files:

- `(source-dir)/foo.nt` that contains the definitions of `my-func` and `other-func`
- `(source-dir)/item/bar.nt` that contains the definitions of `baz` and `qux`

The field `preset` is used in combination with `enable-preset` in `dependency`.

Suppose we released a module that contains the `preset` definition shown above. Also, suppose someone is developing a module `MMM` and has added our module to `MMM`'s dependencies:

```ens
// module.ens in MMM

{
  // ..
  dependency {
    sample {
      digest "BASE64_URL_ENCODED_SHA256_CHECKSUM",
      mirror ["SOME_URL"],
      enable-preset true,
    },
    // ..
  },
  // ..
}
```

In this case, source files in `MMM` automatically import our preset names because `enable-preset` is `true`.

As an example, suppose a file in `MMM` contains an `import` like the following:

```neut
import {
  sample::foo {my-func},
}

define baz() -> int {
  let i = my-func();
  add-int(i, 10)
}
```

This code is the same as the following since the preset is enabled:

```neut
define baz() -> int {
  let i = my-func();
  add-int(i, 10)
}
```

The field `preset` is expected to be used as a way to provide "preludes" like those in other languages.

The field `preset` is optional. The default value of `preset` is `{}`.

## `universal`

The field `universal` declares whether the module must work on every platform. It should look like the following:

```ens
{
  // ..
  universal false,
  // ..
}
```

A module that declares `universal true` must build under every set of capabilities that a platform provides:

```text
source/worker.nt:2:3
Error: A universal module cannot require `thread`
```

The declaration covers what the module inherits through `import`, not only what it declares with `require` itself.

For more on capabilities, see [require](./statements.md#require).

The field `universal` is optional. The default value of `universal` is `true`.

## `inline-limit`

The field `inline-limit` defines the limit on recursion performed during compilation. It should look like the following:

```ens
{
  // ..
  inline-limit 100000,
  // ..
}
```

During compilation, the compiler performs possibly recursive computation when:

- type-checking, and
- expanding the definitions of inline functions.

The `inline-limit` specifies a limit here. For type-checking, it bounds the number of steps taken while reducing a type. For inlining, it bounds the number of expansions performed within one definition, where an expansion is the application of an inline function, a meta function, or a type alias, or the reduction of a `match` or an `unquote` at compile time. If the limit is exceeded, the compiler reports an error like the following:

```text
/path/to/file.nt:123:45
Error: Exceeded max recursion depth of N during (..)
```

The field `inline-limit` is optional. The default value of `inline-limit` is `1000000`.

## `antecedent`

The _internal_ field `antecedent` defines the list of older compatible versions. This field should look like the following:

```ens
{
  // ..
  antecedent [
    "Bp8RulJ-XGTL9Eovre0yQupJpeS3lGNk8Q6QQYua7ag",
    // ..
    "zptXghmyD5druBl8kx2Qrei6O6fDsKCA7z2KoHp1aqA",
  ],
  // ..
}
```

This information is used to select the newest compatible version of the module. For more information, see the explanation on `neut archive` in [Commands](./commands.md).

The field `antecedent` is optional. The default value of `antecedent` is `[]`.

This field must be modified _only by the compiler_. If you modify this field manually, the behavior of the compiler is undefined.

<div class="info-block">

Internally, the compiler treats a module as a library if and only if the module's `module.ens` contains the key `antecedent`.

</div>
