# Modules

A directory (including all its children) is a _module_ if it contains a file named `module.ens`.

Below is a list of the configurations in `module.ens`.

## Table of Contents

- [target](#target)
- [zen](#zen)
- [dependency](#dependency)
- [archive](#archive)
- [cache](#cache)
- [source](#source)
- [foreign](#foreign)
- [extra-content](#extra-content)
- [text-file](#text-file)
- [preset](#preset)
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

## `zen`

The field `zen` defines the configuration used by `neut zen`. It should look like the following:

```ens
{
  // ..
  zen {
    build-option [
      "-fsanitize=address",
    ],
    allocator "mimalloc",
  },
  // ..
}
```

The fields available inside `zen` are the same as those available inside each entry of `target`:

- `build-option`
- `compile-option`
- `link-option`
- `allocator`

These fields affect builds performed by `neut zen`, rather than `neut build TARGET`.

The field `zen` is optional. By default, it uses empty build options and the system allocator.

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

The field `foreign` defines a way to compile external source files. It should look like the following:

```ens
{
  // ..
  foreign {
    input [
      "source/foo.c",
      "source/bar.c",
    ],
    output [
      "foo.o",
      "bar.o",
    ],
    script [
      "{{clang}} -c -flto=thin -O2 source/foo.c -o {{foreign}}/foo.o",
      "{{clang}} -c -flto=thin -O2 source/bar.c -o {{foreign}}/bar.o",
    ],
  },
}
```

### `input`

The field `input` specifies the list of external source files. The paths are relative to the root of the module.

When running `neut archive`, the compiler adds all the `input` files to the resulting tarball.

### `output`

The field `output` specifies the output files produced from foreign source files. The paths are relative to a directory named the _foreign directory_. You can find the foreign directory in the build directory.

When running `neut build`, the compiler links all the `output` files in the foreign directory (in addition to Neut's "domestic" object files).

### `script`

The field `script` specifies how to compile external source files. When running `neut build`, the compiler executes the specified commands immediately after resolving all the `import`s.

In the field `script`, you can use the following placeholders:

- `{{clang}}`: The `clang` used by the compiler
- `{{foreign}}`: The foreign directory

The compiler skips running the `script` if all the files in `output` are newer than `input`.

When running the `script`, the compiler sets the current working directory to the module's root directory.

### Notes

The field `foreign` is optional. The default value of `foreign` is:

```ens
{
  input [],
  output [],
  script [],
}
```

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

## `text-file`

The field `text-file` defines the list of text files that can be embedded into source files at compile time. It should look like the following:

```ens
{
  // ..
  text-file {
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
  core.string {from-text},
  text-file {some-file, other-file},
  // ..
}

define use-some-file() -> unit {
  print(from-text(static some-file));
  print(from-text(static other-file))
}
```

After specifying a key for a text file in `import`, you can use `static` to embed the file's content into the source file at compile time. Here, `static` assumes that the encoding of the file is UTF-8. For the term-level details of `static` and `text`, see [Terms](./terms.md#static) and [Primitives](./primitives.md#primitive-types).

The compiler triggers recompilation when necessary by comparing the modification times of static resources and source files. In the code above, for example, the compiler recompiles `foo.nt` if you modify the content of `some-file.txt`.

The field `text-file` is optional. The default value of `text-file` is `{}`.

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
  sample.foo {my-func},
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

The `inline-limit` specifies a limit here. If the limit is exceeded, the compiler reports an error like the following:

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
