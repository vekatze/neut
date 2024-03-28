# Modules

A directory (including its all children) is a _module_ if it contains a file named `module.ens`. This file is for per-module configuration.

## Table of Contents

- [target](#target)
- [dependency](#dependency)
- [archive](#archive)
- [build](#build)
- [source](#source)
- [prefix](#prefix)
- [foreign](#foreign)
- [preset](#preset)
- [antecedent](#antecedent)
- [inline-limit](#inline-limit)

## `target`

The field `target` defines the entry points of a module. It should look like the below:

```ens
{
  // ..
  target {
    TARGET-1 "path/to/source/file-1.nt"
    // ..
    TARGET-N "path/to/source/file-n.nt"
  }
  // ..
}
```

Suppose that your module has the following `target` in `module.ens`:

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

In this case, your module has two targets: `foo` and `bar`. The entry point of `foo` is the `main` function in `(source-dir)/foo.nt`. The entry point of `bar` is the `main` function in `(source-dir)/item/bar.nt`.

The names in `target` can be specified when running `neut build`. For example, given the above definition of `target`, you can run `neut build foo`.

The names of targets are also used as the names of executables. For example, if you run `neut build --install ./bin/`, two binaries named `foo` and `bar` will be created under the directory `./bin/`.

`target` must have at least one field.

## `dependency`

The field `dependency` defines the dependencies of a module. It should look like the below:

```ens
{
  // ..
  dependency {
    foo {
      digest "(base64url-encoded sha256 checksum)"
      mirror [
        "URL-1"
        // ..
        "URL-N"
      ]
      enable-preset <true | false> // ← optional field
    }
    // ..
    bar { .. }
  }
  // ..
}
```

An example of `dependency`:

```ens
{
  // ..
  dependency {
    core {
      digest "ub3MUXVac9F1rebIhl_Crm2_GJ7PzCAekgp8aYH3-mo"
      mirror [
        "https://github.com/vekatze/neut-core/raw/main/archive/0-38.tar.zst"
      ]
      enable-preset true
    }
    some-package {
      digest "F_ST8PtL9dLCDWVZ4GpuS7sviUU0_-TUz2s6iw-86KU"
      mirror [
        "https://example.com/foobarbuz/packages/22-3.tar.zst"
      ]
    }
  }
  // ..
}
```

The field `digest` specifies the checksum of the tarball of the dependency. The digest is a Base64URL-encoded SHA256 checksum of the tarball. This digest is the "real" name of this dependency, and used as an identifier.

The field `mirror` specifies a list of URLs from which the compiler can fetch the tarball. When running `neut get`, the compiler will try to get the tarball if necessary, from the beginning of this list.

The optional field `enable-preset` specifies whether to import `preset`s automatically, as in "prelude" in other languages. This field should only be used (and set to be true) with the core library. For more information, see the explanation of `preset` in this section.

The field `dependency` can be omitted. The default value of `dependency` is `{}`.

## `archive`

The field `archive` defines the path of the directory into which the subcommand `neut archive` store tarballs. It should look like the below:

```ens
{
  // ..
  archive "my-archive"
  // ..
}
```

The field `archive` can be omitted. The default value of `archive` is `./archive/`.

## `build`

The field `build` defines the path of the directory to store object files, executables, etc. It should look like the below:

```ens
{
  // ..
  build ".build"
  // ..
}
```

The field `build` can be omitted. The default value of `build` is `./build/`.

## `source`

The field `source` defines the path of the directory to store source files. It should look like the below:

```ens
{
  // ..
  source "."
  // ..
}
```

The field `source` can be omitted. The default value of `source` is `./source/`.

## `prefix`

The field `prefix` defines the aliases of source files. It should look like the below:

```ens
{
  // ..
  prefix {
    Foo "this.foo"
    // ..
    Bar "this.item.bar"
  }
  // ..
}
```

Each field in `prefix` specifies an alias of the specified source file. For example, given the definition above, the code

```neut
import {
- this.foo
- this.item.bar {some-func}
}
```

can be rewritten into the below:

```neut
import {
- Foo
- Bar {some-func}
}
```

The prefixes specified in a `module.ens` of a module can be used only in the module.

The field `prefix` can be omitted. The default value of `prefix` is `{}`

## `foreign`

The field `foreign` defines a list of directories that contains platform-dependent object files. It should look like the below:

```ens
{
  // ..
  foreign [
    "./ffi/"
    // ..
    "./some-directory/"
  ]
  // ..
}
```

The structure of a directory specified in `foreign` must be something like the below:

```text
./ffi/
├── amd64-linux/
│  ├── foo.o
│  └── bar.o
├── arm64-darwin/
│  ├── foo.o
│  └── bar.o
└── arm64-linux/
   ├── foo.o
   └── bar.o
```

That is, the directory must be of the following form:

```text
{foreign-directory-root}/{platform-name}/(.. list of object files ..)
```

When building a module with `foreign` directories, all the object files placed in the corresponding `{platform-name}` in `foreign` directories are used during linking phase.

The "object files" here are expected to be something that can be generated using clang as follows:

```sh
clang -c -o ffi/arm64-darwin/foo.o path/to/foo.c
```

The field `foreign` can be omitted. The default value of `foreign` is `[]`.

<div class="info-block">

If you peek at the `build` directory, you'll find a directory named `artifact` that contains cache files (`foo.i`) and object files (`foo.o`). When linking phase, all the object files in the `artifact` directory and the object files in the `foreign` directories are linked to produce an executable.

</div>

## `preset`

The field `preset` defines the list of files that must be imported implicitly when the module is used as a dependency. It should look like the below:

```ens
{
  // ..
  preset {
    foo ["my-func" "other-func"]
    item.bar ["hoge" "pohe"]
  }
  // ..
}
```

In the example above, the current module is expected to have the followings:

- the file `(source-dir)/foo.nt` that contains the definitions of `my-func` and `other-func`
- the file `(source-dir)/item/bar.nt` that contains the definitions of `hoge` and `pohe`

The field `preset` is used in combination with `enable-preset` in `dependency`. Suppose that we released the module that contains the definition of `preset` as in the above. Also suppose that someone is developing a module `MMM` and they added our module to `MMM`'s dependency:

```ens
{
  // ..
  dependency {
    sample {
      digest "BASE64_URL_ENCODED_SHA256_CHECKSUM"
      mirror ["SOME_URL"]
      enable-preset true
    }
    // ..
  }
  // ..
}
```

In this case, source files in `MMM` imports our preset names automatically since `enable-preset` is `true`.

As an example, suppose that a file in `MMM` contains an `import` like the below:

```neut
import {
- sample.foo {my-func, other-func}
- sample.item.bar {hoge, pohe}
}

define buz() {
  let i = my-func() in
  print-int(i)
}
```

This is the same as the below since the preset is enabled:

```neut
define buz() {
  let i = my-func() in
  print-int(i)
}
```

The field `preset` is expected to be used as a way to realize "preludes" in other languages.

The field `preset` can be omitted. The default value of `preset` is `{}`.

## `antecedent`

The field `antecedent` defines the list of older compatible versions. This field should like the below:

```ens
{
  // ..
  antecedent [
    "Bp8RulJ-XGTL9Eovre0yQupJpeS3lGNk8Q6QQYua7ag"
    // ..
    "zptXghmyD5druBl8kx2Qrei6O6fDsKCA7z2KoHp1aqA"
  ]
  // ..
}
```

This information is used to select the newest compatible version of the module. For more information, see the explanation on `neut archive` in [Commands](./commands.md).

The field `antecedent` can be omitted. The default value of `antecedent` is `[]`.

<div class="info-block">

You won't write this field by hand because it is inserted automatically into `module.ens` of a tarball when `neut archive` is executed.

</div>

## `inline-limit`

The field `inline-limit` defines the limit on recursion performed during compilation. It should look like the below:

```ens
{
  // ..
  inline-limit 1000000
  // ..
}
```

During compilation, the compiler performs possibly recursive computation when:

- typechecking, and
- expanding definitions of inline functions.

The `inline-limit` specifies a limit here. If the limit is exceeded, the compiler reports an error like the below:

```text
/path/to/file.nt:123:45
error: exceeded max recursion depth of 1000 during (..)
```

The field `inline-limit` can be omitted. The default value of `inline-limit` is `100000`.
