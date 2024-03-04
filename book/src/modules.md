# Modules

A directory (including its all children) is a _module_ if it contains a file named `module.ens`. This file is for per-module configuration.

## `target`

`target` defines the entry points of your module.

### Syntax

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

### Semantics

`target` defines the entry points of your module.

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

In this case, the entry points of your modules are:

- the `main` function in `(source-dir)/foo.nt`, and
- the `main` function in `(source-dir)/item/bar.nt`.

The names in `target` can be specified when running `neut build`. For example, given the above definition of `target`, you can run `neut build foo`.

The names of targets are also used as the names of executables. For example, if you run `neut build --install ./bin/`, two binaries named `foo` and `bar` will be created under the directory `./bin/`.

`target` must have at least one entry.

## `dependency`

`dependency` defines dependencies of your module. It should look like the below:

```ens
{
  // ..
  dependency {
    DEPENDENCY-1 {
      digest "BASE64_URL_ENCODED_SHA256_CHECKSUM"
      mirror [
        "URL-1"
        // ..
        "URL-N"
      ]
      enable-preset <true | false> // ← optional field
    }
    // ..
    DEPENDENCY-N { .. }
  }
  // ..
}
```

The field `digest` specifies the checksum of the tarball of the dependency. The digest is a Base64URL-encoded SHA256 checksum of the tarball. This digest is the "real" name of this dependency, and used as an identifier.

The field `mirror` specifies a list of URLs from which the compiler can fetch the tarball. When running `neut get`, the compiler will try to get the tarball if necessary, from the beginning of this list.

The optional field `enable-preset` specifies whether to import `preset`s automatically, as in "prelude" in other languages. This field should only be used (and set to be true) with the core library. For more information, see the explanation of `prefix` in this section.

The default value of `dependency` is `{}`.

## `archive`

The path of the directory to store archives (modules that can be used by others).

The default value of `archive` is `./archive/`.

## `build`

The path of the directory to store object files, executables, etc.

The default value of `build` is `./build/`.

## `source`

The path of the directory to store source files.

The default value of `source` is `./source/`.

## `prefix`

`prefix` defines aliases of source files. It should look like the below:

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

The default value of `prefix` is `{}`

## `foreign`

`foreign` defines a list of directories that contains platform-dependent object files. It should look like the below:

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

The "object files" here are the ones that can be generated using clang as follows:

```sh
clang -c -o ffi/arm64-darwin/foo.o path/to/foo.c
```

<div class="info-block">

If you peek at the `build` directory, you'll find a directory named `artifact` that contains cache files (`foo.i`) and object files (`foo.o`). When linking phase, all the object files in the `artifact` directory and the `foreign` directory are linked to produce an executable.

</div>

## `preset`

`preset` defines list of files that must be imported implicitly. It should look like the below:

```ens
{
  // ..
  preset {
    foo ["my-func" "other-func"]
    item.bar ["hoge-add" "pohe-sum"]
  }
  // ..
}
```

In the example above, the current module is expected to have the file `(source-dir)/foo.nt` and the file contains the definitions of `my-func` and `other-func`. Also, the current module is expected to have the file `(source-dir)/item/bar.nt` and the file contains the definitions of `hoge-add` and `pohe-sum`.

`preset` is used in combination with `enable-preset` in `dependency`. Suppose that we released the module that contains the definition of `preset` as in the above. Also suppose that someone is developing a module `MMM` and they added our module to `MMM`'s dependency:

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

In this case, source files in `MMM` imports our preset names in `foo` and `item.bar` automatically. That is, if a file in `MMM` contains an `import` like the below:

```neut
import {
- sample.foo {my-func, other-func}
- item.bar {hoge-add, pohe-sum}
}

define buz() {
  let i = my-func() in
  print-int(i)
}
```

This is the same as the below:

```neut
define buz() {
  let i = my-func() in
  print-int(i)
}
```

`sample.foo.my-func` is imported automatically, since `enable-preset` is `true`, and `preset` of the dependency contains `foo ["my-func", "other-func"]`.

The default value of `preset` is `{}`.

## `antecedent`

`antecedent` defines the list of older compatible versions. You won't have to write this field by yourself because `antecedents` is inserted automatically when `neut archive` creates tarballs. This field should like the below:

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

The default value of `antecedent` is `[]`.

## `inline-limit`

`inline-limit` defines the limit on recursion performed during compilation. It should look like the below:

```ens
{
  // ..
  inline-limit 1000000
  // ..
}
```

During compilation, Neut performs possibly recursive computation when:

- dependent typechecking, and
- expanding definitions of inline functions.

The `inline-limit` specifies a limit here. If the limit is exceeded, the compiler reports an error like the below:

```text
/path/to/file.nt:123:45
error: exceeded max recursion depth of 1000 during (..)
```

The default value of `inline-limit` is `100000`.

## Using Names

Suppose that you added a dependency under the name `sample`.

FIXME: add notes on name resolution.

## The Obligated XKCD

Yes, our sacred [xkcd](https://xkcd.com/927/) is there, but this is my drawing after all. Nothing is wrong.

<figure>
  <img src="https://imgs.xkcd.com/comics/standards.png" alt="The beautiful MDN logo." />
</figure>

There are 14 competing standards and a modest drawing in a frame, in the same room.
