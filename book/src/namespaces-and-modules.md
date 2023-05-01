# Namespaces and Modules

Here we'll see how Neut organizes names and modules.

FIXME: reorganize

## Namespace Basics

### The Structure of Global Names

Every global name of Neut has the following structure:

```neut
module-name.path.to.source.file.top-level-name
```

We call this the definite description of a global variable.

For example, the definite description of a function `my-func` defined in a file `source/foo/item.nt` is:

- `this.foo.item.my-func`, if the module is the main module
- `CHECKSUM-OF-TARBALL.foo.item.my-func`, if the module is a library module

### Importing a File in the Current Module

When you create a module using `neut create NAME`, the directory named `source` is created:

```sh
neut create hey
tree -a hey
# => hey/
#    ├── source/
#    │  └── hey.nt
#    └── module.ens
```

Suppose you created a file `source/yo.nt`, as follows:

```neut
// yo.nt

export {
- yo-func // this allows other modules to see the definition of `yo-func`
}

define yo-func(): i64 {
  1
}
```

This file can be imported from `hey.nt` by writing, for example, as follows:

```neut
// hey.nt

import {
- this.yo // importing yo.nt
}

define main(): i64 {
  yo-func()
  // you can also write this.yo.yo-func()
}
```

You can import a file by writing `this.yo`, where `this` means the current module. In this case, `this` is our project `hey`.

The `yo` part of `this.yo` denotes the relative path from the directory `/source/`. Suppose we have a file `/source/foo/item.nt`:

```sh
tree hey
# => hey/
#    ├── source/
#    │  ├── foo/
#    │  │  └── item.nt
#    │  ├── hey.nt
#    │  └── yo.nt
#    └── module.ens
```

This `item.nt` can be imported from `hey.nt` as follows:

```neut
import {
- this.yo // importing yo.nt
- this.foo.item // importing foo/item.nt
}

define main(): i64 {
  // ...
}
```


### Exporting Names

By default, every name in a file is private. You can make functions public by using `export`:

```neut
// source/foo/item.nt

export {
- item-func // `item-func` can now be used from other files
}

define item-func(): i64 {
  some-private-func()
}

define some-private-func(): i64 {
  2
}
```

Variant types and its constructors can also be exported:


```neut
export {
- my-item { // export a variant type
  - Foo     // ... and its constructors
  - Bar
  }
- other-item {..} // `{..}` exports all its constructors
}

variant my-item {
- Foo
- Bar
}

variant other-item {
- Buz
- Qux
}
```

Also, you can re-export names from other files:

```neut
// source/exporter.nt

import {
- this.foo.item
}

export {
- this.foo.item.item-func
- this.foo.item.item-func => item-func-alias // re-exporting an alias
}
```

Also, as done in the above, you can re-export an alias of a name.

Re-exported names can be used as usual:

```neut
// source/user.nt

import {
- this.exporter
}

define foo(): i64 {
  let _ = item-func() // using re-exported name
  item-func-alias()   // using re-exported alias
}
```

### Qualifying Exported Names

Names in other files can be qualified as follows:

```neut
//
// source/foo/item.nt
//

export {
- item-func
}

define item-func(): i64 {
  2
}

//
// source/main.nt
//

import {
- this.yo
- this.foo.item => I
}

define main(): i64 {
  I.item-func()
  // `item-func()` will results in an error
}
```

## Module

A module in Neut is a directory that contains a file named `module.ens` and its corresponding source directories. Remember that you can create a module by calling `neut create MODULE_NAME`:

```sh
neut create hey
tree hey
# => hey/
#    ├── source/
#    │  └── hey.nt
#    └── module.ens
```

### Adding Module Dependencies

A module can use other modules. Such module dependencies can be added using `neut add`:

```sh
neut add core https://github.com/vekatze/neut-core/raw/main/release/0.2.0.4.tar.zst
```

By running the code above, the specified tarball is downloaded into `~/.cache/neut/library`:

```sh
ls ~/.cache/neut/library
# => jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=
```

where the `jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=` is the checksum of the module. Also, the module information is added to the current module's `module.ens`:

```text
dependency = {
  core = {
    URL = "https://github.com/vekatze/neut-core/raw/main/release/0.2.0.4.tar.zst"
    checksum = "jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o="
  }
}
```

Note that *the name of a library is defined by the user of the library*, not the creator of the library.

After that, you can import files in added modules from your module:

```neut
import {
- this.yo
- this.foo.item => I
- core.text.io // ← NEW!
}

define sample() {
  let v = core.text.io.get-line() // read line from stdin
  // you can also write `let v = target-line()`, of course
  ...
}
```

Internally, when running compilation, the compiler adds the following correspondence to its internal state:

```
core => jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=
```

and do the following name resolution:

```text
core.text.io.get-line

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=.text.io.get-line
```

### Resolving `this`

A module added as a dependency can contain a code like below:

```neut
import {
- this.path-to-some-file
}
```

This `this` in a library is also resolved into its corresponding checksum. Below is the detailed story.

When running compilation, every module is marked as "main" or "library". The main module is the module in which `neut build` is executed. Library modules are all the other modules that are necessary for compilation.

When compiling a library module, the compiler adds the following correspondence to its internal state:

```sh
this => CHECKSUM_OF_THE_LIBRARY

# e.g. this => jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=
```

and resolves `this` in a library into its corresponding checksum.

On the other hand, the main module's `this` is kept intact. Thus, the resulting code contains a symbol like `this.yo.yo-func`.

## Publishing Your Code

### Creating an Archive for Your Module

You can create a tarball for your module using `neut release VERSION`:

```sh
tree .
# => ./
#    ├── source/
#    │  ├── foo/
#    │  │  └── item.nt
#    │  ├── exporter.nt
#    │  ├── hey.nt
#    │  ├── user.nt
#    │  └── yo.nt
#    └── module.ens

# create a new release
neut release 0.1.0.0

tree .
# => ./
#    ├── release/
#    │  └── 0.1.0.0.tar.zst ← NEW!
#    ├── source/
#    │  ├── foo/
#    │  │  └── item.nt
#    │  ├── exporter.nt
#    │  ├── hey.nt
#    │  ├── user.nt
#    │  └── yo.nt
#    └── module.ens
```

`neut release` creates a tarball for the current module. The resulting tarball can be, for example, commited and pushed to your repository. Then, a user of your library can then add it to their module using `neut add NAME URL`.

### Identity of a Module

As we've seen, Neut identifies a module using its tarball's checksum, which I believe isn't a very common approach.

Now, suppose that you released a library with version `1.0.0`. The library contains a file like below:

```neut
variant some-type {
- Foo
- Bar(some-type)
}

define func(): some-type {
  Foo
}

define add-bar(x: some-type): some-type {
  Bar(some-type)
}
```

You, however, actually wanted to return `Bar(Foo)` in `func`. You fix this bug, and release the new version as `1.0.1`.

Both of the definitions of `some-time` and `add-bar` are the same between `1.0.0` and `1.0.1`, of course. However, if every package was naively distinguished by its checksum, the `add-bar` in `1.0.1` cannot be applied to values of type `some-type` in `1.0.0`, since the argument of `add-bar` must be of type `some-type` in `1.0.1`.

I believe this isn't desirable. We would like to equate the two compatible versions in some sense.

Neut resolves this problem via an approach based on Semantic Versioning, as described below.

### Practical Significance of Semantic Versioning

A module tarball in Neut is created using `neut release VERSION`. The `VERSION` in `neut release VERSION` must follow a versioning scheme which is essentially the same as the Semantic Versioning.

If you release a version of your module that is compatible to existing modules, *the compatibility information is saved to the module file in the newly-created tarball*. If new version is incompatible to others, then no compatibility information is saved.

For example, suppose you have `release/0.1.0.0.tar.zst`. If you run `neut release 0.1.1.0`, which is a minor update release, the `module.ens` in the newly-created tarball `release/0.1.1.0.tar.zst` contains the following part:

```text
antecedents = [
  "-doDFqpUZ2qCm2bsO_hpZ51WG3FxoVKSmzjue0sKh0g="
]
```

where the `-doDFqpUZ2qCm2bsO_hpZ51WG3FxoVKSmzjue0sKh0g=` is the checksum of the existing tarball `0.1.0.0.tar.zst`. `antecedents` stores all the checksums of the existing compatible releases. By this way, `module.ens` stores compatibility information.

This compatibility information is then used by compiler. When building a module, the compiler constructs a graph of antecedents, and *always use the latest compatible version of a module by tracing the graph* during compilation.

In this sense, the `antecedents` defines the identity of a library.

### Actual Versioning Scheme

Versions like `1.0.0`, `10.2.8`, `0.1.3`, or `0.0.0.1.8.0` are supported. Each non-negative integer between '.' are called as version segment. The string until the first non-zero version segment (including the version) is the major version of the release. For example, the major version of `10.2.8` is `10`. That of `0.0.0.1.8.0` is `0.0.0.1`.

### On What a Breaking Change Is

A breaking change is basically the same as that of the Semantic Versioning. However, please note that *adding a function to an existing file is also considered to be a breaking change*. This is because it could cause name conflict in user's source files.

If you want to add functions in non-major updates, create a new file and add them to the file.

Specifying non-compatible releases as compatible is a bug of a release.

## Other Notes on Namespaces and Modules

- You can change `~/.cache/neut` part by setting the environment variable `NEUT_CACHE_DIR`
- A module named `core` is treated special (the prelude library)
- Compiled objects, caches, etc. are stored in the directory `.build` of a module
