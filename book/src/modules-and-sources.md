# Modules and Sources

In this section, we'll see how to use modules in Neut.

## Table of Contents

- [Basics of Modules](#basics-of-modules)
- [Basics of Source Files](#basics-of-source-files)
- [Publishing Modules](#publishing-modules)
- [Adding Dependency Modules](#adding-dependency-modules)
- [Importing Files](#importing-files)
- [Restricting Top-Level Names](#restricting-top-level-names)

## Basics of Modules

### Creating, Building, and Executing a Module

Let's create a module by running the following command:

```sh
neut create sample
```

This command creates a template module `./sample/` that prints "Hello, world!".

You can build and execute this module as follows:

```sh
cd ./sample
neut build sample --execute # => "Hello, world!"
```

You can also install the resulting binary and run it directly:

```sh
neut build sample --install ./bin # creates a directory `bin` if necessary
./bin/sample # => "Hello, world!"
```

### Structure of a Module

The structure of a module is as follows:

```text
sample/
├── cache/
│  └── ...
├── source/
│  └── sample.nt
└── module.ens
```

The directory `cache` is where object files (binary files) and dependencies are put. You don't normally have to go into the directory.

The directory `source` is where source files are put.

The file `module.ens` contains meta information about this module, such as dependencies.

<div class="info-block">

You can change the locations of special directories such as `cache` using `module.ens`. See [Modules](./modules.md) for more information.

</div>

### module.ens

The contents of `module.ens` look roughly like the following:

```ens
{
  target {
    sample {
      main "sample.nt",
      allocator "system",
    },
  },
  dependency {
    core {
      digest "(base64url-encoded checksum)",
      mirror [
        "https://github.com/.../X-Y-Z.tar.zst",
      ],
      enable-preset true,
    },
  },
}
```

`target` specifies the targets of a module. In the example above, the command `neut build sample` builds the module using the file `source/sample.nt` as its entry point.

`allocator` specifies which allocator is used by the target. By setting it to `"mimalloc"`, you can use mimalloc for that target.

`dependency` specifies the dependencies of a module. Since our running example is small, the only dependency is `core`, which plays the same role as the Prelude in other languages.

`digest` is the base64url-encoded checksum of a dependency.

`mirror` is a list of URLs of a dependency.

`enable-preset` makes the dependency behave similarly to the Prelude in other languages. If enabled, the names listed in the dependency's `preset` are imported automatically into every file in our module.

## Basics of Source Files

### Editing Source Files

Let's see the contents of `source/sample.nt`:

```neut
// sample.nt

define main() -> unit {
  print("Hello, world!\n"); // `print` is defined in `core`
}
```

The above code defines a function `main` that returns a value of type `unit`. This function prints `"Hello, world!\n"`.

Let's try editing the code as follows:

```neut
define main() -> unit {
  print("Yo\n");
}
```

Then, build and execute the project:

```sh
neut build sample --execute # => Yo
```

You should be able to see that the result changes accordingly.

### Defining Functions

You can also define your own functions:

```neut
// sample.nt

import {
  core::int.io {print-int},
}

define get-int() -> int {
  42
}

define main() -> unit {
  print-int(get-int()); // => 42
}
```

Functions can also take arguments. Let's rewrite `sample.nt` as follows:

```neut
// sample.nt

import {
  core::int.io {print-int},
}

define increment(x: int) -> int {
  add-int(x, 1)
}

define my-add(x: int, y: int) -> int {
  add-int(x, y)
}

define main() -> unit {
  print-int(my-add(10, increment(10))); // => 21
}
```

Top-level items like `define` are called statements. You'll learn more about them on the next page.

<div class="info-block">

As in F#, statements in Neut are order-sensitive. Therefore, if you define `main` before `my-add`, the code won't compile. For forward references, you have to declare names explicitly beforehand using a statement called `nominal`, which we'll see on the next page.

</div>

## Publishing Modules

Let's publish our module so others can use `increment` and `my-add`.

You can create a tarball snapshot of your module using `neut archive`:

```sh
neut archive 0-1
ls ./archive # => 0-1.tar.zst
```

The argument of `archive` must be something like `0-1`, `0-1-0`, or `1-2-0-1`.

You can then publish these tarballs on GitHub, for example.

## Adding Dependency Modules

`neut get` can be used to add dependencies:

```sh
# creates a new module
neut create new-item
cd new-item

# adds a sample module that contains `my-add` and `increment` to your module
neut get util https://github.com/vekatze/neut-sample/raw/main/archive/0-1-0.tar.zst
```

The subcommand `get` fetches the tarball from the specified URL and adds it to the current module. You can then refer to that dependency as `util` in your module.

The new dependency information is saved to `module.ens`:

```ens
{
  target {
    new-item {
      main "new-item.nt",
      allocator "system",
    },
  },
  dependency {
    core { .. },
    // ↓ HERE
    util {
      digest "..",
      mirror [
        "https://github.com/YOUR_NAME/YOUR_REPO_NAME/raw/main/archive/0-1-0.tar.zst",
      ],
    },
  },
}
```

<div class="info-block">

The canonical identifier of a dependency is the digest of the tarball. Names such as `util` are just aliases.

</div>

## Importing Files

### Importing Files in the Current Module

Let's try creating two files:

```neut
// item.nt

define hi() -> unit {
  print("Hi");
}
```

```neut
// foo/greet.nt

define yo() -> unit {
  print("Yo");
}
```

These files can then be used from `new-item/source/new-item.nt` as follows:

```neut
// new-item.nt

import {
  this::item {hi},
  this::foo.greet {yo},
}

define main() -> unit {
  hi();
  yo();
}
```

In other words, a file in the current module is imported through its path from the source directory.

### Importing Files in Dependencies

Now let's use the dependency we added above:

```neut
// new-item.nt

import {
  core::int.io {print-int},
  util::sample {my-add}, // imports `my-add` in util's `source/sample.nt`
}

define main() -> unit {
  print-int(my-add(10, 11)); // ← using `my-add`
}
```

The general form of a fully-qualified name in a dependency is:

```text
module.path::source.path::body.path
```

The `module.path` part is a dot-separated sequence of dependency aliases, such as `util` or `foo.http`. The `source.path` part is the path from the source directory of the last dependency module. The `body.path` part is the path to a name inside the file.

Thus, `util::sample::my-add` refers to `my-add` in `source/sample.nt` of the dependency `util`.

Every item of an `import` can optionally have a list of names, as in `util::sample {my-add}`. Names in these lists are made available after `import`.

You can also use the fully-qualified form:

```neut
// new-item.nt

import {
  core::int.io {print-int},
  util::sample, // removed `{my-add}`
}

define main() -> unit {
  // ↓ using the fully-qualified form of `my-add`
  print-int(util::sample::my-add(10, 11));
}
```

A file can also be imported as a namespace, using the entry `* as name`:

```neut
import {
  core::int.io {print-int},
  util::sample {* as s},
}

define main() -> unit {
  print-int(s.my-add(10, 11));
}
```

Also, files like `source/foo/item.nt` in `util` can be imported as follows:

```neut
import {
  util::foo.item,
}
```

Dependency aliases can be chained. For example, if the current module depends on `foo`, and `foo` depends on `http`, then the current module can use files in `http` as follows:

```neut
import {
  foo.http::client {request},
}
```

You can also use the fully-qualified form:

```neut
define main() -> unit {
  foo.http::client::request()
}
```

Module paths are always relative to the module where they are written. If a public signature in `foo` mentions `http.json::some-file::some-type`, then a user of `foo` sees the same type through `foo.http.json::some-file::some-type`.

## Restricting Top-Level Names

### Required Prefixes

An `_`-prefixed segment restricts a name to a required prefix. To find it, normalize identity segments, take the part before the deepest `_`-prefixed segment, and remove the separator immediately before that segment. The result uses the same component notation as an ordinary name and never ends in `.` or `::`.

| Full name | Required prefix |
| --- | --- |
| `this::a.b::f` | none |
| `this::_a.b::f` | `this` |
| `this::a._b::f` | `this::a` |
| `this::a.b::_f` | `this::a.b` |
| `this::a.b::ns._f` | `this::a.b::ns` |

A name is available at positions that start with its required prefix. Module paths, source paths, and body paths retain their boundaries during this comparison. For example:

- `this::a.b::f` is available from anywhere.
- `this::_a.b::f` is available inside the defining module.
- `this::a._b::f` is available in `source/a.nt` and files below `source/a/`.
- `this::a.b::_f` is available in `source/a/b.nt` and files below `source/a/b/`.
- `this::a.b::ns._f` is available inside namespace `ns` and its children in `source/a/b.nt`.

### Exposed Names

A top-level item can't be more public than the names it exposes. For example:

```neut
data _secret {}

// Error: `expose` is more public than `_secret`
define expose(x: _secret) -> unit {
  Unit
}
```

### Private Dependencies

Dependency aliases are also components of qualified names, so the same prefix rule applies to them. This gives a way to name an implementation-only dependency: start its alias with `_`.

```ens
{
  dependency {
    _http {
      digest "..",
      mirror [".."],
    },
  },
}
```

Such a dependency can be used inside the module that declares it:

```neut
import {
  _http::client {request},
}
```

Here, `_http::client::request` is available inside the module that declares `_http`.

On the other hand, suppose another module depends on this module through the alias `foo` and tries to write:

```neut
import {
  foo._http::client {request},
}
```

This import is rejected because the `_http` component restricts where the qualified name can be used.
