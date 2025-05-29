# Modules and Sources

In this section, we'll see how to use modules in Neut.

## Table of Contents

- [Basics of Modules](#basics-of-modules)
- [Basics of Source Files](#basics-of-source-files)
- [Publishing Modules](#publishing-modules)
- [Adding Dependency Modules](#adding-dependency-modules)
- [Importing Files](#importing-files)

## Basics of Modules

### Creating, Building, and Executing a Module

Let's create a module by running the following command:

```sh
neut create sample
```

This command creates a template module `./sample/` that performs "hello world".

You can build and execute this module as follows:

```sh
cd ./sample
neut build sample --execute # => "Hello, world!"
```

You can also retrieve the resulting binary:

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

The content of `module.ens` is something like the following:

```ens
{
  target {
    sample {
      main "sample.nt",
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

`dependency` specifies the dependencies of a module. Since our running example doesn't do much, the only dependency is `core`, which is the same as "prelude" in other languages.

`digest` is the base64url-encoded checksum of a dependency.

`mirror` is a list of URLs of a dependency.

`enable-preset` makes the dependency behave similarly to the Prelude in Haskell. That is, when `enable-preset` is set to `true`, the names specified in the dependency are automatically imported into every file in our module. This field should be set to `true` only for the `core` library.

## Basics of Source Files

### Editing Source Files

Let's see the content of `source/sample.nt`:

```neut
// sample.nt

define main(): unit {
  print("Hello, world!\n") // `print` is defined in `core`
}
```

The above code defines a function `main` that returns a value of type `unit`. This function prints `"Hello, world!\n"`.

Let's try editing the code as follows:

```neut
define main(): unit {
  print("Yo\n")
}
```

Then, build and execute the project:

```sh
neut build sample --execute # => Yo
```

You should be able to see that the result changes accordingly.

### Defining Functions

Of course, you can define a function:

```neut
// sample.nt

define get-int(): int {
  42
}

define main(): unit {
  printf("{}\n", [show-int(get-int())]) // => 42
}
```

A function can take arguments. Let's rewrite `sample.nt` as follows:

```neut
// sample.nt

define increment(x: int): int {
  add-int(x, 1)
}

define my-add(x: int, y: int): int {
  add-int(x, y)
}

define main(): unit {
  printf("{}\n", [my-add(10, increment(10))]) // # => 21
}
```

Top-level items like `define` are called statements. You’ll learn more about them in the next section.

<div class="info-block">

As in F#, statements in Neut are order-sensitive. Therefore, if you define `main` before `my-add`, the code won't compile. For forward references, you have to explicitly declare names beforehand using a statement called `nominal`, which we'll see in the next section.

</div>

## Publishing Modules

Let's publish our module so others can use `increment` and `my-add`.

You can create a tarball snapshot of your module using `neut archive`:

```sh
neut archive 0-1
ls ./archive # => 0-1.tar.zst
```

The argument of `neut archive` must be something like `0-1-0`, `2-3-1`, or `1-2-3-4-5-6`. The compiler interprets these names as semantic versions.

You can then upload these tarballs by pushing them to GitHub, for example.

## Adding Dependency Modules

`neut get` can be used to add dependencies:

```sh
# creates a new module
neut create new-item
cd new-item

# adds a sample module that contains `my-add` and `increment` to your module
neut get some-name https://github.com/vekatze/neut-sample/raw/main/archive/0-1.tar.zst
```

The command `neut get` fetches the tarball from the specified URL and adds it to the current module. The module can then be used as `some-name` in your module.

The information of the newly-added module is saved to `module.ens`:

```ens
{
  target {
    new-item {
      main "new-item.nt",
    },
  },
  dependency {
    core { .. },
    // ↓ HERE
    some-name {
      digest "..",
      mirror [
        "https://github.com/YOUR_NAME/YOUR_REPO_NAME/raw/main/archive/0-1.tar.zst",
      ],
    },
  },
}
```

<div class="info-block">

The "real" name of a dependency is the digest of the tarball. Names such as `some-name` are just aliases.

</div>

## Importing Files

### Importing Files in Dependencies

Dependencies can be used in your code, of course:

```neut
// new-item.nt

import {
  some-name.sample {my-add}, // imports `my-add` in `source/sample.nt`
}

define main(): unit {
  printf("{}\n", [my-add(10, 11)]) // ← using `my-add`
}
```

Let's focus on `import`. `import` consists of lines like the one below:

```neut
some-name.sample {my-add}
```

The first component of such a line (`some-name`) is our alias of the dependency.

What follows (`sample`) is the relative path to the file from the source directory of the dependency module. Here, you don't have to write the file extension `.nt`.

Like `{my-add}` in the example above, every item of an `import` can optionally have a list of names. Names in these lists are made available after `import`.

You can also use the fully-qualified form of `my-add`:

```neut
// new-item.nt

import {
  some-name.sample, // removed `{my-add}`
}

define main(): unit {
  // ↓ using the fully-qualified form of `my-add`
  printf("{}\n", [some-name.sample.my-add(10, 11)])
}
```

Also, files like `source/foo/item.nt` in `some-name` can be imported as follows:

```neut
import {
  some-name.foo.item,
}
```

### Importing Files in the Current Module

Let's try creating a new file `new-item/source/foo/greet.nt` with the following content:

```neut
// foo/greet.nt

define yo(): unit {
  print("Yo")
}
```

This file can then be used from `new-item/source/new-item.nt` as follows:

```neut
// new-item.nt

import {
  this.foo.greet {yo},
}

define main(): unit {
  yo()
}
```

That is, the name of the current module is always `this`.
