# Modules and Sources

In this section, we'll see how to use modules in Neut, using the example of the usual hello world.

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

You can build and execute this module by running the following commands:

```sh
cd ./sample
neut build sample --execute # => "Hello, world!"
```

You can also obtain the resulting binary:

```sh
neut build sample --install ./bin # creates a directory `bin` if necessary
./bin/sample # => "Hello, world!"
```

### Structure of a Module

The content of a module should be something like the following:

```text
sample/
├── cache/
│  └── ...
├── source/
│  └── sample.nt
└── module.ens
```

The directory `cache` is where object files (binary files) and dependencies are put in. You won't have to go into the directory for daily use.

The directory `source` is where we put source files.

The file `module.ens` contains meta information about this project, such as dependencies.

<div class="info-block">

You can change the locations of special directories such as `cache` via `module.ens`. See [Modules](./modules.md) for more information.

</div>

### module.ens

The content of `module.ens` should be something like the following:

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

`target` specifies the name and the main file of the resulting executables. In the case above, `neut build` will create an executable named as `sample` by compiling sources using the `main` function in `sample.nt` as the entry point.

`dependency` specifies external dependencies. Since our running example doesn't do much, the only dependency is `core`, which is the same as "prelude" in other languages.

The `digest` is the base64url-encoded checksum of the tarball.

The `mirror` is a list of URLs of the tarball.

The `enable-preset` makes the `core` library behave like Prelude in Haskell. That is, when `enable-preset` is true, specified names in the dependency are automatically imported into every file in our module.

## Basics of Source Files

### Source files

Let's see the content of `source/sample.nt`:

```neut
// sample.nt

define main(): unit {
  print("Hello, world!\n") // `print` is defined in `core`
}
```

The above code defines a function `main` that returns a value of type `unit`. This function prints `"Hello, world!\n"`.

Here, the `unit` is an ADT that contains only one value `Unit`. The explicit definition of `unit` is as follows:

```neut
data unit {
| Unit
}

// The Haskell equivalent of the above is:
//   data unit =
//     Unit
```

Let's try editing the code as follows:

```neut
// sample.nt
import {
  core.text.io {print-int},
}

define main(): unit {
  print-int(42) // `print-int` is also defined in `core`
}
```

Then, build the project:

```sh
neut build sample --execute # => 42
```

### Defining a function

Of course, you can define a function:

```neut
// sample.nt
import {
  core.text.io {print-int},
}

define get-int(): int {
  42
}

define main(): unit {
  print-int(get-int())
}
```

A function can take arguments. Let's rewrite `sample.nt` into the below:

```neut
// sample.nt
import {
  core.text.io {print-int},
}

define increment(x: int): int {
  add-int(x, 1)
}

define my-add(x: int, y: int): int {
  add-int(x, y)
}

define main(): unit {
  print-int(my-add(10, increment(10))) // # => 21
}
```

Top-level items like `define` are called statements. You'll see more in the next section.

<div class="info-block">

As in F#, statements in Neut are order-sensitive. If you define `main` before `my-add`, the code won't compile. For forward references, you'll have to explicitly declare names beforehand using a statement called `nominal`, which we'll see in the next section.

</div>

## Publishing Modules

Let's publish our module so others can use the functions `my-add` and `increment`.

You can create a tarball snapshot (an archive) of your module using `neut archive`:

```sh
neut archive 0-1
ls ./archive # => 0-1.tar.zst
```

The name of an archive must be something like `0-1`, `2-3-1`, `1-2-3-4-5-6`.

The compiler interprets the names of archives as semantic versions. For example, if you create an archive `1-2-3` and then `1-2-4`, the `1-2-4` is treated as a newer compatible version of `1-2-3`.

These archives are intended to be controlled with your version control system like Git:

```sh
# the usual git thing

pwd # => path/to/sample/

git init

git commit --allow-empty -m "initial commit"
echo "cache" > .gitignore
git add .gitignore archive/ module.ens source/
git commit -m "yo"

git remote add origin git@github.com:YOUR_NAME/YOUR_REPO_NAME.git
git push origin main
```

## Adding Dependency Modules

`neut get` can be used to add external dependencies:

```sh
# create a new module
pwd # => ~/Desktop (for example)
neut create new-item
cd new-item

# ↓ this command adds the previous module to our `new-item`
neut get some-name https://github.com/YOUR_NAME/YOUR_REPO_NAME/raw/main/archive/0-1.tar.zst

# (↓ you can try the following sample project instead)
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

The "real" name of an archive is the digest of the tarball. You define an alias of the module for your convenience.

</div>

## Importing Files

### Importing Files in Dependencies

Added dependencies can then be used in your code, of course:

```neut
// new-item.nt

import {
  core.text.io {print-int},
  some-name.sample {my-add}, // imports `my-add` in `source/sample.nt`
}

define main(): unit {
  print-int(my-add(10, 11)) // ← using `my-add`
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
  core.text.io {print-int},
  some-name.sample, // removed `{my-add}`
}

define main(): unit {
  // ↓ using the fully-qualified form of `my-add`
  print-int(some-name.sample.my-add(10, 11))
}
```

Also, files like `source/foo/item.nt` in `some-name` can be imported as follows:

```neut
import {
  some-name.foo.item,
}
```

### Importing Files in the Current Module

Let's create a new file `new-item/source/foo/greet.nt` with the following content:

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

### Qualified Import

We can also use so-called qualified imports. Let's remember the example of fully-qualified names:

```neut
// new-item.nt

import {
  core.text.io {print-int},
  some-name.sample,
}

define main(): unit {
  print-int(some-name.sample.my-add(10, 11))
}
```

We'll define an alias for `some-name.sample`. Firstly, edit the `module.ens` as follows:

```ens
{
  target {..},
  prefix {                  //
    S "some-name.sample",   // ← alias: S -> some-name.sample
  },                        //
  dependency {..},
}
```

We can now rewrite the `new-item.nt` as follows:

```neut
// new-item.nt

import {
  S, // == some-name.sample
  core.text.io {print-int},
}

define main(): unit {
  print-int(S.my-add(10, 11)) // S.my-add == some-name.sample.my-add
}
```

<div class="info-block">

Unlike Haskell, these prefixes are defined per module, not per file. The prefixes must be consistent inside a module.

</div>
