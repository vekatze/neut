# Hello Modules

In this section, starting from the sacred hello world, we'll see how the development cycle in Neut proceeds.

## What You'll Learn Here

We'll explore how to use modules in Neut. More Specifically:

- How to _create_ a module
- How to _build_ a module
- How to _execute_ a module
- How to _add_ dependencies
- How to _publish_ your module

## Creating a Module

### Initialization

Run the following command:

```sh
neut create sample
```

This command creates a new directory `./sample/` and files inside the directory. This directory is an example of a module in Neut. A module in Neut is a directory that contains `module.ens`.

The command `create` creates a sample project that performs "hello world". This module can be built and executed by running the following commands:

```sh
cd ./sample
neut build sample --execute # => "Hello, world!"
```

Let's see what a module in Neut is like.

### Structure

The content should be something like the following:

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

You can change the locations of special directories such as `build` or `source` by using `module.ens`. See [Modules](./modules.md) for more information.

</div>

### module.ens

The content of `module.ens` should be something like the below:

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

`target` specifies the name and the main file of the resulting executables. In the case above, `neut build` will create an executable file `sample` by compiling sources using the `main` function in `sample.nt` as the entry point.

`dependency` specifies external dependencies. Since our running example doesn't do much, the only dependency is `core`, which is the same as "prelude" in other languages.

The `digest` is the base64url-encoded checksum of the tarball.

The `mirror` is a list of URLs of the tarball.

The `enable-preset` makes the `core` library behave like Prelude in Haskell. That is, when `enable-preset` is true, specified names in the dependency are automatically imported into every file in our module.

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

// The Haskell equivalent of the above is (ignoring variable naming conventions):
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

You can also obtain the resulting binary:

```sh
neut build sample --install ./bin # creates a directory `bin` and put the resulting binary there
./bin/sample # => 42
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

As in F#, statements in Neut are order-sensitive. If you were to define `main` before `my-add`, the code won't compile. For forward references, you'll have to explicitly declare names beforehand using a statement called `nominal`, which we'll see in the next section.

</div>

## Publishing Your Module

Let's publish our module so others can use the functions `my-add` and `increment`.

You can create an archive of the current module using `neut archive`:

```sh
neut archive 0-1
ls ./archive # => 0-1.tar.zst
```

`neut archive` creates the directory `archive` at the root of your module. This command also creates an archive for the current module.

The name of a module archive must be something like `0-1`, `2-3-1`, `1-2-3-4-5-6`.

The compiler interprets the names of archives as semantic versions. For example, if you create an archive `1-2-3` and then `1-2-4`, the `1-2-4` is treated as a newer compatible version of `1-2-3`.

This tarball can be controlled with your version control system like Git and pushed to the remote repository, as usual:

```sh
# the usual git thing

pwd # => path/to/sample/

git init

git commit --allow-empty -m "initial commit"
echo "build" > .gitignore
git add .gitignore archive/ module.ens source/
git commit -m "whatever"

git remote add origin git@github.com:YOUR_NAME/YOUR_REPO_NAME.git
git push origin main
```

This tarball can be used as a dependency, as described in the next section.

## Adding Another Module to Your Module

`neut get` can be used to add external dependencies:

```sh
# create a new module
pwd # => ~/Desktop (for example)
neut create new-item
cd new-item

# ↓ add the previous module to our `new-item`
neut get some-name https://github.com/YOUR_NAME/YOUR_REPO_NAME/raw/main/archive/0-1.tar.zst

# you can try the following command for example:
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

## Using Dependencies

These dependencies can then be used in your code:

```neut
// new-item.nt

import {
  core.text.io {print-int},
  some-name.sample {my-add},
}

define main(): unit {
  print-int(my-add(10, 11)) // ← using `my-add`
}
```

Let's focus on `import`. This statement specifies the files we want to use in dependencies.

`import` consists of lines like the one below:

```neut
some-name.sample {my-add}
```

The first component of such a line (`some-name`) is our alias of the dependency.

What follows (`sample`) is the relative path to the file from the source directory of the dependency module. Here, you don't have to write the file extension `.nt`.

Like `{my-add}` in the example above, every bullet item of an `import` can optionally have a list of names. Names in these lists are made available after `import`, as in the example above.

Suppose you didn't write `{my-add}`. In this case, you can use the fully-qualified form of `my-add`:

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

So far, we have used the file `(source-directory)/sample.nt` in the dependency. What if the file we want to `import` isn't at the root of the source directory?

Suppose the dependency `some-name` contained a file `source/entity/item.nt`. In this case, the file `item.nt` can be imported from `new-item.nt` as follows:

```neut
import {
  some-name.entity.item,
}
```

We only have to add `.entity` to specify the path to the file. No surprises.

## Importing Files in the Current Module

We now know how to import files in dependencies. Using the same idea, we can also (of course) import files in the current module.

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

`import` can import multiple files and multiple names at the same time. For example, the following is a valid use of `import`:

```neut
import {
  this.foo.greet {yo},
  some-name.entity.item {add},
}
```

## Prefixed Import

We can also use so-called qualified imports as in Haskell. Let's remember the example of fully-qualified names:

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

We'll rewrite this example into a "prefixed" form. Firstly, edit the `module.ens` as follows:

```ens
{
  target {..},
  prefix {                  //
    S "some-name.sample",   // ← alias: S -> some-name.sample
  },                        //
  dependency {..},
}
```

By registering aliases of files in `module.ens`, you'll be able to use prefixes as aliases for specified files.

We can now rewrite the `new-item.nt` as follows:

```neut
// new-item.nt

import {
  S, // == some-name.sample
  core.text.io {print-int},
}

define main(): unit {
  print-int(S.my-add(10, 11))
}
```

<div class="info-block">

Unlike Haskell, these prefixes are defined per module, not per file. The prefixes of a file must be consistent inside a module.

</div>

## What You've Learned Here

- Use `neut create MODULE_NAME` to create a module
- Use `neut build TARGET_NAME` to build modules
- Use `neut build TARGET_NAME --execute` to execute modules
- Use `neut get` to add external dependencies
- Use `neut archive` and push it to publish modules
