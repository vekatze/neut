# Export and Import

## Exporting Names

By default, every name in a file can't be seen from any other files (i.e. private). You can make names public by using `export`:

```neut
// source/foo/item.nt

export {
- item-func // `item-func` can now be used from other files
}

define item-func(): int {
  some-private-func()
}

// this definition can't be seen from any other files
define some-private-func(): int {
  2
}
```

ADTs and their constructors can be exported as follows:

```neut
export {
- my-item {..} // `{..}` exports all its constructors
- other-item { // export an ADT
  - Buz        // ... and its constructors
  - Qux
  }
}

data my-item(a) {
- Foo
- Bar(int, a)
}

data other-item {
- Buz
- Qux
}
```

## Importing Names

### From the Current Module

Suppose you created a file `source/foo/yo.nt` in your module, as follows:

```neut
// source/foo/yo.nt

export {
- yo-func
}

define yo-func(): int {
  1
}
```

A file in the same module can be imported using `this`:

```neut
// source/hey.nt

import {
- this.foo.yo // imports source/foo/yo.nt
}

define main(): int {
  yo-func()
  // you can also write this.foo.yo.yo-func()
}
```

### From a Library Module

Suppose that you have added a library module to your module:

```text
dependency = {
  core = {
    URL = "https://github.com/vekatze/neut-core/raw/main/release/0.2.0.4.tar.zst"
    checksum = "jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o="
  }
}
```

You can import a file from such a library module by specifying its module alias and the relative path to the file:

```neut
import {
- core.text.io // ← imports core's /source/text/io.nt
}

define sample() {
  let v = core.text.io.get-line() // read line from stdin
  // you can also write `let v = target-line()`, of course
  ...
}
```

Here, the module alias is `core`, and the relative path is `text.io`. Internally, when compiling a library module, the compiler adds the following correspondence from the current module's `module.ens`:

```sh
core => CHECKSUM_OF_THE_LIBRARY

# core => jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=
```

and do the following name resolution:

```text
core.text.io.get-line

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=.text.io.get-line
```

### Behind The Scenes: Resolving `this`

Remember that every module is whether main or library. The name of a global variable is resolved using this distinction.

Firstly, `this` in the main module is kept intact. Thus, the resulting assembly file contains a symbol like `this.foo.yo.yo-func`.

On the other hand, `this` in a library module is resolved into its corresponding checksum. More specifically, when compiling a library module, the compiler adds the following correspondence to its internal state:

```sh
this => CHECKSUM_OF_THE_LIBRARY

# e.g. this => jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=
```

and resolves `this` like below:

```text
this.text.io.get-line

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=.text.io.get-line
```

### Qualified Import

Names in other files can be qualified as follows:

```neut
//
// source/foo/item.nt
//

export {
- item-func
}

define item-func(): int {
  2
}

//
// source/main.nt
//

import {
- this.yo
- this.foo.item => I
}

define main(): int {
  I.item-func()
  // `item-func()` will results in an error
}
```

By the way, when you define an ADT, I recommend you *not* to prefix constructors like the below:

```neut
data term {
- TermVar(ident)
- TermAbs(ident, term)
- TermApp(term, term)
}
```

Rather, create a new file for the ADT (if necessary), then simply write:

```neut
data term {
- Var(ident)
- Abs(ident, term)
- App(term, term)
}
```

and use them via qualified import. The same goes for functions. Please [name your functions and types for qualified import](https://mail.haskell.org/pipermail/haskell-cafe/2008-June/043986.html).

### Importing & Re-Exporting Names

You can also re-export names from other files:

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

define foo(): int {
  let _ = item-func() // using re-exported name
  item-func-alias()   // using re-exported alias
}
```

## Other Notes on Namespaces and Modules

- A module named `core` is treated specially (the prelude library)
- Compiled objects, caches, etc. are stored in `.build/` of the corresponding module
