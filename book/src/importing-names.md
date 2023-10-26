# Importing Names

## Importing Names From the Current Module

Suppose you created a file `source/foo/yo.nt` in your module, as follows:

```neut
// source/foo/yo.nt

define some-func(): int {
  1
}

define hey(): int {
  2
}
```

A file in the same module can be imported using `this`:

```neut
// source/hey.nt

import {
// from `/source/foo/yo.nt`, the below imports `some-func` and `hey`
- this.foo.yo [some-func, hey]
}

define main(): int {
  some-func()
}
```

The list of imported names can also be written vertically:

```neut
import {
- this.foo.yo {
  - some-func
  - hey
  }
}
```

You can also omit the list of imported names. In that case, you can use the imported names only via their fully qualified forms:

```neut
import {
- this.foo.yo
}

define main(): int {
  this.foo.yo.some-func()
}
```

Incidentally, omitting the list of imported names is equivalent to specifying the empty list as the list of imported names:

```neut
// the following two are equivalent

import {
- this.foo.yo
}

import {
- this.foo.yo []
}
```

## Importing Names From a Library Module

Suppose that you have added a library module to your module:

```ens
{
  // ...
  dependency {
    core {
      digest "tdD1XQaQGOrgbuHz7sbvY_3hudbRjp_XsNB3ExWXL_g="
      mirror [
        "https://github.com/vekatze/neut-core/raw/main/archive/0-10.tar.zst"
      ]
    }
  }
  // ...
}
```

You can import a file from such a library module by specifying its module alias and the relative path to the file:

```neut
import {
// from core's `/source/text/io.nt`, the below imports `get-line`
- core.text.io [get-line]
}

define sample() {
  let v = get-line() in
  ...
}
```

Here, the module alias of `core.text.io` is `core`, and the relative path is `text.io`. Internally, when compiling a library module, the compiler adds the following correspondence from the current module's `module.ens`:

```sh
core => DIGEST_OF_THE_LIBRARY

# core => tdD1XQaQGOrgbuHz7sbvY_3hudbRjp_XsNB3ExWXL_g=
```

and do the following name resolution:

```text
core.text.io.get-line

↓

tdD1XQaQGOrgbuHz7sbvY_3hudbRjp_XsNB3ExWXL_g=.text.io.get-line
```

## Module-Based Qualified Import

Names can also be imported via "prefixes". You can define prefixes in your `module.ens`:

```ens
{
  // ...
  prefix {
    hey "this.foo.yo" // `hey` is a prefix of `this.foo.yo`
    hoo "this.bar"    // `hoo` is a prefix of `this.bar`
  }
}
```

These prefixes can then be used in source files of your module:

```neut
import {
- hey // "hey.*" ~> "this.foo.yo.*"
- hoo // "hoo.*" ~> "this.bar.*"
}

define main(): int {
  let _ = hey.func1() in  // `hey.func1()` ~> `this.foo.yo.func1()`
  let _ = hoo.func2() in  // `hoo.func2()` ~> `this.bar.func2()`
  ..
}
```

This module-based approach forces us to use prefixes in a consistent manner within a module.

## Behind The Scenes: Resolving `this`

Remember that every module is whether the main or a library. The name of a global variable is resolved using this distinction.

Firstly, `this` in the main module is kept intact. Thus, the resulting assembly file contains a symbol like `this.foo.yo.some-func`.

On the other hand, `this` in a library module is resolved into its corresponding digest. More specifically, when compiling a library module, the compiler adds the following correspondence to its internal state:

```sh
this => DIGEST_OF_THE_LIBRARY

# e.g. this => jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=
```

and resolves `this` like below:

```text
this.text.io.get-line

↓

jIx5FxfoymZ-X0jLXGcALSwK4J7NlR1yCdXqH2ij67o=.text.io.get-line
```

## Other Notes on Namespaces and Modules

- A module named `core` is treated specially (the prelude library)
- Compiled objects, caches, etc. are stored in `build/` of the corresponding module
