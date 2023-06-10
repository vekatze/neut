# Publishing Your Code

## Creating an Archive for Your Module

You can create a tarball for your module using `neut release VERSION`:

```sh
# create a new release
neut release 0-1-0-0

tree .
# => ./
#    ├── release/
#    │  └── 0-1-0-0.tar.zst ← NEW!
#    ├── source/
#    │  └── hey.nt
#    └── module.ens
```

The command `neut release` creates a tarball for the current module. The resulting tarball can be, for example, committed and pushed to your repository. Then, a user of your library can then add it to their module using `neut add ALIAS URL`.

## Identity of a Module

As we've seen, Neut identifies a module using its tarball's checksum, which I believe isn't a very common approach.

Now, suppose that you released a library `X` with version `1-0-0`. The library contains a file like below:

```neut
data some-type {
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

One day you find a bug in `func`. You fix this bug and release the new version as `1-0-1`. Both of the definitions of `some-time` and `add-bar` are the same between `1-0-0` and `1-0-1`, of course.

Now, if every package was naively identified only by its checksum, the `add-bar` in `1-0-1` cannot be applied to values of type `some-type` in `1-0-0`, since the argument of `add-bar` in `1-0-1` must be of type `some-type` in `1-0-1`.

Therefore, if we use a library `A` which uses `X` of version `1-0-0`, and also a library `B` which uses `X` of version `1-0-1`, we can't make these two libraries `A` and `B` cooperate in our module. I believe this isn't desirable. We would like to equate the two compatible versions in some way.

Neut resolves this problem via an approach based on Semantic Versioning, as described below.

## Practical Significance of Semantic Versioning

Firstly, the `VERSION` in `neut release VERSION` must follow a versioning scheme that is essentially the same as Semantic Versioning.

If you release a version of your module that is compatible with existing modules, *the compatibility information is saved to the module file in the newly-created tarball*. The module file thus contains the list of checksums of all the compatible but older alternatives.

For example, suppose you have `release/0-1-0-0.tar.zst`. If you run `neut release 0-1-1-0`, which is a minor update release, the module file in `release/0-1-1-0.tar.zst` should contain:

```text
antecedent = [
  "-doDFqpUZ2qCm2bsO_hpZ51WG3FxoVKSmzjue0sKh0g="
]
```

where the `-doDFqpUZ2qCm2bsO_hpZ51WG3FxoVKSmzjue0sKh0g=` is the checksum of the existing tarball `0-1-0-0.tar.zst`. The value of `antecedent` stores all the checksums of the existing compatible releases. In this way, the module file stores compatibility information.

This compatibility information is then used by the compiler. When building a module, the compiler constructs a graph of antecedents, and *always uses the latest compatible version of a module by tracing the graph* during compilation.

In this sense, the field `antecedent` defines the identity of a library.

## Actual Versioning Scheme

Versions like `1-0-0`, `10-2`, `0-1-3`, or `0-0-0-1-8-0` are supported. Each natural number between `-` is called a version segment. The string until the first non-zero version segment is the major version of the release. For example, the major version of `10-2-8` is `10`. That of `0-0-0-1-8-0` is `0-0-0-1`.

## On What a Breaking Change Is

A breaking change in Neut is basically the same as that of Semantic Versioning. However, please note that *adding a function to an existing file is also considered to be a breaking change*. This is because it could cause name conflict in a user's source files.

If you want to add functions in non-major updates, create a new file and add them to the file.

Specifying non-compatible releases as compatible is regarded as a bug of a release. It is expected to be fixed in a later release, just like other bugs.
