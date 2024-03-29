# Basics of Modules

## Module

A module in Neut is a directory (and its contents) that contains a file named `module.ens`. As we've already seen, we can create a module by calling `neut create MODULE_NAME`:

```sh
neut create hey
tree hey
# => hey/
#    ├── source/
#    │  └── hey.nt
#    └── module.ens
```

### Main Module and Library Module

When running compilation, every module is marked as "main" or "library". The main module is the module in which `neut build` is executed. Library modules are all the other modules that are necessary for compilation. This distinction will be useful when considering name resolution, as you'll see in the next section.

### Adding Module Dependencies

A module can use other modules. Such module dependencies can be added using `neut add`:

```sh
neut add core https://github.com/vekatze/neut-core/raw/main/archive/0-12.tar.zst
```

By running the code above, the specified tarball is downloaded into `~/.cache/neut/library`:

```sh
ls ~/.cache/neut/library
# => ...
#    Rmtqvu9H0ipzJFaxObbmT8qN1MCkgt3AJtDHMr4dhkg=
#    ...
```

where the `Rmtqvu9H0ipzJFaxObbmT8qN1MCkgt3AJtDHMr4dhkg=` is the digest of the module. Also, the module information is added to the current module's `module.ens`:

```ens
{
  // ...
  dependency {
    core {
      digest "Rmtqvu9H0ipzJFaxObbmT8qN1MCkgt3AJtDHMr4dhkg="
      mirror [
        "https://github.com/vekatze/neut-core/raw/main/archive/0-12.tar.zst"
      ]
    }
  }
  // ...
}
```

It might be worth noting that an alias of a library is chosen *by a user of the library*. Indeed, in the example above, we (user) chosed the alias `core` for the downloaded library. The "true" name of the library is its digest, and this is not something that can be chosen arbitrarily by the creator of the library.
