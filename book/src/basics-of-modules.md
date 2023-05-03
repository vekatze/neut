# Basics of Modules

## Module

A module in Neut is a directory that contains a file named `module.ens`. As we've already seen, we can create a module by calling `neut create MODULE_NAME`:

```sh
neut create hey
tree hey
# => hey/
#    ├── source/
#    │  └── hey.nt
#    └── module.ens
```

Incidentally, compiled objects, caches, etc. are stored into the module's `.build/`.

### Main Module and Library Module

When running compilation, every module is marked as "main" or "library". The main module is the module in which `neut build` is executed. Library modules are all the other modules that are necessary for compilation. This distinction will be useful when considering name resolution, as you'll see in the next section.

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

Note that *the name of a library is defined by the user of the library*, not the creator of the library. We thus sometimes call a name like `core` as a module alias.

### Other Remarks

- The directory `~/.cache/neut` can be changed via `NEUT_CACHE_DIR`
