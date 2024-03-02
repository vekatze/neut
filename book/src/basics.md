# Basis

- call-by-value
- the content of a variable is copied `n-1` times when the variable is used `n` times
- the content of a variable is discarded if the variable isn't used

## Value

## Resource Exponentials

- `base.#.imm` is a closed function that (..).
- `base.#.cls` is a closed function that (..).

## Allocation Canceling

## Symbols

## Configuration

The behavior of the compiler can be adjusted using the following environment variables:

| Environment Variable      | Meaning                           |
| ------------------------- | --------------------------------- |
| `NEUT_CACHE_DIR`          | the directory used to save caches |
| `NEUT_CLANG`              | the command to call `clang`       |
| `NEUT_CORE_MODULE_DIGEST` | the digest of the core module     |
| `NEUT_CORE_MODULE_URL`    | the URL of the core module        |
| `NEUT_TARGET_ARCH`        | target arch (`amd64` or `arm64`)  |
| `NEUT_TARGET_OS`          | target OS (`linux` or `darwin`)   |

The default values are as follows:

| Environment Variable      | Default Value                 |
| ------------------------- | ----------------------------- |
| `NEUT_CACHE_DIR`          | `$XDG_CACHE_HOME/neut`        |
| `NEUT_CLANG`              | `clang`                       |
| `NEUT_CORE_MODULE_DIGEST` | (undefined; you must set one) |
| `NEUT_CORE_MODULE_URL`    | (undefined; you must set one) |
| `NEUT_TARGET_ARCH`        | (host's arch)                 |
| `NEUT_TARGET_OS`          | (host's OS)                   |
