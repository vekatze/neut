# Installation / Uninstallation

## Installation

Make sure all the following dependencies are on your system:

- `clang (>= 15.0.0)`
- `curl`
- `tar`
- `zstd`

Add the below to your `.bashrc`, `.zshrc`, etc.

```sh
# select the core module (or "prelude")
export NEUT_CORE_MODULE_URL="https://github.com/vekatze/neut-core/raw/main/archive/0-37.tar.zst"
export NEUT_CORE_MODULE_DIGEST="ocDmPr9kkTZJMkJnYpZGrX8-skEB0YUCls5HeWSb7r8"
```

Then get the compiler:

```sh
# macOS (arm64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-darwin
# Linux (amd64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-amd64-linux
# Linux (arm64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-linux

# make it executable
chmod +x ~/.local/bin/neut
```

... and test your installation:

```sh
# the obligated "hello world"
neut create sample
cd sample
neut build --execute # => Hello, world!
```

## Uninstallation

Just remove the binary and the directory `~/.cache/neut/`. Neut won't clutter your PC.
