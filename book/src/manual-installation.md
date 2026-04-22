# Manual Installation

Make sure the following dependencies are installed on your system:

- `clang (>= 15.0.0)`
- `curl`
- `tar`
- `zstd`

Add the following to your `.bashrc`, `.zshrc`, etc.:

```sh
# this sets the core module (or "prelude") that is used in `neut create`
export NEUT_CORE_MODULE_URL="https://github.com/vekatze/neut-core/raw/main/archive/0-54-9.tar.zst"
export NEUT_CORE_MODULE_DIGEST="yNWbpR6FH89l8vrvc2fh4PMZYlLEGtCzBHxT8aZQS5o"
```

Then, download the compiler binary:

```sh
mkdir -p ~/.local/bin/

# macOS (arm64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-darwin
# Linux (amd64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-amd64-linux
# Linux (arm64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-linux

# make it executable
chmod +x ~/.local/bin/neut
```

Ensure that the binary is in your `$PATH`; in the example above, it is placed in `~/.local/bin`.

You can check if the compiler is installed correctly by running `neut version`:

```sh
neut version
#=> X.Y.Z
```
