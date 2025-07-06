# Manual Installation

Make sure all the following dependencies are on your system:

- `clang (>= 15.0.0)`
- `curl`
- `tar`
- `zstd`

Add the below to your `.bashrc`, `.zshrc`, etc.

```sh
# this sets the core module (or "prelude") that is used in `neut create`
export NEUT_CORE_MODULE_URL="https://github.com/vekatze/neut-core/raw/main/archive/0-51-10.tar.zst"
export NEUT_CORE_MODULE_DIGEST="NV9vKfEBwxCbNW381wEGXsqYkJqFpxBceqhwyaw4oog"
```

Then, get the compiler:

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

Ensure that `~/.local/bin` is in your `$PATH`.

<div class="info-block">

You can put the binary anywhere you want as long as the location is in your `$PATH`.

</div>

You can check if the compiler is installed correctly by running `neut version`:

```sh
neut version
#=> X.Y.Z
```
