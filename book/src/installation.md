# Installation

## External Dependencies

Neut depends on `curl`, `tar`, `zstd` and `clang (>= 14.0.0)`. Please make sure all of them are installed.

### Notes on M1 mac

If you are using M1 mac, you may want to install M1 native clang from Homebrew and add the following configuration:

```sh
export NEUT_TARGET_ARCH=arm64
export NEUT_CLANG=/opt/homebrew/opt/llvm/bin/clang
```

This allows you to build M1 native (ARM64) binaries. Without this, you'll get AMD64 binaries.

## Using a Prebuilt Binary

You can get a prebuilt binary of Neut as follows:

```sh
# macOS
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-amd64-darwin

# Linux (amd64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-amd64-linux

# Linux (arm64)
curl -L -o ~/.local/bin/neut https://github.com/vekatze/neut/releases/latest/download/neut-arm64-linux
```

The path `~/.local/bin/` is just an example; You can change it to anywhere you like as long as the path is in your `$PATH`.

Also, don't forget to make it executable:

```sh
# make it executable
chmod +x ~/.local/bin/neut
```

We also need to register the URL and the digest of the core module (standard library):

```sh
# add the below to your bashrc, zshrc, etc.
export NEUT_CORE_MODULE_URL="https://github.com/vekatze/neut-core/raw/main/release/0-2-1.tar.zst"
export NEUT_CORE_MODULE_DIGEST="KdSwxXPhj0mNQ9yoWVJ7tRFrUSDlXAxWaSZJBngaRDU="
```

Now, let's create a sample project and build it to check if your installation is correct:

```sh
neut create sample
cd sample
cat source/sample.nt
# => define main(): unit {
#      print("Hello, world!\n")
#    }

# build & execute
neut build --execute
# => Hello, world!

# build & copy the resulting binary to ./bin
neut build --install ./bin
./bin/sample
# => Hello, world!
```

If no error is reported, you're ready. Let's go to the next section. If not, please follow your error message.

## Uninstallation

Just remove the binary and the directory `~/.cache/neut/`. Neut won't clutter your PC.

## Build by Yourself

### Locally

If you want, you can build the compiler by yourself. With [stack](https://docs.haskellstack.org/en/stable/) installed, do the following:

```sh
git clone https://github.com/vekatze/neut
cd neut
stack install # => the binary goes into ~/.local/bin/
```

### Using Docker

If you have [Docker](https://www.docker.com/) and [just](https://github.com/casey/just) installed, you can also take the following way:

```sh
git clone https://github.com/vekatze/neut
cd neut

# [for amd64 linux binary] (with docker desktop running)
just build-image-amd64-linux
just build-compiler-amd64-linux
cp ./bin/neut-amd64-linux ~/.local/bin/neut

# [for arm64 linux binary] (with docker desktop running)
just build-image-arm64-linux
just build-compiler-arm64-linux
cp ./bin/neut-arm64-linux ~/.local/bin/neut
```
