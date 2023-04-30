# Installation

## External Dependencies

Neut depends on `tar`, `curl`, and `clang (>= 14.0.0)`. Make sure all of them are installed.

## Using a Prebuilt Binary

If you're using Linux and the architecture is amd64 or arm64, you can use the following prebuilt binary:

```sh
# insert link here
```

If you're using macOS and the architecture is amd64 or arm64, you can use the following prebuilt binary:

```sh
# insert link here
```

Put one of them to your PATH like `~/.local/bin/`.

## Build by Yourself

You can build Neut by yourself. You need to install [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) to build Neut. With stack, installed, do the following:

```sh
git clone https://github.com/vekatze/neut
cd neut
git checkout 0.3.0.0
stack install # => the binary goes into ~/.local/bin
neut version # => 0.3.0.0
```

## Check If Installed Correctly

Let's create a sample project and build it to check if your installation is correct. Do the following:

```sh
neut create test
cd test
cat source/test.nt
# => define main(): i64 {
#      0
#    }
neut build --execute
# => The program simply returns 0; Thus nothing should happen
```

If no error is reported, you're ready. Go to the next section.
