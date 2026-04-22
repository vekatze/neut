# Installation

## Installing the Toolchain

To install the toolchain, execute [the installation script](https://raw.githubusercontent.com/vekatze/neut/main/install.sh):

```sh
curl -sSL https://raw.githubusercontent.com/vekatze/neut/main/install.sh | bash
```

Then follow the instructions.

You might also want to [configure your editor](./editor-setup.md).

If you prefer a manual installation, see [Manual Installation](./manual-installation.md).

## Uninstallation

To uninstall, remove the following:

1. the binary `~/.local/bin/neut`,
2. related environment variables from your shell config, and
3. cache directories in your projects (if desired).
