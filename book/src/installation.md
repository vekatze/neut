# Installation

## Installation

To install the toolchain, execute [the installation script](https://raw.githubusercontent.com/vekatze/neut/main/install.sh):

```sh
curl -sSL https://raw.githubusercontent.com/vekatze/neut/main/install.sh | bash
```

Then please follow the instructions.

You might also want to [configure your editor](./editor-setup.md).

If you prefer manual installation, see [here](./manual-installation.md).

## Uninstallation

To uninstall,

1. remove the binary `~/.local/bin/neut`,
2. remove `NEUT_CORE_MODULE_URL` and `NEUT_CORE_MODULE_DIGEST` in your shell config, and
3. remove cache directories in your projects (if desired).
