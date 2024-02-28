# Editor Support

Supported editors are (in alphabetical order):

- Emacs
- Vim
- Visual Studio Code

## Emacs

<https://github.com/vekatze/neut-mode>

`neut-mode` is an Emacs major mode for Neut. The mode provides syntax highlighting and automatic indentation. It also provides a way to use the LSP server via `lsp-mode` and `eglot`.

You can install it using, for example, [straight.el](https://github.com/radian-software/straight.el) as follows:

```text
(use-package neut-mode
  :straight
  (:host github :repo "vekatze/neut-mode"))
```

### lsp-mode

To enable `lsp-mode`, you can do the following:

```text
M-x neut-mode-setup-lsp-mode
M-x lsp
```

### eglot

To enable `eglot`, you can do the following:

```text
M-x neut-mode-setup-eglot
M-x eglot
```

## Vim

TBD

## Visual Studio Code

TBD
