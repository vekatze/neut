# Editor Setup

## Emacs

<https://github.com/vekatze/neut-mode>

`neut-mode` is an Emacs major mode for Neut. The mode provides syntax highlighting and automatic indentation. It also provides a way to use the LSP server via `lsp-mode` and `eglot`.

`neut-mode` is available on [Melpa](https://melpa.org/#/neut-mode). You can install the package with:

```text
M-x package-install RET neut-mode RET
```

Below is an example configuration:

```lisp
(use-package neut-mode
  :init
  (defun my/neut-initialize ()
    (interactive)
    (when (featurep 'lsp-mode)
      (setq-local lsp-before-save-edits t)
      (lsp)))
  (defun my/neut-compile ()
    (interactive)
    (compile (concat "neut zen " (buffer-file-name))))
  (add-hook 'neut-mode-hook 'my/neut-initialize)
  :bind
  (:map neut-mode-map
        ("C-c C-c" . 'my/neut-compile)))
```

## Neovim

<https://github.com/vekatze/vim-neut>

You can install `vim-neut` for syntax highlighting and automatic indentation.

Install it with your preferred plugin manager, such as [lazy.nvim](https://github.com/folke/lazy.nvim).

If you want to use Neovim's built-in LSP client, you can use a configuration like the following:

```lua
vim.api.nvim_create_autocmd("FileType", {
  desc = "Launch Neut's LSP server",
  pattern = "neut",
  callback = function()
    vim.lsp.start({
      name = "neut lsp",
      cmd = { "neut", "lsp" },
      root_dir = vim.fs.dirname(vim.fs.find({ "module.ens" }, { upward = true })[1]),
    })
  end
})
```

Key points:

- Run `neut lsp` to start the LSP server
- Look for `module.ens` to find the root of a module

Any other LSP client for Neovim should work as well.

## Vim

<https://github.com/vekatze/vim-neut>

You can install `vim-neut` for syntax highlighting and automatic indentation.

Install it with your preferred plugin manager, such as [vim-plug](https://github.com/junegunn/vim-plug).

If you want to use [vim-lsp](https://github.com/prabirshrestha/vim-lsp), you can use a configuration like the following:

```vim
call plug#begin()

" ...

Plug 'vekatze/vim-neut'

Plug 'prabirshrestha/vim-lsp'

" ...

call plug#end()

if executable('neut')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'neut',
        \ 'cmd': {server_info->['neut', 'lsp']},
        \ 'allowlist': ['neut'],
        \ })
endif


function! s:on_lsp_buffer_enabled() abort
    " ... (see the readme of vim-lsp)
endfunction


augroup lsp_install
    " ... (see the readme of vim-lsp)
augroup END
```

Key points:

- Run `neut lsp` to start the LSP server
- Look for `module.ens` to find the root of a module

Any other LSP client for Vim should work as well.

## Visual Studio Code

<https://github.com/vekatze/vscode-neut>

You can install `vscode-neut` for syntax highlighting, automatic indentation, and LSP support. This extension is available on the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=vekatze.vscode-neut).
