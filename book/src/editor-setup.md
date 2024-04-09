# Editor Setup

## Emacs

<https://github.com/vekatze/neut-mode>

`neut-mode` is an Emacs major mode for Neut. The mode provides syntax highlighting and automatic indentation. It also provides a way to use the LSP server via `lsp-mode` and `eglot`.

You can install it using, for example, [straight.el](https://github.com/radian-software/straight.el) as follows:

```lisp
(use-package neut-mode
  :straight
  (:host github :repo "vekatze/neut-mode"))
```

## Neovim

<https://github.com/vekatze/vim-neut>

You can install `vim-neut` for syntax highlighting and automatic indentation.

The installation process is as usual. Just use your favorite plugin manager like [lazy.nvim](https://github.com/folke/lazy.nvim).

If you want to use the built-in LSP client, for example, you can do something like the following:

```vim
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

The points are as follows:

- Run `neut lsp` to start the LSP server
- Look for `module.ens` to find the root of a module

You should be able to use any other LSP clients for Neovim.

## Vim

<https://github.com/vekatze/vim-neut>

You can install `vim-neut` for syntax highlighting and automatic indentation.

The installation process is as usual. Just use your favorite plugin manager like [vim-plug](https://github.com/junegunn/vim-plug).

If you want to use [vim-lsp](https://github.com/prabirshrestha/vim-lsp), for example, you can do something like the following:

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

The points are as follows:

- Run `neut lsp` to start the LSP server
- Look for `module.ens` to find the root of a module

You should be able to use any other LSP clients for Vim.

## Visual Studio Code

<https://github.com/vekatze/vscode-neut>

You can install `vscode-neut` for syntax highlighting, automatic indentation, and LSP support. This extension is available on the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=vekatze.vscode-neut).
