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
      -- The root of a Neut module is the directory containing `module.ens`.
      root_dir = vim.fs.dirname(vim.fs.find({ "module.ens" }, { upward = true })[1]),
    })
  end,
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client then return end

    -- Format on save.
    if client:supports_method("textDocument/formatting") then
      local grp = vim.api.nvim_create_augroup("lsp_format_on_save_" .. bufnr, { clear = true })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = grp,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({ bufnr = bufnr, async = false, id = client.id })
        end,
      })
    end

    -- Highlight references to the symbol under the cursor.
    if client:supports_method("textDocument/documentHighlight") then
      local grp = vim.api.nvim_create_augroup("lsp_document_highlight_" .. bufnr, { clear = true })
      vim.api.nvim_create_autocmd("CursorHold", {
        group = grp, buffer = bufnr, callback = vim.lsp.buf.document_highlight,
      })
      vim.api.nvim_create_autocmd("CursorMoved", {
        group = grp, buffer = bufnr, callback = vim.lsp.buf.clear_references,
      })
    end
  end,
})
```

For completion, you can either use Neovim's built-in `vim.lsp.completion` or a dedicated completion engine. Below is an example using [nvim-cmp](https://github.com/hrsh7th/nvim-cmp) with [cmp-nvim-lsp](https://github.com/hrsh7th/cmp-nvim-lsp) and [LuaSnip](https://github.com/L3MON4D3/LuaSnip):

```lua
local cmp = require("cmp")
cmp.setup({
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },
  sources = {
    { name = "nvim_lsp" },
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-l>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
})
```

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

Any other LSP client for Vim should work as well.

## Visual Studio Code

<https://github.com/vekatze/vscode-neut>

You can install `vscode-neut` for syntax highlighting, automatic indentation, and LSP support. This extension is available on the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=vekatze.vscode-neut).
