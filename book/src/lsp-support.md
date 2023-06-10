# LSP Support

The compiler also contains an experimental LSP server to provide a better development experience in an editor-agnostic way. The LSP server currently supports:

1. jump to definition,
2. linting on save, and
3. function name completion.

You can launch the LSP server by running `neut lsp`. Thus, for example, if you're using `eglot`, you can set up the LSP server as follows:

```neut
(add-to-list 'eglot-server-programs
             `(neut-mode . ("neut" "lsp" "--no-color")))
```

The command `neut lsp` must be run inside a project directory. Otherwise, it will report an error something like below:

```text
error: couldn't find a module file (context: /Users/foo/)
```
