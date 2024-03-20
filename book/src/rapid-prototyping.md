# Rapid Prototyping

A compiler command named `zen` can be used to think about your new function.

Suppose that you created a new function deep inside your module.

You can technically create a test function for the function and check its behavior. Or modify the main function to call the new function just to see its behavior. However, it is a bit cumbersome to my (and hopefully our) liking. I need rapid try-and-error cycles in certain circumstances.

The command `zen` can be used here. Suppose that a file `some-file.nt` contains a function `foo` that is defined as in the below:

```neut
define foo(x: int): int {
  do-complex-calculation(x)
}
```

The behavior of `foo` can be inspected rapidly by defining a function named `zen` in the file:

```neut
// the type of `zen` must be `() -> unit`
define zen(): unit {
  print-int(foo(10))
}
```

and execute the following command:

```sh
neut zen path/to/some-file.nt # => (the result of `foo(10)` is printed)
```

This can be done even if `some-file.nt` isn't an entrypoint of the module. You can think of functions named `zen` as alternative `main`s.

## Zen Experience

You can configure your editor to call `neut zen path/to/current/file.nt`. Also create a key binding for that operation.

Below is an example where Emacs executes this `zen` command when `C-c C-c` is typed:

![zen](./image/screencasts/zen.gif "zen")

This feels a bit like a scripting language, doesn't it?

### Notes for Emacs

Incidentally, the configuration for Emacs in the above example is like the below:

```text
(defun ext/neut-compile ()
  (interactive)
  (compile (concat "neut zen " (buffer-file-name))))

(bind-key "C-c C-c" 'ext/neut-compile 'neut-mode-map)
```

The above example also uses the package `fancy-compilation`:

```text
(use-package fancy-compilation
  :init
  (fancy-compilation-mode t)
  (setq compilation-error-regexp-alist nil)
  (setq compilation-highlight-regexp nil)
  (setq compilation-mode-font-lock-keywords nil)
  (setq fancy-compilation-quiet-prelude t))
```
