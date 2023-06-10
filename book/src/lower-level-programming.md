# Lower Level Programming

Here, we'll see some magical stuff in Neut.

[Section 4.1 (Introspection)](introspection.md) shows how to get some information from the compiler. This is useful when writing platform-dependent code.

[Section 4.2 (Magic)](magic.md) shows some black magic. Here, you'll see how to perform arbitrary casting, LLVM-level memory handling, etc.

[Section 4.3 (Resource Type)](resource-type.md) explains a new way to define a type. Remember that a type in Neut is translated into a function that copies/discards the values of the type. This section reverses the storyline: We define a type by specifying how to copy/discard the values of the type.
