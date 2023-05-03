# Lower Level Programming

Here, we'll see some magical stuff in Neut.

[The Section 4.1 (Introspection)](http://localhost:3000/introspection.html) shows how to get some information from the compiler. Using this feature, you can write platform-dependent code.

[The Section 4.2 (Magic)](http://localhost:3000/magic.html) shows some black magics. Here, you'll see how to perform arbitrary castng, LLVM-level memory handling, etc.

[The Section 4.3 (Resource Type)](http://localhost:3000/resource-type.html) explains a new way to define a type. Remember that a type in Neut is translated into a function that copies/discards the values of the type. This section reverses the storyline: we define a type by specifying how to copy/discard the values of the type.
