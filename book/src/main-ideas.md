# Main Ideas

In this chapter, we'll see how Neut realizes static memory management.

[Section 2.1 (In a Linear Language)](./in-a-linear-language.md) briefly examines a linear language to see how things behave in a simplified scenario. Starting from here, we'll look for a way to exploit the power of linearity from a non-linear language.

[Section 2.2 (Executing Types)](./executing-types.md) explains the idea of translating types into functions that can copy and discard the terms of the types. Neut's approach to static memory management is twofold, and this section is about the *"fair"* part of it.

[Section 2.3 (Exponentials in Action)](./exponentials-in-action.md) supplements the previous section, showing how concrete types in Neut are compiled into functions.

[Section 2.4 (Noetic Optimization)](./noetic-optimization.md) provides a loophole to achieve better performance. Using this loophole, we can avoid certain unsatisfactory runtime behaviors caused by the "fair" translation. This section is about the *"cheat"* part of Neut's approach.
