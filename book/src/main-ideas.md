# Main Ideas

In this chapter, we'll see how Neut realizes static memory management.

[The Section 2.1 (In a Linear Language)](./in-a-linear-language.md) briefly examines a linear language to see how things behaves in a simplified scenario. Starting from here, we'll look for the way to exploit the power of linearity from a non-linear language.

[The Section 2.2 (Executing Types)](./executing-types.md) explains the idea of translating types into functions that can copy and discard the terms of the types. Neut's approach for static memory management is twofold, and this section is about the *"fair"* part of it.

[The Section 2.3 (Exponentials in Action)](./exponentials-in-action.md) supplements the previous section, showing how types in Neut are actually compiled to functions.

[The Section 2.4 (Noetic Optimization)](./noetic-optimization.md) provides a loophole to achieve better performance. Using this loophole, we can avoid certain unsatisfactory runtime behaviors caused by the "fair" translation. This section is about the *"cheat"* part of Neut's approach.

Let's start from linearity.
