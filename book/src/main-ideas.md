# Main Ideas

In this chapter, we'll see how Neut realizes static memory management.

We firstly need a short prologue for our plot. That is, we need to see the fact that static memory management is almost trivial if every variable is used exactly once. This is done in [the Section 3.1 (Paradise of Linearity)](./paradise-of-linearity.md).

And the real story begins. Neut's approach to static memory management is twofold:

1. It firstly finds *a "fair" way* to compile the λ-calculus consistently, and then
2. spots out some *"cheatable" fragments* in the calculus.

[The Section 3.2 (Executing Types)](./executing-types.md) explains the "fair" way; A way to translate types into functions than know how to copy/discard the terms of the types.

You'll see that the consistent way can sometimes result in an unsatisfactory runtime performance. [The Section 3.3 (Noetic Optimization)](./noetic-optimization.md) provides a loophole to achieve better performance.

Let's start from linearity.
