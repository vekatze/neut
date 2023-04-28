# Main Ideas

Neut's approach to static memory management is twofold.

1. It firstly finds *a "fair" way* to compile the λ-calculus consistently, and then
2. spots out some *"cheatable" fragments* in the calculus.

Here, the consistent way is to translate types into functions than know how to copy/discard the terms of the types. This is explained in [the Section 3.1 (Executing Types)](./executing-types.md).

You'll see that the consistent way can sometimes result in an unsatisfactory runtime performance. [The Section 3.2 (Noetic Optimization)](./noetic-optimization.md) provides a way to achieve better performance.

Let's start from the "fair" part.
