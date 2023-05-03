# Main Ideas

In this chapter, we'll see how Neut realizes static memory management.

[The Section 2.1 (Paradise of Linearity)](./paradise-of-linearity.md) firstly check the fact that static memory management is almost trivial if every variable is used exactly once. This is a short prologue for our plot.

[The Section 2.2 (Executing Types)](./executing-types.md) explains how Neut translates types into functions that can copy and discard the terms of the types. This linearizes the whole language in a consistent manner. This section is for the *"fair"* part of Neut's approach to static memory management.

[The Section 2.3 (Noetic Optimization)](./noetic-optimization.md) provides a loophole to achieve better performance. Using this loophole, we can avoid certain unsatisfactory runtime behaviors caused by the "fair" translation. This section is for the *"cheat"* part of Neut's approach.

Let's start from linearity.
