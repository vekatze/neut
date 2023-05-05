# Some Backstage Notes

## What are the Possible Drawbacks of Neut's Approach?

The prominent one that comes to my mind is that, when you use a polymorphic variant type, its type arguments must be stored in its values. For example, consider something like below:

```neut
// a syntax to define an ADT
variant Foo(a) {
- ConsA(i64, bool) // e.g. ConsA(3, True): Foo(a)
- ConsB(a)
}
```

In most language, the internal representation of `ConsA(3, True)` will be something like:

```neut
// `0` is a tag to distinguish constructors
(0, 3, True)
```

However, in Neut, the type information must also be stored:

```neut
// `TYPE` is the address of the type (as function) `a`
(0, TYPE, 3, True)
```

This means that in Neut you must pay additional spaces to use parameterized variant types. So, it might be necessary to use non-parameterized variant types in performance-critical situations, like:

```neut
variant FooText {
- ConsA(i64, bool) // e.g. ConsA(3, True): Foo(a)
- ConsB(text)
}
```

In that case, the internal representation will be `(0, 3, True)`. We'll have to be careful about what we have to pay for polymorphism.

## Why was it Named Neut?

During compilation, every program in Neut is polarized into its positive fragment and its negative fragment, as done in [Call-by-Push-Value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf). From this perspective, every term in a source file of this language can be seen as neutral. So, just as `.txt` is used as the file extension for texts, I chose to use `.neut` (which is now `.nt`) as the file extension for neutral terms. Over time, it started to look like a proper noun, and I decided to adopt it as the name of the language.

(I don't by any means claim this obviously opinionated language to be more neutral than others)

## How Did Neut Find the Approach?

By trying to utilize the other side of logical harmony in the sense of Michael Dummett.

From the viewpoint of natural deduction, executing a program is reducing detours in a proof tree, as is well known (the lovely Curry-Howard correspondence). Such a reduction of a proof tree can be seen as a utilization of local soundness.

On the other hand, the flip side of harmony, namely local completeness, is often ignored when thinking about programs. At least, that was how it seemed to me N years ago. It made me wonder: Why does local completeness seem to play almost no role in a program when local soundness handles the time aspect of a program, which is of great importance?

Led by the question, I tried to interpret local completeness as something related to the space aspect of a program. Then I noticed that the ability of local expansion, which is a fruit of local completeness, can be interpreted as our knowledge about the structure of the values of the type, which in turn means we can copy and discard values using their type. Here lies the idea of Neut.

A good lecture note on harmony can be found [here](https://www.cs.cmu.edu/~fp/courses/15317-f09/lectures/03-harmony.pdf).
