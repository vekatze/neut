# Some Random Notes

## Why Types are Chosen as Linearizer?

To utilize the other side of the logical harmony (in the sense of Michael Dummett).

## Why is it Named as Neut?

During compilation, every program in Neut is polarized into its positive fragment and its negative fragment, as done in [Call-by-Push-Value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf). From this perspective, every term in a source file of this language can be seen as neutral. So, just as `.txt` is used as the file extension for texts, I chose to use `.neut` (which is now `.nt`) as the file extension for neutral terms. Over time, it started to look like a proper noun, and I decided to adopt it as the name of the language.

## What are Possible Drawbacks of Neut's Approach?

The prominent one that come to my mind is that, when you use a polymorphic variant type, its type arguments must be stored in its values. For example, consider something like below:

```neut
// a syntax to define an ADT
variant Foo(a) {
- ConsA(i64, bool) // e.g. ConsA(3, True): Foo(a)
- ConsB(a)
}

// Haskell counterpart:
//   data Foo a
//     = ConsA Int64 Bool
//     | ConsB a
```

In an ordinary language, the internal representation of `ConsA(3, True)` will be something like:

```neut
// `0` is a tag to distinguish constructors
(0, 3, True)
```

However, in Neut, the type information must also be stored:

```neut
// `TYPE` is the address of the type (as function) `a`
(0, TYPE, 3, True)
```

This means that in Neut you must pay additional spaces to use parameterized variant types. So, it might be necessary to use non-parameterized variant types in performance critical situations, like:

```neut
variant FooText {
- ConsA(i64, bool) // e.g. ConsA(3, True): Foo(a)
- ConsB(text)
}
```

In that case, the internal representation will be `(0, 3, True)`. We'll have to be careful about what we have to pay for polymorphism.

## ... But What After All is This Thing?

I've always wanted something like this, but couldn't find one. As usual, according to the noble law of our majestic solar system, I had to make it exist by myself, spending quite a lot of time. Neut is the outcome of the process I had to go through.

—Well, yes, the above is true, but I feel like it doesn't quite capture the whole story. Let me retry.

To tell the truth, this language is actually a painting. A small painting, redrawn again and again, alone, for like 7 years or longer, seeking my own understanding of beauty™, that happened to take the form of a programming language. Of course, this isn't a heroic thing or whatever, but rather a symptom, if I name it. This painting is entirely dedicated to my conceited obsession. Still, I now believe that the resulting language has something sparkling in its concept, and also I don't have any reason to keep it secret in my atelier.

I'd be happy if you were inspired by skimming this book over this weekend for example, or even happier if you chose to try it on your PC. Such a chain of reactions is a little lucky and lovely accident, which I believe is the fundamental element that colors our world.

A lot of pieces of mirrors here, reflecting each other.

The painting is now a mirror.
