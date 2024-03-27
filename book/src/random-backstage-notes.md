# Random Backstage Notes

## How Did Neut Find the Approach?

By trying to utilize the other side of logical harmony in the sense of Michael Dummett.

From the viewpoint of natural deduction, executing a program is reducing detours in a proof tree, as is well known (the lovely Curry-Howard correspondence). Such a reduction of a proof tree can be seen as a utilization of local soundness.

On the other hand, the flip side of harmony, namely local completeness, is often ignored when thinking about programs. At least, that was how it seemed to me N years ago. It made me wonder: Why does local completeness seem to play almost no role in a program when local soundness handles the time aspect of a program, which is of great importance?

Led by the question, I tried to interpret local completeness as something related to the space aspect of a program. Then I noticed that the ability of local expansion, or local completeness, can be interpreted as our knowledge about the structure of the values of the type, which in turn means we can copy and discard values using their type. Here lies the idea of Neut.

A good lecture note on harmony can be found [here](https://www.cs.cmu.edu/~fp/courses/15317-f09/lectures/03-harmony.pdf).

## Why was it Named Neut?

During compilation, every program in Neut is polarized into its positive fragment and its negative fragment, as done in [Call-by-Push-Value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf). From this perspective, every term in a source file of this language can be seen as neutral. So, just as `.txt` is used as the file extension for texts, I chose to use `.neut` (which is now `.nt`) as the file extension for neutral terms. Over time, it started to look like a proper noun, and I decided to adopt it as the name of the language.

(I don't by any means claim this obviously opinionated language to be more neutral than others)

## Petals and a Gust

So you reached at the end of this book. Great. I'll tell you a private secret. Please don't spread!

Between you and me, but this language is actually a painting. A small painting, redrawn again and again, alone, for like eight years or longer, seeking my own understanding of beauty™, that happened to take the form of a programming language. This painting, or symptom, is entirely dedicated to my conceited obsession. Still, I now think that the resulting language has something sparkling in its concept, and I don't have any reason to keep it secret in my atelier.

I'd be happy if you were inspired by skimming this book over this weekend, for example, or even happier if you chose to try it on your PC. It might then react to your internal stuff, drawing a new axis in your perspective, and you might feel like writing a new program in the language. Such a chain of reactions is a lucky and lovely accident, which might be the fundamental element that colors our internal world.

Petals in my hands, waiting for your gust.
