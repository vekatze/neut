# Paradise of Linearity

## Using a Variable Exactly Once

Consider a toy language in which every variable is used exactly once. Something like this:

```neut
let x = 1
let y = x + 2
let f =
  lambda (z) {
    y + z // (A)
  }
f(4)
```

Note that every variable (`x`, `y`, `z`, and `f`) is used exactly once. Such a use of a variable is called "linear".

If we were to write, for example, `y + x` at `(A)`, it should result in an error, because `x` is used twice in that case. `y + 3` also should result in an error, because `z` isn't used in that case.

In such a language, we just have to allocate memory when a value is introduced, and deallocate it after the value is used. This is because, in such a language, we know that a value won't be used ever again if once used.

## Example: Compiling a Closure

Let's see how a term is compiled in a linear language. In a linear language, we would compile a function

```neut
lambda (x) { e }
```

into something like this:

```neut
let ptr = malloc({2-words})
store(ptr[0], FREE_VARIABLES(e))
store(ptr[1], CLOSURE_LABEL)
ptr // = (pointer-to-free-variables, pointer-to-function-label)
```

Also, we would compile a function call

```neut
f(x)
```

into something like below:

```neut
let pointer-to-free-variables = f[0]
let pointer-to-closure-label = f[1]
free(f) // deallocates the outer tuple of `f`
pointer-to-closure-label(x, pointer-to-free-variables)
```

The point here is the existence of `free(f)`. This deallocation is allowed thanks to the linearity of the language. Without linearity, it might be possible that a closure `f` is used after `f(x)`.

In short, we can say a value is "consumed" when a value is used in a linear language. Static memory management in such a language is trivial; We just have to deallocate memory region when a value is consumed.

## Paradise Broken Into Pieces

As you can easily tell, however, a naive linear language is fairly restrictive. You can't even, for example, apply the same lambda function twice:

```neut
let increment = lambda (x) {x + 1}
increment(increment(3)) // error: `increment` is used twice
```

So we need a remedy here. The possible approach can be

- Stay inside the linear language
  - Introduce some machinery to recover expressiveness
- Go out of the linear language
  - Find a way to translate a non-linear program into a linear one

Neut's main aim is to realize ordinary lambda calculus in a static manner, so the first option cannot be taken. The resulting language must be something that allows every innocent lambda term, like:

```neut
let increment = lambda (x) {x + 1}
increment(increment(3)) // ← this must be allowed
```

and this must be achieved without annotation to the language. Every lambda term must be, say, intact. As long as Neut sticks to its original aim, the resulting language's terms in themselves shouldn't have to care about mortal human's convenience, von Neumann architecture, or something like that.

That is to say, we humans must find a linearizer *inside* the λ-calculus, instead of annotating additional notes to the language for our convenience. —And still, the resulting language must also have a good predictable behavior in cooperation with our beloved von Neumann architecture. As a mere mortal, we want a great kawaii programming language that runs fast, after all.

Yes, a picky child's request...

## To Gather Paradise

And here comes the the interesting point. Neut actually found such a linearizer inside the λ-calculus. The key is executing types. *Using a type to copy/discard the terms of the type*.

If we can copy/discard values freely, we can adjust the use variables so that all of them are linear. Indeed, if a variable isn't used, we can insert `discard(x)`. If a variable is used more that twice, we can insert `copy(x)` as necessary.

After this translation, the storyline of a linear language can be directly applied; Just `free` consumed pointers after using it, as we've done when talking about closures.

This is the end of the prologue. Insert your favorite opening movie here.

And the main story begins.
