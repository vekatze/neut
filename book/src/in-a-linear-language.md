# In a Linear Language

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

The variables `x`, `y`, `z`, and `f` are used exactly once. Such a use of a variable is called "linear".

In such a language, we just have to allocate memory when a value is introduced, and deallocate unnecessary part after the value is used. This can be done because, in such a language, we know that a value won't be used ever again if once used. In a linear language, static memory management is almost trivial.

## Going Out of Linearity

As you can easily tell, however, a naive linear language is fairly restrictive. You can't even, for example, apply the same lambda function twice:

```neut
let increment = lambda (x) {x + 1}
increment(increment(3)) // error: `increment` is used twice
```

So we need a remedy here. The possible approaches are

- stay inside linearity, or
- go out of linearity.

The former way requires us to introduce some machineries for recovering expressiveness. Since Neut's main aim is to realize the ordinary λ-calculus in a static manner, this option can't be taken now. The resulting language must be something that allows every λ-term.

Neut takes the latter way. It starts from a non-linear language, our usual λ-calculus, and tries to recover the power of linearity, without annotating the type system.

## To Gather Paradise

Luckily, Neut found a linearizer inside the λ-calculus. Found a way to translate a non-linear language into a linear one. The key is to execute types. *Using a type to copy/discard the terms of the type*.

If we can copy/discard values, we can adjust the use variables so that all of them are linear. Indeed, if a variable isn't used, we can insert `discard(x)`. If a variable is used more that twice, we can insert `copy(x)` as necessary.

After linearizing the language, we can enjoy the paradise of linearity; static memory management is almost trivial now.

In the next section, we'll see how types can be used to copy/discard their values.
