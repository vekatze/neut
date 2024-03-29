# In a Linear Language

## Using a Variable Exactly Once

Consider a toy language in which every variable is used exactly once. Something like this:

```neut
let x = 1 in
let y = x + 2 in
let f =
  // lambda function
  (z) => {
    y + z // (A)
  }
in
f(4)
```

The variables `x`, `y`, `z`, and `f` are used exactly once. Such a use of a variable is called "linear".

In such a language, we just have to allocate memory when a value is defined and deallocate unnecessary parts after the value is used. This can be done because, in such a language, we know that a value won't be used ever again if once used. In a linear language, static memory management is almost trivial.

## Linearity and Expressiveness

As you can easily tell, however, a naive linear language is fairly restrictive. You can't even, for example, apply the same lambda function twice:

```neut
let increment = (x) => { x + 1 } in
increment(increment(3)) // error: `increment` is used twice
```

So we need a remedy here. The possible approaches are (A) staying inside linearity, or (B) going out of linearity.

The former way requires us to introduce some machinery for recovering expressiveness, like annotating the type system. Since one of the main aims of Neut is to realize the ordinary λ-calculus in a memory-predictable manner, this option can't be taken now. The resulting language must be something that allows every λ-term (which isn't because the latter way is objectively better or whatever, but simply because I find natural deduction lovely).

Neut thus takes the latter way. It starts from a non-linear language, our usual λ-calculus, and tries to recover the power of linearity, without annotating the type system.

## Luck of Neut

That might sound difficult. Luckily, however, Neut found a way inside the λ-calculus that translates a non-linear language into a linear one. The key is to execute types. *Using a type to copy/discard the terms of the type*.

If we can copy/discard values, we can adjust the use variables so that all of them are linear. Indeed, if a variable isn't used, we can insert `discard(x)`. If a variable is used more than twice, we can insert `copy(x)` as necessary.

After linearizing the language, we can enjoy the paradise of linearity; static memory management is almost trivial now.

In the next section, we'll see how types can be used to copy/discard their values.
