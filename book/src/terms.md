# Language Reference: Terms

## The type of types

### Example

```neut
define sample(): unit {
  // `tau` used as a term
  let foo = tau in
  Unit
}

// `tau` used as a type
define identity(a: tau, x: a): a {
  x
}
```

### Syntax

```neut
tau
```

### Semantics

`tau` is lowered to a pointer to `base.#.imm`. Thus `tau` is lowered to an immediate.

### Inference Rule

```neut
(Γ is a context)
----------------
  Γ ⊢ tau: tau
```

### Notes

- For any `x: tau`, `x` can be copied/discarded for free since `x` is lowered to an immediate.
- `tau` is used in combination with the inference rule of variables.

## Local Variables

### Example

```neut
define sample(): unit {
  // defining/using various local variables
  let x = Unit in
  let foo = x in
  let 'bar = foo in
  let buz' = 'bar in
  let _h-e-l-l-o = buz' in
  let αβγ = _h-e-l-l-o in
  let theSpreadingWideMyNarrowHandsToGatherParadise = αβγ in
  let 冥きより冥き道にぞ入りぬべきはるかに照らせ山の端の月 = Unit in
  Unit
}
```

### Syntax

A token can be the name of a variable if it doesn't contain any of `=() "\n\t:;,<>[]{}/*`.

The name of a variable is interpreted as that of a local variable if the name is defined in a local scope (using `let` for example).

### Semantics

If the content of a variable `x` is an immediate, `x` is lowered to a name of a register which stores the immediate. Otherwise, `x` is lowered to a pointer to the content.

If a variable `x: a` is used n times, the content of `x` is copied along `a` for `n-1` times. If a variable `x: a` isn't used, the content is discarded. This copying/discarding happens immediately after definition.

### Inference Rule

```neut
  Γ ⊢ a: tau
----------------
Γ, x: a ⊢ x: a
```

### Notes

- Variables in Neut are immutable. You'll need `core.cell` to achieve mutability.
- The compiler detects and reports unused variables. You can use the name `_` to suppress it.

## Top-Level Variables

### Example

```neut
import {
- core.bool {bool}
- B
}

define sample(): unit {
  // using top-level variables
  let _ = bool // using an imported top-level name
  let _ = core.bool.bool // using the definite description of `core.bool.bool`
  let _ = B.bool // using a prefixed top-level name
  Unit
}
```

### Syntax

If a name is defined in a local scope (using `let` for example), the name is interpreted as that of a local variable. If the name is defined in the top-level scope (using `define` for example), the name is interpreted as that of a top-level variable.

## Literals

### Example

```neut
define foo(var: int): unit {
  let _: int = 100 in
  //           ^^^
  let _: float = 3.8 in
  //             ^^^
  let _: &text = "test" in
  //             ^^^^^^
  Unit
}

```

## The function type

### Examples

```neut
// a function that accepts ints and returns bools
(value: int) -> bool

// this is equivalent to `(_: int) -> bool`:
(int) -> bool

// use `a`
(a: tau, x: a) -> a

// make the first argument implicit
<a: tau>(x: a) -> a

// this is equivalent to `<a: _>(x: a) -> a`
<a>(x: a) -> a
```

### Syntax

```neut
<x1: a1, ..., xn: an>(y1: b1, ..., ym: bm) -> c
```

The following abbreviations are available:

```neut
(y1: b1, ..., ym: bm) -> c
↓
<>(y1: b1, ..., ym: bm) -> c


(b1, ..., bm) -> c
↓
(_: b1, ..., _: bm) -> c


<a1, ..., an>(y1: b1, ..., ym: bm) -> c
↓
<a1: _, ..., an: _>(y1: b1, ..., ym: bm) -> c
```

## `function (x1: a1, ..., xn: an) { e }`

## Function Elimination

## `let`

## `try`

## `tie`

## ADT Formation

## ADT Introduction

## ADT Elimination

## Noema Formation: `&a`

## `on` (Noema Introduction)

## `*e` - Noema Elimination

## `magic`

## `introspect`

## `_`

## `use`

## `if`

## `when cond { e }`

## `e1; e2`

## `admit`

## `detach`

## `attach`

## `assert`

## `with` / `bind`

## `e::x`

## `{ e }`

  <!-- = Tau -->
  <!-- | Var Name -->
  <!-- | Pi (Args a) (Args a) C a Loc -->
  <!-- | PiIntro (Args a) (Args a) C (Block a) Loc -->
  <!-- | PiIntroFix C DefInfo -->
  <!-- | PiElim a C (SE.Series a) -->
  <!-- | PiElimByKey Name C (SE.Series (Hint, Key, C, C, a)) -- auxiliary syntax for key-call -->
  <!-- | PiElimExact C a -->
  <!-- | Data (AttrD.Attr BN.BaseName) BN.BaseName [a] -->
  <!-- | DataIntro (AttrDI.Attr BN.BaseName) BN.BaseName [a] [a] -- (attr, consName, dataArgs, consArgs) -->
  <!-- | DataElim C N.IsNoetic (SE.Series a) (SE.Series (RP.RawPatternRow a)) -->
  <!-- | Noema a -->
  <!-- | Embody a -->
  <!-- | Let LetKind C (Hint, RP.RawPattern, C, C, a) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc -->
  <!-- | StaticText a T.Text -->
  <!-- | Magic C RawMagic -- (magic kind arg-1 ... arg-n) -->
  <!-- | Hole HoleID -->
  <!-- | Annotation RemarkLevel (Annot.Annotation ()) a -->
  <!-- | Resource C (a, C) (a, C) -- DD is only for printing -->
  <!-- | Use C a C (Args a) C a Loc -->
  <!-- | If (KeywordClause a) [KeywordClause a] (EL a) -->
  <!-- | When (KeywordClause a) -->
  <!-- | Seq (a, C) C a -->
  <!-- | ListIntro (SE.Series a) -->
  <!-- | Admit -->
  <!-- | Detach C C (a, C) -->
  <!-- | Attach C C (a, C) -->
  <!-- | Option a -->
  <!-- | Assert C (Hint, T.Text) C C (a, C) -->
  <!-- | Introspect C T.Text C (SE.Series (Maybe T.Text, C, a)) -->
  <!-- | With (KeywordClause a) -->
  <!-- | Projection a (Hint, RawIdent) Loc -->
  <!-- | Brace C (a, C) -->
