# Language Reference: Terms

## Table of Contents

Lorem ipsum.

## The Type of Types

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
  let theSpreadingWideMyNarrowHandsToGatherParadise = αβγ in
  let _h-e-l-l-o = buz' in
  let αβγ = _h-e-l-l-o in
  let 冥きより冥き道にぞ入りぬべきはるかに照らせ山の端の月 = Unit in
  let _ = Unit in

  // shadowing (not reassignment)
  let x = Unit in
  let x = tau in
  let f = function (x: bool) { x } in

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

stub

A token can be the name of a top-level variable if it doesn't contain any of `=() "\n\t:;,<>[]{}/*`.

The name of a variable is interpreted as that of a top-level variable if the name is defined in a local scope (using `let` for example).

If a name is defined in a local scope (using `let` for example), the name is interpreted as that of a local variable. If the name is defined in the top-level scope (using `define` for example), the name is interpreted as that of a top-level variable.

### Semantics

A top-level variable `f` is lowered to the following 3-word tuple:

```
(base.#.imm, 0, POINTER_TO_FUNCTION(f))
```

See the Note below for more detailed explanation.

### Inference Rule

```neut
(Γ is a context)     (c: a is defined at the top-level)
-------------------------------------------------------
                  Γ ⊢ c: a
```

### Note

Let's see how top-level variables are lowered. Consider the following top-level functions:

```neut
// (source-dir)/sample.nt

// defining a top-level variable `increment`
define increment(x: int): int {
  add-int(x, 1)
}

define get-increment(): (int) -> int {
  increment // using a top-level variable `increment`
}
```

This `increment` and `get-increment` are lowered to LLVM functions like the below:

```llvm
; (build-dir)/path/to/sample.ll

define fastcc ptr @"this.sample.increment"(ptr %_1) {
  %_2 = ptrtoint ptr %_1 to i64
  %_3 = add i64 %_2, 1
  %_4 = inttoptr i64 %_3 to ptr
  ret ptr %_4
}

define fastcc ptr @"this.sample.get-increment"() {
  ; `increment` in `get-increment` is lowered to the following code:

  ; calculate the size of 3-word tuples
  %_1 = getelementptr ptr, ptr null, i32 3
  %_2 = ptrtoint ptr %_1 to i64
  ; allocate memory
  %_3 = call fastcc ptr @malloc(i64 %_2)
  ; store contents
  %_4 = getelementptr [3 x ptr], ptr %_3, i32 0, i32 0
  %_5 = getelementptr [3 x ptr], ptr %_3, i32 0, i32 1
  %_6 = getelementptr [3 x ptr], ptr %_3, i32 0, i32 2
  store ptr @"base.#.imm", ptr %_4            ; tuple[0] = `base.#.imm`
  store ptr null, ptr %_5                     ; tuple[1] = null
  store ptr @"this.sample.increment", ptr %_6 ; tuple[2] = (function pointer)
  ; return the pointer to the tuple
  ret ptr %_3
}
```

Incidentally, 3-word tuples of global variables are usually reduced at compile time.

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
