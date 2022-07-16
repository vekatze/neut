module Entity.PreTerm where

import Control.Comonad.Cofree
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.EnumTypeName
import qualified Entity.GlobalLocator as GL
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import Entity.LamKind
import qualified Entity.LocalLocator as LL
import Entity.Magic
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNum
import Entity.PrimNumSize

type PreTerm = Cofree PreTermF Hint

data PreTermF a
  = Tau
  | Var Ident
  | VarGlobal GL.GlobalLocator LL.LocalLocator
  | VarGlobalStrict DD.DefiniteDescription
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Sigma [BinderF a]
  | SigmaIntro [a]
  | SigmaElim [BinderF a] a a
  | Let (BinderF a) a a -- let x = e1 in e2 (with no context extension)
  | Aster HoleID
  | Prim Prim.Prim
  | Int a Integer
  | Float a Double
  | Enum EnumTypeName
  | EnumIntro PreEnumLabel
  | EnumElim (a, a) [(PreEnumCase, a)]
  | Question a a -- e : t (output the type `t` as note)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Match
      (Maybe a) -- noetic subject (this is for `case-noetic`)
      (a, a) -- (pattern-matched value, its type)
      [(PrePatternF a, a)]
  | Noema a a
  | NoemaIntro Ident a
  | NoemaElim Ident a
  | Array a
  | ArrayIntro a [a]
  | ArrayAccess a a a a
  | Text
  | TextIntro T.Text
  | Cell a -- cell(list(i64))
  | CellIntro a a -- cell-new(v) (the first argument is the type of `v`)
  | CellRead a -- cell-read(ptr)
  | CellWrite a a -- cell-write(ptr, value)

type DefInfo =
  ((Hint, T.Text), [BinderF PreTerm], PreTerm, PreTerm)

type TopDefInfo =
  ((Hint, BN.BaseName), [BinderF PreTerm], [BinderF PreTerm], PreTerm, PreTerm)

i8 :: Hint -> PreTerm
i8 m =
  m :< Prim (Prim.Type $ PrimNumInt $ IntSize 8)

i64 :: Hint -> PreTerm
i64 m =
  m :< Prim (Prim.Type $ PrimNumInt $ IntSize 64)
