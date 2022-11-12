module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDefInfo,
    i64,
  )
where

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
import Entity.PrimNumSize
import qualified Entity.PrimNumType as PNT

type RawTerm = Cofree RawTermF Hint

data RawTermF a
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
  | Match (a, a) [(PrePatternF a, a)] -- (pattern-matched value, its type)

type DefInfo =
  ((Hint, T.Text), [BinderF RawTerm], RawTerm, RawTerm)

type TopDefInfo =
  ((Hint, BN.BaseName), [BinderF RawTerm], [BinderF RawTerm], RawTerm, RawTerm)

i64 :: Hint -> RawTerm
i64 m =
  m :< Prim (Prim.Type $ PNT.Int $ IntSize 64)
