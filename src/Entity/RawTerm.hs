module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDefInfo,
    i64,
  )
where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalLocator qualified as GL
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import Entity.LamKind
import Entity.LocalLocator qualified as LL
import Entity.Magic
import Entity.Mutability
import Entity.Noema qualified as N
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.RawPattern qualified as RP
import Entity.WeakPrim qualified as WP

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Ident
  | VarGlobal GL.GlobalLocator LL.LocalLocator
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Data DD.DefiniteDescription [a]
  | DataIntro DD.DefiniteDescription DD.DefiniteDescription D.Discriminant [a] [a]
  | DataElim N.IsNoetic [a] (RP.RawPatternMatrix a)
  | Noema a
  | Cell a
  | Let (BinderF a) [(Mutability, Hint, Ident)] a a -- let x on x1, ..., xn = e1 in e2 (with no context extension)
  | Prim (WP.WeakPrim a)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | New T.Text [(Hint, T.Text, RawTerm)] -- auxiliary syntax for codata introduction

type DefInfo =
  ((Hint, T.Text), [BinderF RawTerm], RawTerm, RawTerm)

type TopDefInfo =
  ((Hint, BN.BaseName), [BinderF RawTerm], [BinderF RawTerm], RawTerm, RawTerm)

i64 :: Hint -> RawTerm
i64 m =
  m :< Prim (WP.Type $ PT.Int $ IntSize 64)
