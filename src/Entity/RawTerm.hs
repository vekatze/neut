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
import qualified Entity.Discriminant as D
import qualified Entity.GlobalLocator as GL
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import Entity.LamKind
import qualified Entity.LocalLocator as LL
import Entity.Magic
import Entity.PrimNumSize
import qualified Entity.PrimType as PT
import qualified Entity.RawPattern as RP
import qualified Entity.WeakPrim as WP

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
  | DataElim [a] (RP.RawPatternMatrix a)
  | Sigma [BinderF a]
  | SigmaIntro [a]
  | SigmaElim [BinderF a] a a
  | Let (BinderF a) a a -- let x = e1 in e2 (with no context extension)
  | Aster HoleID
  | Prim (WP.WeakPrim a)
  | Question a a -- e : t (output the type `t` as note)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)

type DefInfo =
  ((Hint, T.Text), [BinderF RawTerm], RawTerm, RawTerm)

type TopDefInfo =
  ((Hint, BN.BaseName), [BinderF RawTerm], [BinderF RawTerm], RawTerm, RawTerm)

i64 :: Hint -> RawTerm
i64 m =
  m :< Prim (WP.Type $ PT.Int $ IntSize 64)
