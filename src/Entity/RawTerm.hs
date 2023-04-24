module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDefInfo,
  )
where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Annotation qualified as AN
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
import Entity.RawPattern qualified as RP
import Entity.Remark
import Entity.WeakPrim qualified as WP

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Ident
  | VarGlobal GL.GlobalLocator LL.LocalLocator
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Data DD.DefiniteDescription [DD.DefiniteDescription] [a]
  | DataIntro DD.DefiniteDescription DD.DefiniteDescription [DD.DefiniteDescription] D.Discriminant [a] [a]
  | DataElim N.IsNoetic [a] (RP.RawPatternMatrix a)
  | Noema a
  | Embody a
  | Cell a
  | Let (BinderF a) [(Mutability, Hint, Ident)] a a -- let x on x1, ..., xn = e1 in e2 (with no context extension)
  | Prim (WP.WeakPrim a)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | New T.Text [(Hint, T.Text, RawTerm)] -- auxiliary syntax for codata introduction
  | Annotation RemarkLevel (AN.Annotation ()) a
  | Flow
      (GL.GlobalLocator, LL.LocalLocator) -- "core.thread.flow-inner"
      a -- actual-argument
  | FlowIntro
      (GL.GlobalLocator, LL.LocalLocator) -- "core.thread.flow-inner"
      (GL.GlobalLocator, LL.LocalLocator) -- "core.thread.run"
      a -- lambda
  | FlowElim
      (GL.GlobalLocator, LL.LocalLocator) -- "core.thread.flow-inner"
      (GL.GlobalLocator, LL.LocalLocator) -- , "core.thread.wait"
      a -- flow

type DefInfo =
  ((Hint, T.Text), [BinderF RawTerm], RawTerm, RawTerm)

type TopDefInfo =
  ((Hint, BN.BaseName), [BinderF RawTerm], [BinderF RawTerm], RawTerm, RawTerm)
