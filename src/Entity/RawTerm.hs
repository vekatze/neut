module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDefInfo,
  )
where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Annotation qualified as Annot
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Key
import Entity.Locator as L
import Entity.Magic
import Entity.Name
import Entity.Noema qualified as N
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawLamKind
import Entity.RawPattern qualified as RP
import Entity.Remark
import Entity.WeakPrim qualified as WP

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Name
  | Pi [RawBinder a] a
  | PiIntro (RawLamKind a) [RawBinder a] a
  | PiElim a [a]
  | PiElimByKey Name [a] [(Hint, Key, a)] -- auxiliary syntax for key-call
  | Data DD.DefiniteDescription [DD.DefiniteDescription] [a]
  | DataIntro
      DD.DefiniteDescription
      DD.DefiniteDescription
      [DD.DefiniteDescription]
      D.Discriminant
      [a]
      [a]
  | DataElim N.IsNoetic [a] (RP.RawPatternMatrix a)
  | Noema a
  | Embody a
  | Let (RawBinder a) [(Hint, RawIdent)] a a -- let x on x1, ..., xn = e1 in e2 (with no context extension)
  | Prim (WP.WeakPrim a)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole
  | Annotation RemarkLevel (Annot.Annotation ()) a
  | Flow
      L.Locator -- "core.thread.flow-inner"
      a -- inner type
  | FlowIntro
      L.Locator -- "core.thread.flow-inner"
      L.Locator -- "core.thread.detach"
      a -- lambda
  | FlowElim
      L.Locator -- "core.thread.flow-inner"
      L.Locator -- "core.thread.attach"
      a -- flow

type DefInfo =
  ((Hint, T.Text), [RawBinder RawTerm], RawTerm, RawTerm)

type TopDefInfo =
  ((Hint, BN.BaseName), [RawBinder RawTerm], RawTerm, RawTerm)
