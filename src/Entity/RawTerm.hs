module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDefInfo,
    TopDefHeader,
    LetKind (..),
    lam,
    piElim,
  )
where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Annotation qualified as Annot
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.BaseName qualified as BN
import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.HoleID
import Entity.IsExplicit (IsExplicit)
import Entity.Key
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
  | Pi C [RawBinder (a, C)] C C [RawBinder (a, C)] C C a
  | PiIntro (RawLamKind a) [RawBinder a] [RawBinder a] a
  | PiElim IsExplicit a [a]
  | PiElimByKey IsExplicit Name [(Hint, Key, a)] -- auxiliary syntax for key-call
  | PiElimExact a
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | DataIntro AttrDI.Attr DD.DefiniteDescription [a] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim N.IsNoetic [a] (RP.RawPatternMatrix a)
  | Noema a
  | Embody a
  | Let LetKind (Hint, RP.RawPattern, C, C, a) [(Hint, RawIdent)] a a
  | Prim (WP.WeakPrim a)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | Annotation RemarkLevel (Annot.Annotation ()) a
  | Resource DD.DefiniteDescription a a -- DD is only for printing
  | Use a [RawBinder a] a
  | If a a [(a, a)] a
  | When a a
  | Seq a a
  | ListIntro [a]
  | Admit
  | Detach a
  | Attach a
  | Option a
  | Assert (Hint, T.Text) a
  | Introspect T.Text [(Maybe T.Text, a)]
  | With a a

type DefInfo =
  ((Hint, T.Text), [RawBinder (RawTerm, C)], [RawBinder (RawTerm, C)], RawTerm, RawTerm)

type TopDefHeader =
  ((Hint, BN.BaseName), [RawBinder RawTerm], [RawBinder RawTerm], RawTerm)

type TopDefInfo =
  (TopDefHeader, RawTerm)

piElim :: a -> [a] -> RawTermF a
piElim =
  PiElim False

lam :: Hint -> [RawBinder RawTerm] -> RawTerm -> RawTerm
lam m varList e =
  m :< PiIntro Normal [] varList e

data LetKind
  = Plain
  | Noetic
  | Try
  | Bind
