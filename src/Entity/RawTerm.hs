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
import Entity.Key
import Entity.Magic
import Entity.Name
import Entity.Noema qualified as N
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawPattern qualified as RP
import Entity.Remark
import Entity.WeakPrim qualified as WP

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Name
  | Pi (Args a) (Args a) C a
  | PiIntro (Args a) (Args a) C a
  | PiIntroFix C (DefInfo a)
  | PiElim a C [(a, C)]
  | PiElimByKey Name C C C [(Hint, Key, C, C, (a, C))] -- auxiliary syntax for key-call
  | PiElimExact C a
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | DataIntro AttrDI.Attr DD.DefiniteDescription [a] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim C N.IsNoetic [(a, C)] C (RP.RawPatternMatrix (a, C))
  | Noema a
  | Embody a
  | Let LetKind (Hint, RP.RawPattern, C, C, (a, C)) [(Hint, RawIdent)] a a
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

type DefInfo a =
  ((Hint, RawIdent), C, Args a, Args a, C, (a, C), a)

type TopDefHeader =
  ((Hint, BN.BaseName), [RawBinder RawTerm], [RawBinder RawTerm], RawTerm)

type TopDefInfo =
  (TopDefHeader, RawTerm)

piElim :: a -> [a] -> RawTermF a
piElim e es =
  PiElim e [] (map (,[]) es)

lam :: Hint -> [RawBinder (RawTerm, C)] -> RawTerm -> RawTerm
lam m varList e =
  m :< PiIntro ([], ([], [])) ([], (varList, [])) [] e

type Args a =
  CSeq (RawBinder (a, C))

type CSeq a =
  (C, ([a], C))

data LetKind
  = Plain
  | Noetic
  | Try
  | Bind
