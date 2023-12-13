module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDefInfo,
    TopDefHeader,
    LetKind (..),
    RawMagic (..),
    IfClause,
    EL,
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
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.HoleID
import Entity.Key
import Entity.LowType
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
  | PiElim a C [EL a]
  | PiElimByKey Name C C C [(Hint, Key, C, C, (a, C))] -- auxiliary syntax for key-call
  | PiElimExact C a
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | DataIntro AttrDI.Attr DD.DefiniteDescription [a] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim C N.IsNoetic [EL a] C (RP.RawPatternMatrix (a, C))
  | Noema a
  | Embody a
  | Let LetKind C (Hint, RP.RawPattern, C, C, (a, C)) [(C, ((Hint, RawIdent), C))] C a C a
  | Prim (WP.WeakPrim a)
  | Magic C RawMagic -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | Annotation RemarkLevel (Annot.Annotation ()) a
  | Resource DD.DefiniteDescription C (a, C) (a, C) -- DD is only for printing
  | Use C a C (Args a) C a
  | If (IfClause a) [IfClause a] C C (a, C)
  | When C (a, C) C (a, C)
  | Seq (a, C) C a
  | ListIntro [EL a]
  | Admit
  | Detach C C (a, C)
  | Attach C C (a, C)
  | Option C a
  | Assert C (Hint, T.Text) C C (a, C)
  | Introspect C T.Text C C [(C, (Maybe T.Text, C, (a, C)))]
  | With C a C C (a, C)
  | Brace C (a, C)

type IfClause a =
  (C, (a, C), C, (a, C), C)

type DefInfo a =
  ((Hint, RawIdent), C, Args a, Args a, C, (a, C), C, (a, C))

type TopDefHeader =
  ((Hint, BN.BaseName), [RawBinder RawTerm], [RawBinder RawTerm], RawTerm)

type TopDefInfo =
  (TopDefHeader, RawTerm)

piElim :: a -> [a] -> RawTermF a
piElim e es =
  PiElim e [] (map (\arg -> ([], (arg, []))) es)

lam :: Hint -> [RawBinder (RawTerm, C)] -> RawTerm -> RawTerm
lam m varList e =
  m :< PiIntro ([], []) (map ([],) varList, []) [] e

type Args a =
  ([(C, RawBinder (a, C))], C)

data LetKind
  = Plain
  | Noetic
  | Try
  | Bind

data RawMagic
  = Cast C (EL RawTerm) (EL RawTerm) (EL RawTerm)
  | Store C (EL LowType) (EL RawTerm) (EL RawTerm)
  | Load C (EL LowType) (EL RawTerm)
  | External C [LowType] LowType (EL EN.ExternalName) [EL RawTerm] [(C, ((RawTerm, C), (LowType, C)))]
  | Global C C (EN.ExternalName, C) C (LowType, C)

-- elem
type EL a =
  (C, (a, C))
