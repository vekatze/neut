module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDef,
    TopDefHeader,
    LetKind (..),
    RawMagic (..),
    IfClause,
    EL,
    Args,
    RawDecl (..),
    RawDef (..),
    VarArg,
    emptyArgs,
    extractArgs,
    lam,
    piElim,
    pushCommentToIfClause,
    extractFromIfClause,
    decodeLetKind,
    mapEL,
  )
where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Annotation qualified as Annot
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.HoleID
import Entity.IsConstLike
import Entity.Key
import Entity.LowType
import Entity.Name
import Entity.Noema qualified as N
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawPattern qualified as RP
import Entity.Remark
import Entity.Syntax.Series qualified as SE
import Entity.WeakPrim qualified as WP

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Name
  | Pi (Args a) (Args a) C a
  | PiIntro (Args a) (Args a) C a
  | PiIntroFix C DefInfo
  | PiElim a C (SE.Series a)
  | PiElimByKey Name C (SE.Series (Hint, Key, C, C, a)) -- auxiliary syntax for key-call
  | PiElimExact C a
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | DataIntro AttrDI.Attr DD.DefiniteDescription [a] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim C N.IsNoetic (SE.Series a) (SE.Series (RP.RawPatternRow a))
  | Noema a
  | Embody a
  | Let LetKind C (Hint, RP.RawPattern, C, C, a) C (SE.Series (Hint, RawIdent)) C a C C a
  | Prim (WP.WeakPrim a)
  | Magic C RawMagic -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | Annotation RemarkLevel (Annot.Annotation ()) a
  | Resource DD.DefiniteDescription C (a, C) (a, C) -- DD is only for printing
  | Use C a C (Args a) C a
  | If (IfClause a) [IfClause a] (EL a)
  | When (IfClause a)
  | Seq (a, C) C a
  | ListIntro (SE.Series a)
  | Admit
  | Detach C C (a, C)
  | Attach C C (a, C)
  | Option a
  | Assert C (Hint, T.Text) C C (a, C)
  | Introspect C T.Text C (SE.Series (Maybe (T.Text, C), C, a))
  | With C a C C (a, C)
  | Brace C (a, C)

type Args a =
  (SE.Series (RawBinder a), C)

emptyArgs :: Args a
emptyArgs =
  (SE.emptySeriesPC, [])

extractArgs :: Args a -> [RawBinder a]
extractArgs (series, _) =
  SE.extract series

type IfClause a =
  (EL a, EL a)

pushCommentToIfClause :: C -> IfClause a -> IfClause a
pushCommentToIfClause c ((c1, cond), (c2, body)) =
  ((c ++ c1, cond), (c2, body))

extractFromIfClause :: IfClause a -> (a, a)
extractFromIfClause ((_, (cond, _)), (_, (body, _))) =
  (cond, body)

data RawDecl a = RawDecl
  { loc :: Hint,
    name :: (a, C),
    isConstLike :: IsConstLike,
    impArgs :: Args RawTerm,
    expArgs :: Args RawTerm,
    cod :: (C, RawTerm)
  }

data RawDef a = RawDef
  { decl :: RawDecl a,
    leadingComment :: C,
    body :: RawTerm,
    trailingComment :: C
  }

type DefInfo =
  RawDef RawIdent

type TopDefHeader =
  RawDecl DD.DefiniteDescription

type TopDef =
  RawDef DD.DefiniteDescription

piElim :: a -> [a] -> RawTermF a
piElim e es =
  PiElim e [] (SE.fromList' es)

lam :: Hint -> [(RawBinder RawTerm, C)] -> RawTerm -> RawTerm
lam m varList e =
  m
    :< PiIntro
      (SE.emptySeries SE.Angle SE.Comma, [])
      (SE.assoc $ SE.fromList SE.Paren SE.Comma varList, [])
      []
      e

data LetKind
  = Plain
  | Noetic
  | Try
  | Bind

decodeLetKind :: LetKind -> T.Text
decodeLetKind letKind =
  case letKind of
    Plain -> "let"
    Noetic -> "tie"
    Try -> "try"
    Bind -> "bind"

type VarArg =
  (Hint, RawTerm, C, C, LowType)

data RawMagic
  = Cast C (EL RawTerm) (EL RawTerm) (EL RawTerm)
  | Store C (EL LowType) (EL RawTerm) (EL RawTerm)
  | Load C (EL LowType) (EL RawTerm)
  | External C EN.ExternalName C (SE.Series RawTerm) (Maybe (C, SE.Series VarArg))
  | Global C (EL EN.ExternalName) (EL LowType)

-- elem
type EL a =
  (C, (a, C))

mapEL :: (a -> b) -> EL a -> EL b
mapEL f (c1, (x, c2)) =
  (c1, (f x, c2))
