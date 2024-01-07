module Entity.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDef,
    TopGeist,
    LetKind (..),
    RawMagic (..),
    KeywordClause,
    EL,
    Args,
    RawGeist (..),
    RawDef (..),
    VarArg,
    emptyArgs,
    extractArgs,
    lam,
    piElim,
    pushCommentToKeywordClause,
    extractFromKeywordClause,
    decodeLetKind,
    mapEL,
    mapKeywordClause,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Text qualified as T
import Entity.Annotation qualified as Annot
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.BaseName qualified as BN
import Entity.C
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.HoleID
import Entity.IsConstLike
import Entity.Key
import Entity.Name
import Entity.Noema qualified as N
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawLowType qualified as RLT
import Entity.RawPattern qualified as RP
import Entity.Remark
import Entity.Syntax.Series qualified as SE

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Name
  | Pi (Args a) (Args a) C a Loc
  | PiIntro (Args a) (Args a) C (EL a)
  | PiIntroFix C DefInfo
  | PiElim a C (SE.Series a)
  | PiElimByKey Name C (SE.Series (Hint, Key, C, C, a)) -- auxiliary syntax for key-call
  | PiElimExact C a
  | Data (AttrD.Attr BN.BaseName) BN.BaseName [a]
  | DataIntro (AttrDI.Attr BN.BaseName) BN.BaseName [a] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim C N.IsNoetic (SE.Series a) (SE.Series (RP.RawPatternRow a))
  | Noema a
  | Embody a
  | Let LetKind C (Hint, RP.RawPattern, C, C, a) C (SE.Series (Hint, RawIdent)) C a C C a
  | StaticText a T.Text
  | Magic C RawMagic -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | Annotation RemarkLevel (Annot.Annotation ()) a
  | Resource C (a, C) (a, C) -- DD is only for printing
  | Use C a C (Args a) C a
  | If (KeywordClause a) [KeywordClause a] (EL a)
  | When (KeywordClause a)
  | Seq (a, C) C a
  | ListIntro (SE.Series a)
  | Admit
  | Detach C C (a, C)
  | Attach C C (a, C)
  | Option a
  | Assert C (Hint, T.Text) C C (a, C)
  | Introspect C T.Text C (SE.Series (Maybe T.Text, C, a))
  | With (KeywordClause a)
  | Brace C (a, C)

type Args a =
  (SE.Series (RawBinder a), C)

emptyArgs :: Args a
emptyArgs =
  (SE.emptySeriesPC, [])

extractArgs :: Args a -> [RawBinder a]
extractArgs (series, _) =
  SE.extract series

type KeywordClause a =
  (EL a, EL a)

mapKeywordClause :: (a -> b) -> KeywordClause a -> KeywordClause b
mapKeywordClause f (el1, el2) =
  (mapEL f el1, mapEL f el2)

pushCommentToKeywordClause :: C -> KeywordClause a -> KeywordClause a
pushCommentToKeywordClause c ((c1, cond), (c2, body)) =
  ((c ++ c1, cond), (c2, body))

extractFromKeywordClause :: KeywordClause a -> (a, a)
extractFromKeywordClause ((_, (cond, _)), (_, (body, _))) =
  (cond, body)

data RawGeist a = RawGeist
  { loc :: Hint,
    name :: (a, C),
    isConstLike :: IsConstLike,
    impArgs :: Args RawTerm,
    expArgs :: Args RawTerm,
    cod :: (C, RawTerm)
  }

instance Functor RawGeist where
  fmap f geist =
    geist {name = first f (name geist)}

data RawDef a = RawDef
  { geist :: RawGeist a,
    leadingComment :: C,
    body :: RawTerm,
    trailingComment :: C
  }

instance Functor RawDef where
  fmap f def =
    def {geist = fmap f (geist def)}

type DefInfo =
  RawDef RawIdent

type TopGeist =
  RawGeist BN.BaseName

type TopDef =
  RawDef BN.BaseName

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
      ([], (e, []))

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
  (Hint, RawTerm, C, C, RLT.RawLowType)

data RawMagic
  = Cast C (EL RawTerm) (EL RawTerm) (EL RawTerm)
  | Store C (EL RLT.RawLowType) (EL RawTerm) (EL RawTerm)
  | Load C (EL RLT.RawLowType) (EL RawTerm)
  | External C EN.ExternalName C (SE.Series RawTerm) (Maybe (C, SE.Series VarArg))
  | Global C (EL EN.ExternalName) (EL RLT.RawLowType)

-- elem
type EL a =
  (C, (a, C))

mapEL :: (a -> b) -> EL a -> EL b
mapEL f (c1, (x, c2)) =
  (c1, (f x, c2))
