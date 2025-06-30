module Language.RawTerm.Rule.RawTerm
  ( RawTerm,
    RawTermF (..),
    DefInfo,
    TopDef,
    TopGeist,
    LetKind (..),
    PiElimKind (..),
    RawMagic (..),
    KeywordClause,
    EL,
    MustIgnoreRelayedVars,
    Args,
    RawGeist (..),
    RawDef (..),
    VarArg,
    getDefName,
    emptyArgs,
    emptyImpArgs,
    extractArgs,
    extractImpArgs,
    extractImpArgsWithDefaults,
    lam,
    piElim,
    pushCommentToKeywordClause,
    extractFromKeywordClause,
    decodeLetKind,
    mapEL,
    mapKeywordClause,
    letKindFromText,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Text qualified as T
import Language.Common.Rule.Annotation qualified as Annot
import Language.Common.Rule.Attr.Data qualified as AttrD
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.HoleID
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.Noema qualified as N
import Language.Common.Rule.Rune qualified as R
import Language.RawTerm.Rule.Key
import Language.RawTerm.Rule.Name
import Language.RawTerm.Rule.NecessityVariant (NecessityVariant)
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawIdent
import Language.RawTerm.Rule.RawPattern qualified as RP
import Logger.Rule.Hint
import Logger.Rule.LogLevel
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Tau
  | Var Name
  | Pi (SE.Series (RawBinder a, Maybe a), C) (Args a) C a Loc
  | PiIntro C FuncInfo
  | PiIntroFix C DefInfo
  | PiElim PiElimKind a C (SE.Series a)
  | PiElimByKey Name C (SE.Series (Hint, Key, C, C, a)) -- auxiliary syntax for key-call
  | PiElimExact C a
  | Data (AttrD.Attr DD.DefiniteDescription) DD.DefiniteDescription [a]
  | DataIntro (AttrDI.Attr DD.DefiniteDescription) DD.DefiniteDescription [a] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim C N.IsNoetic (SE.Series a) (SE.Series (RP.RawPatternRow a))
  | Box a
  | BoxNoema a
  | BoxIntro C C (SE.Series (Hint, RawIdent)) (a, C)
  | BoxIntroQuote C C (a, C)
  | BoxElim NecessityVariant Bool C (PatParam a) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc
  | Embody a
  | Let LetKind C (PatParam a) C C a C Loc C a Loc
  | LetOn LetKind C (PatParam a) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc
  | Pin C (RawBinder a) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc
  | StaticText a T.Text
  | Rune
  | RuneIntro a R.Rune
  | Magic C RawMagic -- (magic kind arg-1 ... arg-n)
  | Hole HoleID
  | Annotation LogLevel (Annot.Annotation ()) a
  | Resource DD.DefiniteDescription C (a, C) (a, C) (a, C) -- DD is only for printing
  | If (KeywordClause a) [KeywordClause a] (EL a)
  | When (KeywordClause a)
  | Seq (a, C) C a
  | SeqEnd a
  | ListIntro (SE.Series a)
  | Admit
  | Detach C C (a, C)
  | Attach C C (a, C)
  | Option a
  | Assert C (Hint, T.Text) C C (a, C)
  | Introspect C T.Text C (SE.Series (Maybe T.Text, C, a))
  | IncludeText C C Hint (T.Text, C)
  | Brace C (a, C)
  | Pointer
  | Void

type PatParam a =
  (Hint, RP.RawPattern, C, C, a)

type Args a =
  (SE.Series (RawBinder a), C)

emptyArgs :: Args a
emptyArgs =
  (SE.emptySeriesPC, [])

emptyImpArgs :: (SE.Series (RawBinder a, Maybe a), C)
emptyImpArgs =
  (SE.emptySeriesPC, [])

extractArgs :: Args a -> [RawBinder a]
extractArgs (series, _) =
  SE.extract series

extractImpArgs :: (SE.Series (RawBinder a, Maybe a), C) -> [RawBinder a]
extractImpArgs (series, _) =
  map fst $ SE.extract series

extractImpArgsWithDefaults :: (SE.Series (RawBinder a, Maybe a), C) -> [(RawBinder a, Maybe a)]
extractImpArgsWithDefaults (series, _) =
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
    impArgs :: (SE.Series (RawBinder RawTerm, Maybe RawTerm), C),
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
    trailingComment :: C,
    endLoc :: Loc
  }

instance Functor RawDef where
  fmap f def =
    def {geist = fmap f (geist def)}

type DefInfo =
  RawDef RawIdent

type FuncInfo =
  RawDef (Maybe T.Text)

type TopGeist =
  RawGeist BN.BaseName

type TopDef =
  RawDef BN.BaseName

getDefName :: RawDef a -> a
getDefName def =
  fst $ name $ geist def

piElim :: a -> [a] -> RawTermF a
piElim e es =
  PiElim Normal e [] (SE.fromList' es)

lam :: Loc -> Hint -> [(RawBinder RawTerm, C)] -> RawTerm -> RawTerm -> RawTerm
lam loc m varList codType e =
  m
    :< PiIntro
      []
      ( RawDef
          { geist =
              RawGeist
                { loc = m,
                  name = (Nothing, []),
                  isConstLike = False,
                  impArgs = (SE.emptySeries (Just SE.Angle) SE.Comma, []),
                  expArgs = (SE.assoc $ SE.fromList SE.Paren SE.Comma varList, []),
                  cod = ([], codType)
                },
            leadingComment = [],
            body = e,
            trailingComment = [],
            endLoc = loc
          }
      )

type MustIgnoreRelayedVars =
  Bool

data LetKind
  = Plain MustIgnoreRelayedVars
  | Noetic
  | Try

data PiElimKind
  = Normal
  | FoldRight

{-# INLINE decodeLetKind #-}
decodeLetKind :: LetKind -> T.Text
decodeLetKind letKind =
  case letKind of
    Plain _ -> "let"
    Noetic -> "tie"
    Try -> "try"

letKindFromText :: T.Text -> Maybe LetKind
letKindFromText t =
  case t of
    "let" ->
      return $ Plain False
    "tie" ->
      return Noetic
    "try" ->
      return Try
    _ ->
      Nothing

type VarArg =
  (Hint, RawTerm, C, C, RawTerm)

data RawMagic
  = Cast C (EL RawTerm) (EL RawTerm) (EL RawTerm) (Maybe C)
  | Store C (EL RawTerm) (EL RawTerm) (EL RawTerm) (Maybe C)
  | Load C (EL RawTerm) (EL RawTerm) (Maybe C)
  | Alloca C (EL RawTerm) (EL RawTerm) (Maybe C)
  | External C Hint EN.ExternalName C (SE.Series RawTerm) (Maybe (C, SE.Series VarArg))
  | Global C (EL EN.ExternalName) (EL RawTerm) (Maybe C)
  | OpaqueValue C (EL RawTerm)
  | CallType C (EL RawTerm) (EL RawTerm) (EL RawTerm)

-- elem
type EL a =
  (C, (a, C))

mapEL :: (a -> b) -> EL a -> EL b
mapEL f (c1, (x, c2)) =
  (c1, (f x, c2))
