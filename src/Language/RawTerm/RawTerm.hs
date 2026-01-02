module Language.RawTerm.RawTerm
  ( RawTerm,
    RawTermF (..),
    RawType,
    RawTypeF (..),
    DefInfo,
    TopDef,
    TopGeist,
    LetKind (..),
    RawMagic (..),
    KeywordClause,
    EL,
    MustIgnoreRelayedVars,
    Args,
    RawGeist (..),
    RawDef (..),
    RawTypeDef (..),
    RawImpVar,
    VarArg,
    getDefName,
    emptyArgs,
    emptyImpArgs,
    emptyDefaultArgs,
    extractArgs,
    extractImpArgs,
    lam,
    piElim,
    pushCommentToKeywordClause,
    extractFromKeywordClause,
    decodeLetKind,
    mapEL,
    mapKeywordClause,
    letKindFromText,
    force,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Text qualified as T
import Kernel.Common.GlobalName qualified as GN
import Language.Common.Annotation qualified as Annot
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.HoleID
import Language.Common.IsConstLike
import Language.Common.Noema qualified as N
import Language.Common.Opacity qualified as O
import Language.Common.Rune qualified as R
import Language.RawTerm.Key
import Language.RawTerm.Name
import Language.RawTerm.NecessityVariant (NecessityVariant)
import Language.RawTerm.RawBinder
import Language.RawTerm.RawIdent
import Language.RawTerm.RawPattern qualified as RP
import Logger.Hint
import Logger.LogLevel
import SyntaxTree.C
import SyntaxTree.Series qualified as SE

type RawImpVar =
  (Hint, RawIdent, C)

data RawTypeF a
  = Tau
  | TypeHole HoleID
  | TyVar RawIdent
  | TyApp a C (SE.Series a)
  | Pi (SE.Series RawImpVar, C) (SE.Series (RawBinder a, RawTerm), C) (Args a) C a Loc
  | Data (AttrD.Attr DD.DefiniteDescription (RawBinder a)) DD.DefiniteDescription [a]
  | Box a
  | BoxNoema a
  | Code a
  | Rune
  | Pointer
  | Void
  | Resource DD.DefiniteDescription C (RawTerm, C) (RawTerm, C) (RawTerm, C) -- DD is only for printing
  | Option a
  | TyIntrospect C T.Text C (SE.Series (Maybe T.Text, C, a))

type RawType = Cofree RawTypeF Hint

type RawTerm = Cofree RawTermF Hint

data RawTermF a
  = Var Name
  | VarGlobal DD.DefiniteDescription GN.GlobalName
  | PiIntro C FuncInfo
  | PiIntroFix O.Opacity C DefInfo
  | PiElim a C (Maybe (SE.Series RawType)) C (SE.Series a)
  | PiElimByKey Name C (SE.Series (Hint, Key, C, C, a)) -- auxiliary syntax for key-call
  | PiElimRule Name C (SE.Series a)
  | PiElimExact C a
  | DataIntro (AttrDI.Attr DD.DefiniteDescription (RawBinder RawType)) DD.DefiniteDescription [RawType] [a] -- (attr, consName, dataArgs, consArgs)
  | DataElim C N.IsNoetic (SE.Series a) (SE.Series (RP.RawPatternRow a))
  | BoxIntro C C (SE.Series (Hint, RawIdent)) (a, C)
  | BoxIntroLift C C (a, C)
  | BoxElim NecessityVariant Bool C (PatParam RawType) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc
  | CodeIntro C C (a, C)
  | CodeElim C C (a, C)
  | Embody a
  | Let LetKind C (PatParam RawType) C C a C Loc C a Loc
  | LetOn LetKind C (PatParam RawType) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc
  | Pin C (RawBinder RawType) C (SE.Series (Hint, RawIdent)) C a C Loc C a Loc
  | StaticText RawType T.Text
  | RuneIntro a R.Rune
  | Magic C RawMagic -- (magic kind arg-1 ... arg-n)
  | Annotation LogLevel (Annot.Annotation ()) a
  | If (KeywordClause a) [KeywordClause a] (EL a)
  | When (KeywordClause a)
  | Seq (a, C) C a
  | SeqEnd a
  | Admit
  | Detach C C (a, C)
  | Attach C C (a, C)
  | Assert C (Hint, T.Text) C C (a, C)
  | Introspect C T.Text C (SE.Series (Maybe T.Text, C, a))
  | IncludeText C C Hint (T.Text, C)
  | Brace C (a, C)
  | Int Integer

type PatParam a =
  (Hint, RP.RawPattern, C, C, a)

type Args a =
  (SE.Series (RawBinder a), C)

emptyArgs :: Args a
emptyArgs =
  (SE.emptySeriesPC, [])

emptyImpArgs :: (SE.Series RawImpVar, C)
emptyImpArgs =
  (SE.emptySeries (Just SE.Angle) SE.Comma, [])

emptyDefaultArgs :: (SE.Series (RawBinder RawType, RawTerm), C)
emptyDefaultArgs =
  (SE.emptySeries (Just SE.Bracket) SE.Comma, [])

extractArgs :: Args a -> [RawBinder a]
extractArgs (series, _) =
  SE.extract series

extractImpArgs :: (SE.Series RawImpVar, C) -> [RawImpVar]
extractImpArgs (series, _) =
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
    impArgs :: (SE.Series RawImpVar, C),
    defaultArgs :: (SE.Series (RawBinder RawType, RawTerm), C),
    expArgs :: Args RawType,
    cod :: (C, RawType)
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

data RawTypeDef a = RawTypeDef
  { typeGeist :: RawGeist a,
    typeLeadingComment :: C,
    typeBody :: RawType,
    typeTrailingComment :: C,
    typeEndLoc :: Loc
  }

instance Functor RawTypeDef where
  fmap f def =
    def {typeGeist = fmap f (typeGeist def)}

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

force :: RawTerm -> RawTerm
force e@(m :< _) =
  m :< piElim e []

piElim :: a -> [a] -> RawTermF a
piElim e es =
  PiElim e [] Nothing [] (SE.fromList' es)

lam :: Loc -> Hint -> [(RawBinder RawType, C)] -> RawType -> RawTerm -> RawTerm
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
                  defaultArgs = (SE.emptySeries (Just SE.Bracket) SE.Comma, []),
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
  (Hint, RawTerm, C, C, RawType)

data RawMagic
  = Cast C (EL RawType) (EL RawType) (EL RawTerm) (Maybe C)
  | Store C (EL RawType) (EL RawTerm) (EL RawTerm) (Maybe C)
  | Load C (EL RawType) (EL RawTerm) (Maybe C)
  | Alloca C (EL RawType) (EL RawTerm) (Maybe C)
  | External C Hint EN.ExternalName C (SE.Series RawTerm) (Maybe (C, SE.Series VarArg))
  | Global C (EL EN.ExternalName) (EL RawType) (Maybe C)
  | OpaqueValue C (EL RawTerm)
  | CallType C (EL RawType) (EL RawTerm) (EL RawTerm)
  | GetTypeTag (EL RawType)
  | GetConsSize C (EL RawType)
  | GetConstructorArgTypes C (EL RawType) C (EL RawTerm)
  | TermType (EL RawType)
  | CompileError T.Text

-- elem
type EL a =
  (C, (a, C))

mapEL :: (a -> b) -> EL a -> EL b
mapEL f (c1, (x, c2)) =
  (c1, (f x, c2))
