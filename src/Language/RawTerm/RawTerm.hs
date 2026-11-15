module Language.RawTerm.RawTerm
  ( RawTerm,
    IntrospectClauseKey,
    RawTermF (..),
    RawType,
    RawTypeF (..),
    RawPiKind (..),
    DefInfo,
    FuncInfo,
    TopDef,
    TopGeist,
    LetKind (..),
    RawMagic (..),
    MarkedArg,
    KeyValueArg (..),
    PrevValue (..),
    StaticItem (..),
    KeywordClause,
    EL,
    MustIgnoreRelayedVars,
    Params,
    RawGeist (..),
    GeistKeys (..),
    getGeistKeys,
    RawDef (..),
    RawTypeDef (..),
    RawImpVar,
    VarArg,
    getDefName,
    emptyParams,
    emptyImpParams,
    emptyDefaultParams,
    extractParams,
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
import Language.Common.CallSite
import Language.Common.CodeVariant (CodeVariant)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.HoleID
import Language.Common.IsConstLike
import Language.Common.IsDestPassing
import Language.Common.LocalDefKind qualified as LDK
import Language.Common.Noema qualified as N
import Language.Common.Rune qualified as R
import Language.Common.VarKind (VarKind)
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

type IntrospectClauseKey =
  (Hint, Maybe T.Text)

data RawPiKind
  = PiNormal
  | PiDestPass
  | PiDataIntro

data RawTypeF a
  = Tau
  | TypeHole HoleID
  | TyVar Name
  | TyApp a C (SE.Series a)
  | Pi (SE.Series (RawBinder a), C) (Params a) (SE.Series (RawBinder a), C) RawPiKind C a Loc
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | Box a
  | BoxNoema a
  | Embed a
  | Code a
  | Rune
  | Pointer
  | Void
  | Option a
  | TyBrace C (a, C)
  | TyIntrospect C T.Text C (SE.Series (IntrospectClauseKey, C, a))

type RawType = Cofree RawTypeF Hint

type RawTerm = Cofree RawTermF Hint

type MarkedArg a = (a, IsSourceArg)

data PrevValue = PrevValue
  { prevValueLoc :: Hint,
    prevValueKind :: VarKind,
    prevValueName :: RawIdent,
    prevValueKeywordComment :: C,
    prevValueComment :: C
  }

data KeyValueArg a = KeyValueArg
  { kvLoc :: Hint,
    kvKey :: Key,
    kvKeyComment :: C,
    kvValueComment :: C,
    kvPrevValue :: Maybe PrevValue,
    kvValue :: MarkedArg a
  }

data RawTermF a
  = Var Name
  | VarGlobal DD.DefiniteDescription GN.GlobalName
  | PiIntro C FuncInfo
  | PiIntroFunction C FuncInfo
  | PiElim a C (Maybe (SE.Series RawType)) IsDestCall C (SE.Series (MarkedArg a)) C (Maybe (SE.Series (Hint, Key, C, C, a)))
  | PiElimImplicit Name C (SE.Series RawType)
  | PiElimByKey Name C (Maybe (SE.Series RawType)) IsDestCall C (SE.Series (KeyValueArg a)) (Maybe (Hint, C, C, a)) -- auxiliary syntax for key-call
  | PiElimRule Name C (SE.Series a)
  | PiElimMeta Name C (Maybe (SE.Series RawType)) C (SE.Series a) C (Maybe (SE.Series (Hint, Key, C, C, a)))
  | PiElimMetaByKey Name C (Maybe (SE.Series RawType)) C (SE.Series (Hint, Key, C, C, a))
  | PiElimExact C a
  | DataIntro AttrDI.Attr DD.DefiniteDescription [RawType] [RawType] [a] -- (attr, consName, dataArgs, consImpArgs, consArgs)
  | DataElim C N.IsNoetic (SE.Series a) (SE.Series (RP.RawPatternRow a))
  | BoxIntro C C (SE.Series (Hint, VarKind, RawIdent)) (a, C)
  | BoxIntroLift C C (a, C)
  | EmbedIntro C C (a, C)
  | BoxElim NecessityVariant Bool C (PatBinder RawType) C (SE.Series (Hint, VarKind, RawIdent)) C a C Loc C a Loc
  | CodeIntro CodeVariant C C (a, C)
  | CodeElim C C (a, C)
  | TauIntro C (EL RawType)
  | TauElim C (Hint, VarKind, RawIdent, C) C a C Loc C a Loc
  | Embody a
  | Let LetKind C (PatBinder RawType) C C a C Loc C a Loc
  | LetOn LetKind C (PatBinder RawType) C (SE.Series (Hint, VarKind, RawIdent)) C a C Loc C a Loc
  | LetRec LDK.LocalDefKind C DefInfo C Loc C a Loc
  | Pin C (RawBinder RawType) C (SE.Series (Hint, VarKind, RawIdent)) C a C Loc C a Loc
  | Invoke C (SE.Series (Hint, Name)) C a
  | RuneIntro R.Rune
  | Magic C RawMagic -- (magic kind arg-1 ... arg-n)
  | Annotation LogLevel (Annot.Annotation ()) a
  | If (KeywordClause a) [KeywordClause a] (EL a)
  | When (KeywordClause a)
  | Seq (a, C) C a
  | SeqEnd a
  | Admit
  | Introspect C T.Text C (SE.Series (IntrospectClauseKey, C, a))
  | Static C Hint StaticItem
  | String T.Text
  | With (KeywordClause a)
  | Brace C (a, C)
  | Int Integer

data StaticItem
  = StaticFileKey T.Text

type PatBinder a =
  (Hint, RP.RawPattern, C, C, a)

type Params a =
  (SE.Series (RawBinder a), C)

emptyParams :: Params a
emptyParams =
  (SE.emptySeriesPC, [])

emptyImpParams :: (SE.Series a, C)
emptyImpParams =
  (SE.emptySeriesAC, [])

emptyDefaultParams :: (SE.Series a, C)
emptyDefaultParams =
  (SE.emptySeries (Just SE.Bracket) SE.Comma, [])

extractParams :: Params a -> [RawBinder a]
extractParams (series, _) =
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
    isDestPassing :: IsDestPassing,
    impParams :: (SE.Series (RawBinder RawType), C),
    expParams :: Params RawType,
    defaultParams :: (SE.Series (RawBinder RawType, RawTerm), C),
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

data GeistKeys = GeistKeys
  { geistImpKeys :: [Key],
    geistExpKeys :: [Key],
    geistDefaultKeys :: [Key]
  }

getGeistKeys :: RawGeist a -> GeistKeys
getGeistKeys geist = do
  let binderKey (_, _, x, _, _, _) = x
  GeistKeys
    { geistImpKeys = map binderKey $ extractParams $ impParams geist,
      geistExpKeys = map binderKey $ extractParams $ expParams geist,
      geistDefaultKeys = map (binderKey . fst) $ SE.extract $ fst $ defaultParams geist
    }

getDefName :: RawDef a -> a
getDefName def =
  fst $ name $ geist def

force :: RawTerm -> RawTerm
force e@(m :< _) =
  m :< piElim e []

piElim :: a -> [a] -> RawTermF a
piElim e es =
  PiElim e [] Nothing False [] (SE.fromList' (map (,False) es)) [] Nothing

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
                  isDestPassing = False,
                  impParams = (SE.emptySeries (Just SE.Angle) SE.Comma, []),
                  defaultParams = (SE.emptySeries (Just SE.Bracket) SE.Comma, []),
                  expParams = (SE.assoc $ SE.fromList SE.Paren SE.Comma varList, []),
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
  | Bind

{-# INLINE decodeLetKind #-}
decodeLetKind :: LetKind -> T.Text
decodeLetKind letKind =
  case letKind of
    Plain _ -> "let"
    Noetic -> "tie"
    Try -> "try"
    Bind -> "bind"

letKindFromText :: T.Text -> Maybe LetKind
letKindFromText t =
  case t of
    "let" ->
      return $ Plain False
    "tie" ->
      return Noetic
    "try" ->
      return Try
    "bind" ->
      return Bind
    _ ->
      Nothing

type VarArg =
  (Hint, RawType, C, RawTerm)

data RawMagic
  = Cast C (EL RawType) (EL RawType) (EL RawTerm) (Maybe C)
  | Store C (EL RawType) (EL RawTerm) (EL RawTerm) (Maybe C)
  | Load C (EL RawType) (EL RawTerm) (Maybe C)
  | Shift C (EL RawTerm) (EL RawTerm) (Maybe C)
  | Alloca C (EL RawType) (EL RawTerm) (Maybe C)
  | Calloc C (EL RawTerm) (EL RawTerm) (Maybe C)
  | Malloc C (EL RawTerm) (Maybe C)
  | MallocOpaque C (EL RawTerm) (Maybe C)
  | Realloc C (EL RawTerm) (EL RawTerm) (Maybe C)
  | Free C (EL RawTerm) (Maybe C)
  | External C Hint EN.ExternalName C (SE.Series RawTerm) (Maybe (C, SE.Series VarArg))
  | Global C Hint EN.ExternalName
  | OpaqueValue C (EL RawTerm)
  | CallType C (EL RawTerm) (EL RawTerm) (EL RawTerm) (EL RawTerm) (Maybe C)
  | InspectType (EL RawType) (Maybe C)
  | EqType (EL RawType) (EL RawType) (Maybe C)
  | ShowType C (EL RawType) (Maybe C)
  | TextCons C (EL RawTerm) (EL RawTerm) (Maybe C)
  | TextUncons C (EL RawTerm) (Maybe C)
  | MakeSwitch C (EL RawTerm) (EL RawTerm) (EL RawTerm) (Maybe C)
  | FreeVariables C (EL RawTerm) (Maybe C)
  | CompileError C (EL RawTerm) (Maybe C)
  | GetOriginFileName C C
  | GetOriginLine C C
  | GetOriginColumn C C

-- elem
type EL a =
  (C, (a, C))

mapEL :: (a -> b) -> EL a -> EL b
mapEL f (c1, (x, c2)) =
  (c1, (f x, c2))
