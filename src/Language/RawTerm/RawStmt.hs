module Language.RawTerm.RawStmt
  ( BaseRawProgram (..),
    RawProgram,
    BaseRawStmt (..),
    RawStmt,
    PostRawStmt (..),
    PostRawProgram (..),
    RawStmtKindTerm,
    RawStmtKindType,
    AliasKind (..),
    RawConsInfo (..),
    RawDefineMeta (..),
    PostRawDefineMeta (..),
    RawImport (..),
    RawImportItem (..),
    isImportEmpty,
    mergeImportList,
    RawForeignItemF (..),
    RawForeignItem,
  )
where

import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.DataInfo (FieldHint)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as F
import Language.Common.LocalLocator qualified as LL
import Language.Common.NominalTag
import Language.Common.RuleKind
import Language.Common.StmtKind qualified as SK
import Language.RawTerm.Name qualified as N
import Language.RawTerm.RawBinder
import Language.RawTerm.RawTerm qualified as RT
import Logger.Hint
import SyntaxTree.C
import SyntaxTree.Series qualified as SE

data BaseRawProgram a
  = RawProgram Hint [(RawImport, C)] [(BaseRawStmt a, C)]

type RawProgram =
  BaseRawProgram BN.BaseName

data RawConsInfo a = RawConsInfo
  { loc :: Hint,
    name :: a,
    expArgs :: Maybe (SE.Series (FieldHint, RawBinder RT.RawType)),
    endLoc :: Loc
  }

data RawDefineMeta name = RawDefineMeta
  { defineMetaLoc :: Hint,
    defineMetaTarget :: (name, C),
    defineMetaTargetArgs :: (SE.Series RT.RawType, C),
    defineMetaExpArgs :: RT.Args RT.RawType,
    defineMetaCod :: (C, RT.RawType),
    defineMetaBody :: RT.RawTerm,
    defineMetaEndLoc :: Loc
  }

type RawStmtKindTerm a =
  SK.BaseStmtKindTerm a (RawBinder RT.RawType) ()

type RawStmtKindType =
  SK.BaseStmtKindType (RawBinder RT.RawType)

data AliasKind
  = TransparentAlias
  | OpaqueAlias
  deriving (Show, Eq)

data BaseRawStmt name
  = RawStmtDefineTerm
      C
      (RawStmtKindTerm name)
      (RT.RawDef name)
  | RawStmtDefineType
      C
      AliasKind
      (RT.RawTypeDef name)
  | RawStmtDefineData
      C
      Hint
      (name, C)
      (Maybe (RT.Args RT.RawType))
      (SE.Series (RawConsInfo name))
      Loc
  | RawStmtDefineResource
      C
      Hint
      (name, C)
      (C, RT.RawTerm) -- discarder
      (C, RT.RawTerm) -- copier
      (C, RT.RawTerm) -- resourceSize
      C
  | RawStmtTrope C Hint (name, C) (SE.Series (RawDefineMeta N.Name)) Loc
  | RawStmtVariadic
      RuleKind
      C
      Hint
      (name, C)
      (C, RT.RawTerm, RT.RawType)
      (C, RT.RawTerm, RT.RawType)
      (C, RT.RawTerm, RT.RawType)
      C
      Loc
  | RawStmtNominal C Hint (SE.Series (NominalTag, RT.RawGeist name, Loc))
  | RawStmtForeign C (SE.Series RawForeignItem)

type RawStmt =
  BaseRawStmt BN.BaseName

data RawImport
  = RawImport C Hint (SE.Series RawImportItem) Loc

data PostRawProgram
  = PostRawProgram Hint [(RawImport, C)] [PostRawStmt]

data PostRawStmt
  = PostRawStmtDefineTerm
      C
      (RawStmtKindTerm DD.DefiniteDescription)
      (RT.RawDef DD.DefiniteDescription)
  | PostRawStmtDefineType
      C
      RawStmtKindType
      (RT.RawTypeDef DD.DefiniteDescription)
  | PostRawStmtDefineResource
      C
      Hint
      (DD.DefiniteDescription, C)
      (C, RT.RawTerm) -- discarder
      (C, RT.RawTerm) -- copier
      (C, RT.RawTerm) -- resourceSize
      C
  | PostRawStmtTrope
      C
      Hint
      (DD.DefiniteDescription, C)
      (SE.Series PostRawDefineMeta)
      Loc
  | PostRawStmtVariadic
      RuleKind
      Hint
      DD.DefiniteDescription
  | PostRawStmtNominal C Hint (SE.Series (NominalTag, RT.RawGeist DD.DefiniteDescription, Loc))
  | PostRawStmtForeign C (SE.Series RawForeignItem)

data PostRawDefineMeta = PostRawDefineMeta
  { postDefineMetaLoc :: Hint,
    postDefineMetaTarget :: (N.Name, C),
    postDefineMetaTargetArgs :: (SE.Series RT.RawType, C),
    postDefineMetaExpArgs :: RT.Args RT.RawType,
    postDefineMetaCod :: (C, RT.RawType),
    postDefineMetaBody :: RT.RawTerm,
    postDefineMetaEndLoc :: Loc,
    postDefineMetaHelperName :: DD.DefiniteDescription
  }

data RawImportItem
  = RawImportItem Hint (T.Text, C) (SE.Series (Hint, LL.LocalLocator))
  | RawStaticFileKey Hint C (SE.Series (Hint, T.Text))

data RawForeignItemF a
  = RawForeignItemF Hint EN.ExternalName C (SE.Series a) C C (F.ForeignCodType a)
  deriving (Functor, Foldable, Traversable)

type RawForeignItem =
  RawForeignItemF RT.RawType

isImportEmpty :: RawImport -> Bool
isImportEmpty rawImport =
  case rawImport of
    RawImport [] _ series _
      | SE.isEmpty series ->
          True
    _ ->
      False

mergeImportList :: Hint -> [(RawImport, C)] -> (RawImport, C)
mergeImportList headHint importList =
  case importList of
    [] -> do
      let beginningOfFile = (1, 1)
      (RawImport [] headHint (SE.emptySeries (Just SE.Brace) SE.Comma) beginningOfFile, [])
    (RawImport c1 m importItems loc, c) : rest -> do
      let (RawImport c1' _ importItems' _, c') = mergeImportList headHint rest
      (RawImport (c1 ++ c1') m (SE.appendLeftBiased importItems importItems') loc, c ++ c')
