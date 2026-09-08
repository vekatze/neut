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
    RawRequire (..),
    RawRequireItem (..),
    RawImportItem (..),
    boundNameList,
    RawImportEntry (..),
    RawAsClause (..),
    isImportEmpty,
    mergeImportList,
    mergeRequireList,
    RawForeignItemF (..),
    RawForeignSignatureF (..),
    RawForeignItem,
    RawExposeItem (..),
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
  = RawProgram Hint [(RawImport, C)] [(RawRequire, C)] [(BaseRawStmt a, C)]

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
  | RawStmtExpose C (SE.Series RawExposeItem)
  | RawStmtNamespace C Hint (name, C) C [(BaseRawStmt name, C)] Loc

type RawStmt =
  BaseRawStmt BN.BaseName

data RawImport
  = RawImport C Hint (SE.Series RawImportItem) Loc

data RawRequire
  = RawRequire C Hint (SE.Series RawRequireItem) Loc

data RawRequireItem
  = RawRequireItem Hint T.Text

data PostRawProgram
  = PostRawProgram Hint [(RawImport, C)] [(RawRequire, C)] [PostRawStmt]

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
  | PostRawStmtExpose C [RawExposeItem]
  | PostRawStmtNamespace Hint DD.DefiniteDescription [PostRawStmt]

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
  = RawImportItem Hint (T.Text, C) (SE.Series RawImportEntry)
  | RawStaticFileKey Hint C (SE.Series (Hint, T.Text))
  | RawConditionalImport
      Hint
      C
      (Hint, T.Text, C)
      (SE.Series RawImportItem, C)
      C
      (SE.Series RawImportItem)

boundNameList :: RawImportItem -> [T.Text]
boundNameList item =
  case item of
    RawImportItem _ _ entries ->
      map boundName $ SE.extract entries
    RawStaticFileKey _ _ keys ->
      map snd $ SE.extract keys
    RawConditionalImport _ _ _ (thenSeries, _) _ _ ->
      concatMap boundNameList $ SE.extract thenSeries

boundName :: RawImportEntry -> T.Text
boundName entry =
  case entry of
    RawImportName _ ll Nothing ->
      LL.reify ll
    RawImportName _ _ (Just (RawAsClause _ _ _ alias)) ->
      BN.reify alias
    RawImportWildcard _ (RawAsClause _ _ _ alias) ->
      BN.reify alias

data RawImportEntry
  = RawImportName Hint LL.LocalLocator (Maybe RawAsClause)
  | RawImportWildcard Hint RawAsClause

data RawAsClause
  = RawAsClause C C Hint BN.BaseName

data RawForeignSignatureF a
  = RawForeignFunction (SE.Series a) C C (F.ForeignCodType a)
  | RawForeignVariable C a
  deriving (Functor, Foldable, Traversable)

data RawForeignItemF a
  = RawForeignItemF Hint EN.ExternalName C (RawForeignSignatureF a)
  deriving (Functor, Foldable, Traversable)

type RawForeignItem =
  RawForeignItemF RT.RawType

data RawExposeItem
  = RawExposeItem Hint (N.Name, C) (Maybe (C, (EN.ExternalName, C)))

isImportEmpty :: RawImport -> Bool
isImportEmpty rawImport =
  case rawImport of
    RawImport [] _ series _
      | SE.isEmpty series ->
          True
    _ ->
      False

mergeRequireList :: Hint -> [(RawRequire, C)] -> (RawRequire, C)
mergeRequireList headHint requireList =
  case requireList of
    [] -> do
      let beginningOfFile = (1, 1)
      (RawRequire [] headHint (SE.emptySeries (Just SE.Brace) SE.Comma) beginningOfFile, [])
    (RawRequire c1 m requireItems loc, c) : rest -> do
      let (RawRequire c1' _ requireItems' _, c') = mergeRequireList headHint rest
      (RawRequire (c1 ++ c1') m (SE.appendLeftBiased requireItems requireItems') loc, c ++ c')

mergeImportList :: Hint -> [(RawImport, C)] -> (RawImport, C)
mergeImportList headHint importList =
  case importList of
    [] -> do
      let beginningOfFile = (1, 1)
      (RawImport [] headHint (SE.emptySeries (Just SE.Brace) SE.Comma) beginningOfFile, [])
    (RawImport c1 m importItems loc, c) : rest -> do
      let (RawImport c1' _ importItems' _, c') = mergeImportList headHint rest
      (RawImport (c1 ++ c1') m (SE.appendLeftBiased importItems importItems') loc, c ++ c')
