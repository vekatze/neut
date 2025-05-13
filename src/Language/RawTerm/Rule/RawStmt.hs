module Language.RawTerm.Rule.RawStmt
  ( BaseRawProgram (..),
    RawProgram,
    BaseRawStmt (..),
    RawStmt,
    PostRawStmt (..),
    PostRawProgram (..),
    RawStmtKind,
    RawConsInfo,
    RawImport (..),
    RawImportItem (..),
    getPostRawStmtName,
    compareImportItem,
    isImportEmpty,
    mergeImportList,
    RawForeignItemF (..),
    RawForeignItem,
  )
where

import Logger.Rule.Hint
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE
import Data.Text qualified as T
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.ForeignCodType qualified as F
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawTerm qualified as RT

data BaseRawProgram a
  = RawProgram Hint [(RawImport, C)] [(BaseRawStmt a, C)]

type RawProgram =
  BaseRawProgram BN.BaseName

-- type PostRawProgram =
--   BaseRawProgram DD.DefiniteDescription

type RawConsInfo a =
  (Hint, a, IsConstLike, SE.Series (RawBinder RT.RawTerm), Loc)

type RawStmtKind a =
  SK.BaseStmtKind a (RawBinder RT.RawTerm) ()

data BaseRawStmt name
  = RawStmtDefine
      C
      (RawStmtKind name)
      (RT.RawDef name)
  | RawStmtDefineData
      C
      Hint
      (name, C)
      (Maybe (RT.Args RT.RawTerm))
      (SE.Series (RawConsInfo name))
      Loc
  | RawStmtDefineResource
      C
      Hint
      (name, C)
      (C, RT.RawTerm)
      (C, RT.RawTerm)
      C
  | RawStmtNominal C Hint (SE.Series (RT.RawGeist name, Loc))
  | RawStmtForeign C (SE.Series RawForeignItem)

type RawStmt =
  BaseRawStmt BN.BaseName

data RawImport
  = RawImport C Hint (SE.Series RawImportItem) Loc

-- type PostRawStmt =
--   BaseRawStmt DD.DefiniteDescription
data PostRawProgram
  = PostRawProgram Hint [(RawImport, C)] [PostRawStmt]

data PostRawStmt
  = PostRawStmtDefine
      C
      (RawStmtKind DD.DefiniteDescription)
      (RT.RawDef DD.DefiniteDescription)
  | PostRawStmtDefineResource
      C
      Hint
      (DD.DefiniteDescription, C)
      (C, RT.RawTerm)
      (C, RT.RawTerm)
      C
  | PostRawStmtNominal C Hint (SE.Series (RT.RawGeist DD.DefiniteDescription, Loc))
  | PostRawStmtForeign C (SE.Series RawForeignItem)

getPostRawStmtName :: PostRawStmt -> [(Hint, DD.DefiniteDescription)]
getPostRawStmtName stmt =
  case stmt of
    PostRawStmtDefine _ _ def -> do
      let m = RT.loc $ RT.geist def
      let name = fst $ RT.name $ RT.geist def
      [(m, name)]
    PostRawStmtDefineResource _ m (name, _) _ _ _ ->
      [(m, name)]
    PostRawStmtNominal {} ->
      []
    PostRawStmtForeign {} ->
      []

data RawImportItem
  = RawImportItem Hint (T.Text, C) (SE.Series (Hint, LL.LocalLocator))
  | RawStaticKey Hint C (SE.Series (Hint, T.Text))

compareImportItem :: RawImportItem -> RawImportItem -> Ordering
compareImportItem item1 item2 = do
  case (item1, item2) of
    (RawImportItem _ locator1 _, RawImportItem _ locator2 _) ->
      compare locator1 locator2
    (RawImportItem {}, RawStaticKey {}) ->
      LT
    (RawStaticKey {}, RawImportItem {}) ->
      GT
    (RawStaticKey {}, RawStaticKey {}) ->
      EQ

data RawForeignItemF a
  = RawForeignItemF Hint EN.ExternalName C (SE.Series a) C C (F.ForeignCodType a)
  deriving (Functor, Foldable, Traversable)

type RawForeignItem =
  RawForeignItemF RT.RawTerm

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
