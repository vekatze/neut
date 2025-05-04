module Language.RawTerm.Rule.RawStmt
  ( RawProgram (..),
    RawStmt (..),
    RawStmtKind,
    RawConsInfo,
    RawImport (..),
    RawImportItem (..),
    compareImportItem,
    isImportEmpty,
    mergeImportList,
    RawForeignItemF (..),
    RawForeignItem,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.ForeignCodType qualified as F
import Language.Common.Rule.Hint
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.C
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawTerm qualified as RT
import Language.RawTerm.Rule.Syntax.Series qualified as SE

data RawProgram
  = RawProgram Hint [(RawImport, C)] [(RawStmt, C)]

type RawConsInfo a =
  (Hint, a, IsConstLike, SE.Series (RawBinder RT.RawTerm), Loc)

type RawStmtKind a =
  SK.BaseStmtKind a (RawBinder RT.RawTerm) RT.RawTerm

data RawStmt
  = RawStmtDefine
      C
      (RawStmtKind BN.BaseName)
      RT.TopDef
  | RawStmtDefineData
      C
      Hint
      (BN.BaseName, C)
      (Maybe (RT.Args RT.RawTerm))
      (SE.Series (RawConsInfo BN.BaseName))
      Loc
  | RawStmtDefineResource
      C
      Hint
      (BN.BaseName, C)
      (C, RT.RawTerm)
      (C, RT.RawTerm)
      C
  | RawStmtNominal C Hint (SE.Series (RT.TopGeist, Loc))
  | RawStmtForeign C (SE.Series RawForeignItem)

data RawImport
  = RawImport C Hint (SE.Series RawImportItem) Loc

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
