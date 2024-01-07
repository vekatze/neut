module Entity.RawProgram
  ( RawProgram (..),
    RawStmt (..),
    RawConsInfo,
    RawImport (..),
    RawImportItem (..),
    compareImportItem,
    RawForeignItem (..),
  )
where

import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.RawBinder
import Entity.RawLowType qualified as RLT
import Entity.RawTerm qualified as RT
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE

data RawProgram
  = RawProgram Hint (Maybe RawImport) C [(RawStmt, C)]

data RawStmt
  = RawStmtDefine
      C
      (SK.RawStmtKind BN.BaseName)
      RT.TopDef
  | RawStmtDefineConst
      C
      Hint
      (BN.BaseName, C)
      (C, (RT.RawTerm, C))
      (C, (RT.RawTerm, C))
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
      C
      (C, (RT.RawTerm, C))
      (C, (RT.RawTerm, C))
  | RawStmtNominal C Hint (SE.Series (RT.TopGeist, Loc))
  | RawStmtForeign C (SE.Series RawForeignItem)

type RawConsInfo a =
  (Hint, (a, C), IsConstLike, SE.Series (RawBinder RT.RawTerm), Loc)

data RawImport
  = RawImport C Hint (SE.Series RawImportItem)

data RawImportItem
  = RawImportItem Hint (T.Text, C) (SE.Series (Hint, LL.LocalLocator))

compareImportItem :: RawImportItem -> RawImportItem -> Ordering
compareImportItem item1 item2 = do
  let RawImportItem _ locator1 _ = item1
  let RawImportItem _ locator2 _ = item2
  compare locator1 locator2

data RawForeignItem
  = RawForeignItem Hint EN.ExternalName C (SE.Series RLT.RawLowType) C C RLT.RawLowType
