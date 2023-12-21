module Entity.RawProgram
  ( RawProgram (..),
    RawStmt (..),
    RawConsInfo,
    RawImport (..),
    RawImportItem (..),
    RawForeign (..),
    RawForeignItem (..),
  )
where

import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.LowType qualified as LT
import Entity.RawBinder
import Entity.RawTerm qualified as RT
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE

data RawProgram
  = RawProgram Hint (Maybe RawImport) C (Maybe RawForeign) C [(RawStmt, C)]

data RawStmt
  = RawStmtDefine
      C
      SK.RawStmtKind
      RT.TopDefHeader
      (C, (RT.RawTerm, C))
  | RawStmtDefineConst
      C
      Hint
      (DD.DefiniteDescription, C)
      (C, (RT.RawTerm, C))
      (C, (RT.RawTerm, C))
  | RawStmtDefineData
      C
      Hint
      (DD.DefiniteDescription, C)
      (Maybe (RT.Args RT.RawTerm))
      (SE.Series (RawConsInfo BN.BaseName))
  | RawStmtDefineResource
      C
      Hint
      (DD.DefiniteDescription, C)
      C
      (C, (RT.RawTerm, C))
      (C, (RT.RawTerm, C))
  | RawStmtDeclare C Hint (SE.Series RT.TopDefHeader)

type RawConsInfo a =
  (Hint, (a, C), IsConstLike, SE.Series (RawBinder RT.RawTerm))

data RawImport
  = RawImport C Hint (SE.Series RawImportItem)

data RawImportItem
  = RawImportItem C Hint (T.Text, C) (SE.Series (Hint, LL.LocalLocator))

data RawForeign
  = RawForeign C (SE.Series RawForeignItem)

data RawForeignItem
  = RawForeignItem EN.ExternalName C (SE.Series LT.LowType) C C LT.LowType
