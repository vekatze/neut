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
import Entity.RawDecl qualified as RDE
import Entity.RawTerm qualified as RT
import Entity.StmtKind qualified as SK

data RawProgram
  = RawProgram Hint (Maybe (RawImport, C)) (Maybe (RawForeign, C)) [(RawStmt, C)]

data RawStmt
  = RawStmtDefine
      C
      IsConstLike
      SK.RawStmtKind
      Hint
      (DD.DefiniteDescription, C)
      RDE.ImpArgs
      RDE.ExpArgs
      (C, (RT.RawTerm, C))
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
      (Maybe RDE.ExpArgs)
      C
      [(C, RawConsInfo)]
  | RawStmtDefineResource
      C
      Hint
      (DD.DefiniteDescription, C)
      C
      (C, (RT.RawTerm, C))
      (C, (RT.RawTerm, C))
  | RawStmtDeclare C Hint C [(C, RDE.RawDecl)]

type RawConsInfo =
  (Hint, (BN.BaseName, C), IsConstLike, RDE.ExpArgs)

data RawImport
  = RawImport C Hint (C, [(C, RawImportItem)])

data RawImportItem
  = RawImportItem C Hint (T.Text, C) (ArgList ((Hint, LL.LocalLocator), C))

data RawForeign
  = RawForeign C (C, [(C, RawForeignItem)])

data RawForeignItem
  = RawForeignItem EN.ExternalName C (ArgList (LT.LowType, C)) C (LT.LowType, C)
