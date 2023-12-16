module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.C
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.LowType qualified as LT
import Entity.RawDecl qualified as RDE
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.StmtKind qualified as SK
import Entity.Term qualified as TM
import Entity.Term.Compress qualified as TM
import Entity.Term.Extend qualified as TM
import Entity.WeakTerm qualified as WT
import GHC.Generics hiding (C)
import Path

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

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

data RawProgram
  = RawProgram (Maybe (RawImport, C)) (Maybe (RawForeign, C)) [(RawStmt, C)]

data WeakStmt
  = WeakStmtDefine
      IsConstLike
      (SK.StmtKind WT.WeakTerm)
      Hint
      DD.DefiniteDescription
      [BinderF WT.WeakTerm]
      [BinderF WT.WeakTerm]
      WT.WeakTerm
      WT.WeakTerm
  | WeakStmtDefineConst Hint DD.DefiniteDescription WT.WeakTerm WT.WeakTerm
  | WeakStmtDeclare Hint [DE.Decl WT.WeakTerm]

type Program =
  (Source.Source, [Stmt])

data StmtF a
  = StmtDefine
      IsConstLike
      (SK.StmtKind a)
      SavedHint
      DD.DefiniteDescription
      [BinderF a]
      [BinderF a]
      a
      a
  | StmtDefineConst SavedHint DD.DefiniteDescription a a
  deriving (Generic)

type Stmt = StmtF TM.Term

type StrippedStmt = StmtF (Cofree TM.TermF ())

instance Binary Stmt

instance Binary StrippedStmt

type PathSet = S.Set (Path Abs File)

compress :: Stmt -> StrippedStmt
compress stmt =
  case stmt of
    StmtDefine isConstLike stmtKind m functionName impArgs expArgs codType e -> do
      let stmtKind' = TM.compressStmtKind stmtKind
      let impArgs' = map TM.compressBinder impArgs
      let expArgs' = map TM.compressBinder expArgs
      let codType' = TM.compress codType
      let e' = TM.compress e
      StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' e'
    StmtDefineConst m dd t e -> do
      let t' = TM.compress t
      let e' = TM.compress e
      StmtDefineConst m dd t' e'

extend :: StrippedStmt -> Stmt
extend stmt =
  case stmt of
    StmtDefine isConstLike stmtKind m functionName impArgs expArgs codType e -> do
      let stmtKind' = TM.extendStmtKind stmtKind
      let impArgs' = map TM.extendBinder impArgs
      let expArgs' = map TM.extendBinder expArgs
      let codType' = TM.extend codType
      let e' = TM.extend e
      StmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' e'
    StmtDefineConst m dd t e -> do
      let t' = TM.extend t
      let e' = TM.extend e
      StmtDefineConst m dd t' e'

argToTerm :: BinderF TM.Term -> TM.Term
argToTerm (m, x, _) =
  m :< TM.Var x

addDiscriminants :: [(a, [(b, c)])] -> [(a, [(b, c, D.Discriminant)])]
addDiscriminants info = do
  let (formInfo, introInfo) = unzip info
  zip formInfo $ map (addDiscriminants' D.zero) introInfo

addDiscriminants' :: D.Discriminant -> [(b, c)] -> [(b, c, D.Discriminant)]
addDiscriminants' d xs =
  case xs of
    [] ->
      []
    (x, y) : rest ->
      (x, y, d) : addDiscriminants' (D.increment d) rest
