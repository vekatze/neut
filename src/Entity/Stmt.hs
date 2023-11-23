module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Binder
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.RawBinder
import Entity.RawDecl qualified as RDE
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.StmtKind qualified as SK
import Entity.Term qualified as TM
import Entity.Term.Compress qualified as TM
import Entity.Term.Extend qualified as TM
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText qualified as WT
import GHC.Generics
import Path

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

data RawStmt
  = RawStmtDefine
      IsConstLike
      SK.RawStmtKind
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [RawBinder RT.RawTerm]
      RT.RawTerm
      RT.RawTerm
  | RawStmtDefineConst Hint DD.DefiniteDescription RT.RawTerm RT.RawTerm
  | RawStmtDeclare Hint [RDE.RawDecl]

data WeakStmt
  = WeakStmtDefine
      IsConstLike
      (SK.StmtKind WT.WeakTerm)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
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
      AN.ArgNum
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
    StmtDefine isConstLike stmtKind m functionName impArgNum args codType e -> do
      let stmtKind' = TM.compressStmtKind stmtKind
      let args' = map TM.compressBinder args
      let codType' = TM.compress codType
      let e' = TM.compress e
      StmtDefine isConstLike stmtKind' m functionName impArgNum args' codType' e'
    StmtDefineConst m dd t e -> do
      let t' = TM.compress t
      let e' = TM.compress e
      StmtDefineConst m dd t' e'

extend :: StrippedStmt -> Stmt
extend stmt =
  case stmt of
    StmtDefine isConstLike stmtKind m functionName impArgNum args codType e -> do
      let stmtKind' = TM.extendStmtKind stmtKind
      let args' = map TM.extendBinder args
      let codType' = TM.extend codType
      let e' = TM.extend e
      StmtDefine isConstLike stmtKind' m functionName impArgNum args' codType' e'
    StmtDefineConst m dd t e -> do
      let t' = TM.extend t
      let e' = TM.extend e
      StmtDefineConst m dd t' e'

showStmt :: WeakStmt -> T.Text
showStmt stmt =
  case stmt of
    WeakStmtDefine _ _ m x _ xts codType e ->
      DD.reify x <> "\n" <> WT.toText (m :< WT.Pi xts codType) <> "\n" <> WT.toText (m :< WT.Pi xts e)
    _ ->
      "<define-resource>"

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
