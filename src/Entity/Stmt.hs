module Entity.Stmt
  ( WeakStmt (..),
    ConsInfo,
    Program,
    StmtF (..),
    Stmt,
    StrippedStmt,
    PathSet,
    compress,
    extend,
  )
where

import Control.Comonad.Cofree
import Data.Binary
import Data.Set qualified as S
import Entity.Binder
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.Source qualified as Source
import Entity.StmtKind qualified as SK
import Entity.Term qualified as TM
import Entity.Term.Compress qualified as TM
import Entity.Term.Extend qualified as TM
import Entity.WeakTerm qualified as WT
import GHC.Generics hiding (C)
import Path

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

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
