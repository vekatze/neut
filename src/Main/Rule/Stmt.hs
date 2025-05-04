module Main.Rule.Stmt
  ( WeakStmt (..),
    ConsInfo,
    Program,
    StmtF (..),
    Stmt,
    StrippedStmt,
    PathSet,
    WeakForeign,
    compress,
    extend,
    getStmtName,
    getWeakStmtName,
  )
where

import Control.Comonad.Cofree
import Data.Binary
import Data.Maybe
import Data.Set qualified as S
import GHC.Generics hiding (C)
import Main.Rule.Binder
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Discriminant qualified as D
import Main.Rule.Foreign qualified as F
import Main.Rule.Geist qualified as G
import Main.Rule.Hint
import Main.Rule.IsConstLike
import Main.Rule.Source qualified as Source
import Main.Rule.StmtKind qualified as SK
import Main.Rule.Term qualified as TM
import Main.Rule.Term.Compress qualified as TM
import Main.Rule.Term.Extend qualified as TM
import Main.Rule.WeakTerm qualified as WT
import Path

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

type WeakForeign =
  F.BaseForeign WT.WeakTerm

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
  | WeakStmtNominal Hint [G.Geist WT.WeakTerm]
  | WeakStmtForeign [WT.WeakForeign]

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
  | StmtForeign [F.Foreign]
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
    StmtForeign foreignList ->
      StmtForeign foreignList

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
    StmtForeign foreignList ->
      StmtForeign foreignList

getStmtName :: [Stmt] -> [(Hint, DD.DefiniteDescription)]
getStmtName =
  mapMaybe getStmtName'

getStmtName' :: Stmt -> Maybe (Hint, DD.DefiniteDescription)
getStmtName' stmt =
  case stmt of
    StmtDefine _ _ (SavedHint m) name _ _ _ _ ->
      return (m, name)
    StmtForeign _ ->
      Nothing

getWeakStmtName :: [WeakStmt] -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName =
  concatMap getWeakStmtName'

getWeakStmtName' :: WeakStmt -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName' stmt =
  case stmt of
    WeakStmtDefine _ _ m name _ _ _ _ ->
      [(m, name)]
    WeakStmtNominal {} ->
      []
    WeakStmtForeign {} ->
      []
