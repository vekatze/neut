module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.RawBinder
import Entity.RawTerm qualified as RT
import Entity.Source qualified as Source
import Entity.StmtKind qualified as SK
import Entity.Term qualified as TM
import Entity.Term.Compress qualified as TM
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
  | RawStmtDefineResource Hint DD.DefiniteDescription RT.RawTerm RT.RawTerm

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
  | WeakStmtDefineResource Hint DD.DefiniteDescription WT.WeakTerm WT.WeakTerm

type Program =
  (Source.Source, [Stmt])

data Stmt
  = StmtDefine
      IsConstLike
      (SK.StmtKind TM.Term)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF TM.Term]
      TM.Term
      TM.Term
  | StmtDefineConst Hint DD.DefiniteDescription TM.Term TM.Term
  | StmtDefineResource Hint DD.DefiniteDescription TM.Term TM.Term
  deriving (Generic)

instance Binary Stmt

type PathSet = S.Set (Path Abs File)

compress :: Stmt -> Stmt
compress stmt =
  case stmt of
    StmtDefine isConstLike stmtKind m functionName impArgNum args codType e -> do
      let codType' = TM.compress codType
      let e' = TM.compress e
      StmtDefine isConstLike stmtKind m functionName impArgNum args codType' e'
    StmtDefineConst {} ->
      stmt
    StmtDefineResource {} ->
      stmt

getNameFromWeakStmt :: WeakStmt -> DD.DefiniteDescription
getNameFromWeakStmt stmt =
  case stmt of
    WeakStmtDefine _ _ _ functionName _ _ _ _ ->
      functionName
    WeakStmtDefineConst _ constName _ _ ->
      constName
    WeakStmtDefineResource _ resourceName _ _ ->
      resourceName

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
