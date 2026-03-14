module Language.Term.Stmt
  ( ConsInfo,
    StmtF (..),
    Stmt,
    StrippedStmt,
    getStmtName,
    getStmtName',
    isMacroStmt,
  )
where

import Control.Comonad.Cofree
import Data.Binary
import Data.Maybe
import GHC.Generics hiding (C)
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Foreign qualified as F
import Language.Common.IsConstLike
import Language.Common.RuleKind (RuleKind)
import Language.Common.StmtKind (isMacroStmtKind)
import Language.Common.StmtKind qualified as SK
import Language.Term.Term qualified as TM
import Logger.Hint

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Type], D.Discriminant)

data StmtF t a
  = StmtDefine
      IsConstLike
      (SK.StmtKindTerm t)
      SavedHint
      DD.DefiniteDescription
      [BinderF t]
      [BinderF t]
      [(BinderF t, a)]
      t
      a
  | StmtDefineType
      IsConstLike
      (SK.StmtKindType t)
      SavedHint
      DD.DefiniteDescription
      [BinderF t]
      [BinderF t]
      [(BinderF t, a)]
      t
      t
  | StmtDefineResource
      SavedHint
      DD.DefiniteDescription
      Int
      Int
      t
      a
      a
  | StmtVariadic RuleKind SavedHint DD.DefiniteDescription
  | StmtForeign [F.Foreign]
  deriving (Generic)

type Stmt = StmtF TM.Type TM.Term

type StrippedStmt = StmtF (Cofree TM.TypeF ()) (Cofree TM.TermF ())

instance (Binary t, Binary a) => Binary (StmtF t a)

getStmtName :: [Stmt] -> [(Hint, DD.DefiniteDescription)]
getStmtName =
  mapMaybe getStmtName'

getStmtName' :: Stmt -> Maybe (Hint, DD.DefiniteDescription)
getStmtName' stmt =
  case stmt of
    StmtDefine _ _ (SavedHint m) name _ _ _ _ _ ->
      return (m, name)
    StmtDefineType _ _ (SavedHint m) name _ _ _ _ _ ->
      return (m, name)
    StmtDefineResource (SavedHint m) name _ _ _ _ _ ->
      return (m, name)
    StmtVariadic _ (SavedHint m) name ->
      return (m, name)
    StmtForeign _ ->
      Nothing

isMacroStmt :: Stmt -> Bool
isMacroStmt stmt =
  case stmt of
    StmtDefine _ stmtKind _ _ _ _ _ _ _ ->
      isMacroStmtKind stmtKind
    _ ->
      False
