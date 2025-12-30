module Language.Term.Stmt
  ( ConsInfo,
    StmtF (..),
    Stmt,
    StrippedStmt,
    getStmtName,
    getStmtName',
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
import Language.Common.StmtKind qualified as SK
import Language.Term.Term qualified as TM
import Logger.Hint

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

data StmtF a
  = StmtDefine
      IsConstLike
      (SK.StmtKind a)
      SavedHint
      DD.DefiniteDescription
      [(BinderF a, Maybe a)]
      [BinderF a]
      a
      a
  | StmtVariadic RuleKind SavedHint DD.DefiniteDescription
  | StmtForeign [F.Foreign]
  deriving (Generic)

type Stmt = StmtF TM.Term

type StrippedStmt = StmtF (Cofree TM.TermF ())

instance Binary Stmt

instance Binary StrippedStmt

getStmtName :: [Stmt] -> [(Hint, DD.DefiniteDescription)]
getStmtName =
  mapMaybe getStmtName'

getStmtName' :: Stmt -> Maybe (Hint, DD.DefiniteDescription)
getStmtName' stmt =
  case stmt of
    StmtDefine _ _ (SavedHint m) name _ _ _ _ ->
      return (m, name)
    StmtVariadic _ (SavedHint m) name ->
      return (m, name)
    StmtForeign _ ->
      Nothing
