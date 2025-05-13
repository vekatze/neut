module Language.Term.Rule.Stmt
  ( ConsInfo,
    StmtF (..),
    Stmt,
    StrippedStmt,
    getStmtName,
    getStmtName',
  )
where

import Logger.Rule.Hint
import Control.Comonad.Cofree
import Data.Binary
import Data.Maybe
import GHC.Generics hiding (C)
import Language.Common.Rule.Binder
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.Foreign qualified as F
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.StmtKind qualified as SK
import Language.Term.Rule.Term qualified as TM

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

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
