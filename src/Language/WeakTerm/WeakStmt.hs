module Language.WeakTerm.WeakStmt
  ( WeakStmt (..),
    WeakForeign,
    getWeakStmtName,
  )
where

import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.Geist qualified as G
import Language.Common.IsConstLike
import Language.Common.RuleKind (RuleKind)
import Language.Common.StmtKind qualified as SK
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

type WeakForeign =
  F.BaseForeign WT.WeakTerm

type WeakStmtKind =
  SK.BaseStmtKind DD.DefiniteDescription (BinderF WT.WeakTerm) WT.WeakTerm

data WeakStmt
  = WeakStmtDefine
      IsConstLike
      WeakStmtKind
      Hint
      DD.DefiniteDescription
      [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)]
      [BinderF WT.WeakTerm]
      WT.WeakTerm
      WT.WeakTerm
  | WeakStmtVariadic RuleKind Hint DD.DefiniteDescription
  | WeakStmtNominal Hint [G.Geist WT.WeakTerm]
  | WeakStmtForeign [WT.WeakForeign]

getWeakStmtName :: [WeakStmt] -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName =
  concatMap getWeakStmtName'

getWeakStmtName' :: WeakStmt -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName' stmt =
  case stmt of
    WeakStmtDefine _ _ m name _ _ _ _ ->
      [(m, name)]
    WeakStmtVariadic _ m name ->
      [(m, name)]
    WeakStmtNominal {} ->
      []
    WeakStmtForeign {} ->
      []
