module Language.WeakTerm.Rule.WeakStmt
  ( WeakStmt (..),
    WeakForeign,
    getWeakStmtName,
  )
where

import Language.Common.Rule.Binder
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Foreign qualified as F
import Language.Common.Rule.Geist qualified as G
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.StmtKind qualified as SK
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint

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
      [BinderF WT.WeakTerm]
      [BinderF WT.WeakTerm]
      WT.WeakTerm
      WT.WeakTerm
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
    WeakStmtNominal {} ->
      []
    WeakStmtForeign {} ->
      []
