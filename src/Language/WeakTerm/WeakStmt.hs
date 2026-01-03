module Language.WeakTerm.WeakStmt
  ( WeakStmt (..),
    WeakStmtKindTerm,
    WeakStmtKindType,
    WeakForeign,
    getWeakStmtName,
  )
where

import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.Geist qualified as G
import Language.Common.IsConstLike
import Language.Common.NominalTag
import Language.Common.RuleKind (RuleKind)
import Language.Common.StmtKind qualified as SK
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

type WeakForeign =
  F.BaseForeign WT.WeakType

type WeakStmtKindTerm =
  SK.BaseStmtKindTerm DD.DefiniteDescription (BinderF WT.WeakType) WT.WeakType

type WeakStmtKindType =
  SK.BaseStmtKindType DD.DefiniteDescription (BinderF WT.WeakType)

data WeakStmt
  = WeakStmtDefineTerm
      IsConstLike
      WeakStmtKindTerm
      Hint
      DD.DefiniteDescription
      [BinderF WT.WeakType]
      [(BinderF WT.WeakType, WT.WeakTerm)]
      [BinderF WT.WeakType]
      WT.WeakType
      WT.WeakTerm
  | WeakStmtDefineType
      IsConstLike
      WeakStmtKindType
      Hint
      DD.DefiniteDescription
      [BinderF WT.WeakType]
      [(BinderF WT.WeakType, WT.WeakTerm)]
      [BinderF WT.WeakType]
      WT.WeakType
      WT.WeakType
  | WeakStmtVariadic RuleKind Hint DD.DefiniteDescription
  | WeakStmtNominal Hint [(NominalTag, G.Geist WT.WeakType WT.WeakTerm)]
  | WeakStmtForeign [WT.WeakForeign]

getWeakStmtName :: [WeakStmt] -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName =
  concatMap getWeakStmtName'

getWeakStmtName' :: WeakStmt -> [(Hint, DD.DefiniteDescription)]
getWeakStmtName' stmt =
  case stmt of
    WeakStmtDefineTerm _ _ m name _ _ _ _ _ ->
      [(m, name)]
    WeakStmtDefineType _ _ m name _ _ _ _ _ ->
      [(m, name)]
    WeakStmtVariadic _ m name ->
      [(m, name)]
    WeakStmtNominal {} ->
      []
    WeakStmtForeign {} ->
      []
