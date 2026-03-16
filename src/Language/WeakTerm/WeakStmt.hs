module Language.WeakTerm.WeakStmt
  ( WeakStmt (..),
    WeakStmtKindTerm,
    WeakStmtKindType,
    WeakForeign,
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
      [BinderF WT.WeakType]
      [(BinderF WT.WeakType, WT.WeakTerm)]
      WT.WeakType
      WT.WeakTerm
  | WeakStmtDefineType
      IsConstLike
      WeakStmtKindType
      Hint
      DD.DefiniteDescription
      [BinderF WT.WeakType]
      [BinderF WT.WeakType]
      [(BinderF WT.WeakType, WT.WeakTerm)]
      WT.WeakType
      WT.WeakType
  | WeakStmtDefineResource
      Hint
      DD.DefiniteDescription
      Int
      WT.WeakType -- unitType
      WT.WeakTerm -- discarder
      WT.WeakTerm -- copier
      WT.WeakTerm -- resourceSize
  | WeakStmtVariadic RuleKind Hint DD.DefiniteDescription
  | WeakStmtNominal Hint [(NominalTag, G.Geist WT.WeakType WT.WeakTerm)]
  | WeakStmtForeign [WT.WeakForeign]
