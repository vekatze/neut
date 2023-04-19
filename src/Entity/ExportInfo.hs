module Entity.ExportInfo
  ( ExportClauseF (..),
    WeakExportInfo,
    WeakExportClause,
    ExportClause,
    ExportInfo,
    VarOrDD,
    AliasName,
    extractName,
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.LocalLocator qualified as LL
import GHC.Generics

data ExportClauseF a
  = Function a
  | Variant a [a] [DD.DefiniteDescription]
  deriving (Generic)

instance Binary a => Binary (ExportClauseF a)

type WeakExportClause =
  ExportClauseF WeakExportInfo

type ExportClause =
  ExportClauseF ExportInfo

type WeakExportInfo =
  ((Hint, AliasName), (Hint, VarOrDD))

type ExportInfo =
  ((Hint, AliasName), (Hint, DD.DefiniteDescription, GN.GlobalName))

type VarOrDD =
  (Either T.Text (GL.GlobalLocator, LL.LocalLocator))

type AliasName =
  DD.DefiniteDescription

extractName :: [ExportClause] -> [DD.DefiniteDescription]
extractName clauseList =
  case clauseList of
    [] ->
      []
    Function ((_, alias), _) : rest ->
      alias : extractName rest
    Variant ((_, dataAlias), _) consClauseList _ : rest ->
      dataAlias : extractName (map Function consClauseList ++ rest)
