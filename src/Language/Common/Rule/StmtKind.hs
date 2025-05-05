module Language.Common.Rule.StmtKind
  ( BaseStmtKind (..),
    StmtKind,
    toOpacity,
    toLowOpacity,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.Rule.Binder
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.Opacity qualified as O
import Logger.Rule.Hint

-- import Language.RawTerm.Rule.RawBinder (RawBinder)
-- import Language.RawTerm.Rule.RawTerm qualified as RT

data BaseStmtKind name b t
  = Normal O.Opacity
  | Data
      name -- the name of the variant type
      [b] -- variant args
      [(SavedHint, name, IsConstLike, [b], D.Discriminant)] -- constructors
  | DataIntro name [b] [b] D.Discriminant
  deriving (Generic)

instance (Binary name, Binary x, Binary t) => Binary (BaseStmtKind name x t)

type StmtKind a =
  BaseStmtKind DD.DefiniteDescription (BinderF a) a

toOpacity :: BaseStmtKind name x t -> O.Opacity
toOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    _ ->
      O.Clear

toLowOpacity :: BaseStmtKind name x t -> O.Opacity
toLowOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    Data {} ->
      O.Opaque -- so as not to reduce recursive terms
    DataIntro {} ->
      O.Clear
