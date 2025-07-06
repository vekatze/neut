module Language.Common.StmtKind
  ( BaseStmtKind (..),
    StmtKind,
    toOpacity,
    toLowOpacity,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Language.Common.Opacity qualified as O
import Logger.Hint

data BaseStmtKind name binder t
  = Normal O.Opacity
  | Main O.Opacity t
  | Data
      name -- the name of the variant type
      [binder] -- variant args
      [(SavedHint, name, IsConstLike, [binder], D.Discriminant)] -- constructors
  | DataIntro name [binder] [binder] D.Discriminant
  deriving (Generic)

instance (Binary name, Binary x, Binary t) => Binary (BaseStmtKind name x t)

type StmtKind a =
  BaseStmtKind DD.DefiniteDescription (BinderF a) a

toOpacity :: BaseStmtKind name x t -> O.Opacity
toOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    Main opacity _ ->
      opacity
    _ ->
      O.Clear

toLowOpacity :: BaseStmtKind name x t -> O.Opacity
toLowOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    Main opacity _ ->
      opacity
    Data {} ->
      O.Opaque -- so as not to reduce recursive terms
    DataIntro {} ->
      O.Clear
