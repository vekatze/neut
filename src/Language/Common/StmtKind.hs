module Language.Common.StmtKind
  ( BaseStmtKind (..),
    StmtKind,
    toOpacity,
    toLowOpacity,
    isInlineStmtKind,
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
  | Inline
  | Main O.Opacity t
  | Alias
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
    Inline ->
      O.Clear
    Main opacity _ ->
      opacity
    Alias ->
      O.Clear
    _ ->
      O.Clear

toLowOpacity :: BaseStmtKind name x t -> O.Opacity
toLowOpacity stmtKind =
  case stmtKind of
    Normal _ ->
      O.Opaque
    Inline ->
      O.Opaque
    Main _ _ ->
      O.Opaque
    Alias ->
      O.Opaque
    Data {} ->
      O.Opaque
    DataIntro {} ->
      O.Clear

isInlineStmtKind :: BaseStmtKind name binder t -> Bool
isInlineStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity == O.Clear
    Inline ->
      False
    Main opacity _ ->
      opacity == O.Clear
    _ ->
      False
