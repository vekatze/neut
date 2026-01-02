module Language.Common.StmtKind
  ( BaseStmtKind (..),
    StmtKind,
    toOpacity,
    toLowOpacity,
    isMacroStmtKind,
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
  = Define
  | Inline
  | Macro
  | Main t
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
    Define ->
      O.Opaque
    Inline ->
      O.Clear
    Macro ->
      O.Clear
    Main _ ->
      O.Opaque
    Alias ->
      O.Clear
    _ ->
      O.Clear

toLowOpacity :: BaseStmtKind name x t -> O.Opacity
toLowOpacity stmtKind =
  case stmtKind of
    Define ->
      O.Opaque
    Inline ->
      O.Opaque
    Macro ->
      O.Opaque
    Main _ ->
      O.Opaque
    Alias ->
      O.Opaque
    Data {} ->
      O.Opaque
    DataIntro {} ->
      O.Clear

isMacroStmtKind :: BaseStmtKind name binder t -> Bool
isMacroStmtKind stmtKind =
  case stmtKind of
    Macro ->
      True
    _ ->
      False

isInlineStmtKind :: BaseStmtKind name binder t -> Bool
isInlineStmtKind stmtKind =
  case stmtKind of
    Define ->
      False
    Inline ->
      False
    Macro ->
      True
    Main _ ->
      False
    _ ->
      False
