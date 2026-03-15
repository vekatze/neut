module Language.Common.StmtKind
  ( BaseStmtKindTerm (..),
    BaseStmtKindType (..),
    StmtKindTerm,
    StmtKindType,
    toOpacityTerm,
    toOpacityType,
    toLowOpacityTerm,
    toLowOpacityType,
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

data BaseStmtKindTerm name binder t
  = Define
  | Script
  | Inline
  | Constant
  | Macro
  | MacroInline
  | Main t
  | DataIntro name [binder] [binder] D.Discriminant
  deriving (Generic)

data BaseStmtKindType name binder
  = Alias
  | AliasOpaque
  | Data
      name
      [binder]
      [(SavedHint, name, IsConstLike, [binder], D.Discriminant)]
  deriving (Generic)

instance (Binary name, Binary x, Binary t) => Binary (BaseStmtKindTerm name x t)

instance (Binary name, Binary x) => Binary (BaseStmtKindType name x)

type StmtKindTerm a =
  BaseStmtKindTerm DD.DefiniteDescription (BinderF a) a

type StmtKindType a =
  BaseStmtKindType DD.DefiniteDescription (BinderF a)

toOpacityTerm :: BaseStmtKindTerm name x t -> O.Opacity
toOpacityTerm stmtKind =
  case stmtKind of
    Define ->
      O.Opaque
    Script ->
      O.Opaque
    Inline ->
      O.Clear
    Constant ->
      O.Clear
    Macro ->
      O.Clear
    MacroInline ->
      O.Clear
    Main _ ->
      O.Opaque
    DataIntro {} ->
      O.Clear

toOpacityType :: BaseStmtKindType name x -> O.Opacity
toOpacityType stmtKind =
  case stmtKind of
    Alias ->
      O.Clear
    AliasOpaque ->
      O.Opaque
    Data {} ->
      O.Clear

toLowOpacityTerm :: BaseStmtKindTerm name x t -> O.Opacity
toLowOpacityTerm stmtKind =
  case stmtKind of
    Define ->
      O.Opaque
    Script ->
      O.Opaque
    Inline ->
      O.Opaque
    Constant ->
      O.Opaque
    Macro ->
      O.Opaque
    MacroInline ->
      O.Opaque
    Main _ ->
      O.Opaque
    DataIntro {} ->
      O.Clear

toLowOpacityType :: BaseStmtKindType name x -> O.Opacity
toLowOpacityType stmtKind =
  case stmtKind of
    Alias ->
      O.Opaque
    AliasOpaque ->
      O.Opaque
    Data {} ->
      O.Opaque

isMacroStmtKind :: BaseStmtKindTerm name binder t -> Bool
isMacroStmtKind stmtKind =
  case stmtKind of
    Macro ->
      True
    MacroInline ->
      True
    _ ->
      False

isInlineStmtKind :: BaseStmtKindTerm name binder t -> Bool
isInlineStmtKind stmtKind =
  case stmtKind of
    Define ->
      False
    Script ->
      False
    Inline ->
      False -- fixme: should be true
    Constant ->
      False
    Macro ->
      True
    MacroInline ->
      True
    Main _ ->
      False
    _ ->
      False
