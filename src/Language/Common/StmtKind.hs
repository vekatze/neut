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
    startsAtStage1,
    isMetaOnlyStmtKind,
    isInlineStmtKind,
    isDestPassingStmtKind,
    isOpaqueTypeStmtKind,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsNominal (IsNominal)
import Language.Common.Opacity qualified as O

data BaseStmtKindTerm name binder t
  = Define
  | DestPassing
  | DestPassingInline
  | Inline
  | Constant
  | ConstantMeta
  | Macro
  | MacroInline
  | Main t
  | DataIntro name [binder] [binder] D.Discriminant
  deriving (Generic)

data BaseStmtKindType binder
  = Alias
  | AliasOpaque
  | Data DD.DefiniteDescription [binder] [DI.StmtConsInfo binder] IsNominal
  deriving (Generic)

instance (Binary name, Binary x, Binary t) => Binary (BaseStmtKindTerm name x t)

instance (Binary x) => Binary (BaseStmtKindType x)

type StmtKindTerm a =
  BaseStmtKindTerm DD.DefiniteDescription (BinderF a) a

type StmtKindType a =
  BaseStmtKindType (BinderF a)

toOpacityTerm :: BaseStmtKindTerm name x t -> O.Opacity
toOpacityTerm stmtKind =
  case stmtKind of
    Define ->
      O.Opaque
    DestPassing ->
      O.Opaque
    DestPassingInline ->
      O.Clear
    Inline ->
      O.Clear
    Constant ->
      O.Clear
    ConstantMeta ->
      O.Clear
    Macro ->
      O.Clear
    MacroInline ->
      O.Clear
    Main _ ->
      O.Opaque
    DataIntro {} ->
      O.Clear

toOpacityType :: BaseStmtKindType x -> O.Opacity
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
    DestPassing ->
      O.Opaque
    DestPassingInline ->
      O.Opaque
    Inline ->
      O.Opaque
    Constant ->
      O.Opaque
    ConstantMeta ->
      O.Opaque
    Macro ->
      O.Opaque
    MacroInline ->
      O.Opaque
    Main _ ->
      O.Opaque
    DataIntro {} ->
      O.Clear

toLowOpacityType :: BaseStmtKindType x -> O.Opacity
toLowOpacityType stmtKind =
  case stmtKind of
    Alias ->
      O.Clear
    AliasOpaque ->
      O.Opaque
    Data {} ->
      O.Opaque

isMacroStmtKind :: BaseStmtKindTerm name binder t -> Bool
isMacroStmtKind stmtKind =
  startsAtStage1 stmtKind

startsAtStage1 :: BaseStmtKindTerm name binder t -> Bool
startsAtStage1 stmtKind =
  case stmtKind of
    ConstantMeta ->
      True
    Macro ->
      True
    MacroInline ->
      True
    _ ->
      False

isMetaOnlyStmtKind :: BaseStmtKindTerm name binder t -> Bool
isMetaOnlyStmtKind stmtKind =
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
    DestPassing ->
      False
    DestPassingInline ->
      False
    Inline ->
      False -- fixme: should be true
    Constant ->
      False
    ConstantMeta ->
      True
    Macro ->
      True
    MacroInline ->
      True
    Main _ ->
      False
    _ ->
      False

isDestPassingStmtKind :: BaseStmtKindTerm name binder t -> Bool
isDestPassingStmtKind stmtKind =
  case stmtKind of
    DestPassing ->
      True
    DestPassingInline ->
      True
    _ ->
      False

isOpaqueTypeStmtKind :: BaseStmtKindType binder -> Bool
isOpaqueTypeStmtKind stmtKind =
  case stmtKind of
    AliasOpaque ->
      True
    _ ->
      False
