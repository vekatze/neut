module Kernel.Common.GlobalName
  ( GlobalName (..),
    getIsConstLike,
    hasNoArgs,
    disableConstLikeFlag,
    isMetaConstant,
    toMetaFunction,
  )
where

import GHC.Generics (Generic)
import Language.Common.ArgNum
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Language.Common.IsDestPassing
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Common.RuleKind (RuleKind)
import Logger.Hint

data GlobalName
  = TopLevelFuncTerm ArgNum IsConstLike IsDestPassing
  | TopLevelMetaTerm ArgNum IsConstLike
  | TopLevelFuncType ArgNum IsConstLike Bool
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data ArgNum [(DD.DefiniteDescription, (Hint, GlobalName))] IsConstLike
  | DataIntro ArgNum ArgNum D.Discriminant IsConstLike
  | Rule RuleKind
  | Trope
  | Namespace
  deriving (Generic)

getIsConstLike :: GlobalName -> IsConstLike
getIsConstLike gn =
  case gn of
    TopLevelFuncTerm _ isConstLike _ ->
      isConstLike
    TopLevelMetaTerm _ isConstLike ->
      isConstLike
    TopLevelFuncType _ isConstLike _ ->
      isConstLike
    Data _ _ isConstLike ->
      isConstLike
    DataIntro _ _ _ isConstLike ->
      isConstLike
    Trope ->
      False
    _ ->
      False

hasNoArgs :: GlobalName -> Bool
hasNoArgs gn =
  case gn of
    TopLevelFuncTerm argNum _ _ ->
      argNum == fromInt 0
    TopLevelMetaTerm argNum _ ->
      argNum == fromInt 0
    TopLevelFuncType argNum _ _ ->
      argNum == fromInt 0
    Data argNum _ _ ->
      argNum == fromInt 0
    DataIntro dataArgNum consArgNum _ _ ->
      dataArgNum == fromInt 0 && consArgNum == fromInt 0
    Rule {} ->
      False
    Trope ->
      False
    Namespace ->
      False
    PrimType _ ->
      True
    PrimOp _ ->
      False

disableConstLikeFlag :: GlobalName -> GlobalName
disableConstLikeFlag gn =
  case gn of
    TopLevelFuncTerm argNum _ isDestPassing ->
      TopLevelFuncTerm argNum False isDestPassing
    TopLevelMetaTerm argNum _ ->
      TopLevelMetaTerm argNum False
    TopLevelFuncType argNum _ isMacro ->
      TopLevelFuncType argNum False isMacro
    Data argNum consInfo _ ->
      Data argNum consInfo False
    DataIntro dataArgNum consArgNum discriminant False ->
      DataIntro dataArgNum consArgNum discriminant False
    Trope ->
      Trope
    _ ->
      gn

isMetaConstant :: GlobalName -> Bool
isMetaConstant gn =
  case gn of
    TopLevelMetaTerm _ True ->
      True
    _ ->
      False

toMetaFunction :: GlobalName -> GlobalName
toMetaFunction gn =
  case gn of
    TopLevelMetaTerm argNum True ->
      TopLevelMetaTerm argNum False
    _ ->
      gn
