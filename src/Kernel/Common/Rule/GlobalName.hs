module Kernel.Common.Rule.GlobalName
  ( GlobalName (..),
    getIsConstLike,
    hasNoArgs,
    disableConstLikeFlag,
  )
where

import GHC.Generics (Generic)
import Language.Common.Rule.ArgNum
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.RuleKind (RuleKind)
import Logger.Rule.Hint

data GlobalName
  = TopLevelFunc ArgNum IsConstLike
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data ArgNum [(DD.DefiniteDescription, (Hint, GlobalName))] IsConstLike
  | DataIntro ArgNum ArgNum D.Discriminant IsConstLike
  | Rule RuleKind
  deriving (Generic)

getIsConstLike :: GlobalName -> IsConstLike
getIsConstLike gn =
  case gn of
    TopLevelFunc _ isConstLike ->
      isConstLike
    Data _ _ isConstLike ->
      isConstLike
    DataIntro _ _ _ isConstLike ->
      isConstLike
    _ ->
      False

hasNoArgs :: GlobalName -> Bool
hasNoArgs gn =
  case gn of
    TopLevelFunc argNum _ ->
      argNum == fromInt 0
    Data argNum _ _ ->
      argNum == fromInt 0
    DataIntro dataArgNum consArgNum _ _ ->
      dataArgNum == fromInt 0 && consArgNum == fromInt 0
    Rule {} ->
      False
    PrimType _ ->
      True
    PrimOp _ ->
      False

disableConstLikeFlag :: GlobalName -> GlobalName
disableConstLikeFlag gn =
  case gn of
    TopLevelFunc argNum _ ->
      TopLevelFunc argNum False
    Data argNum consInfo _ ->
      Data argNum consInfo False
    DataIntro dataArgNum consArgNum discriminant False ->
      DataIntro dataArgNum consArgNum discriminant False
    _ ->
      gn
