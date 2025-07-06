module Kernel.Common.GlobalName
  ( GlobalName (..),
    getIsConstLike,
    hasNoArgs,
    disableConstLikeFlag,
  )
where

import GHC.Generics (Generic)
import Language.Common.ArgNum
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Common.RuleKind (RuleKind)
import Logger.Hint

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
