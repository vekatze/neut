module Kernel.Common.Rule.GlobalName
  ( GlobalName (..),
    getIsConstLike,
  )
where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Rule.ArgNum
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimType qualified as PT
import Logger.Rule.Hint

data GlobalName
  = TopLevelFunc ArgNum IsConstLike
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data ArgNum [(DD.DefiniteDescription, (Hint, GlobalName))] IsConstLike
  | DataIntro ArgNum ArgNum D.Discriminant IsConstLike
  deriving (Show, Generic)

instance Binary GlobalName

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
