module Rule.GlobalName
  ( GlobalName (..),
    getIsConstLike,
  )
where

import Data.Binary
import Rule.ArgNum
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.Hint
import Rule.IsConstLike
import Rule.PrimOp
import Rule.PrimType qualified as PT
import GHC.Generics (Generic)

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
