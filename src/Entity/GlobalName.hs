module Entity.GlobalName
  ( GlobalName (..),
    getIsConstLike,
  )
where

import Data.Binary
import Entity.ArgNum
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.PrimOp
import Entity.PrimType qualified as PT
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
