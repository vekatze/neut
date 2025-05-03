module Main.Rule.GlobalName
  ( GlobalName (..),
    getIsConstLike,
  )
where

import Data.Binary
import GHC.Generics (Generic)
import Main.Rule.ArgNum
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Discriminant qualified as D
import Main.Rule.Hint
import Main.Rule.IsConstLike
import Main.Rule.PrimOp
import Main.Rule.PrimType qualified as PT

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
