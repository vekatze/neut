module Entity.GlobalName (GlobalName (..)) where

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
  | Resource
  | Nat
  | NatZero
  | NatSucc
  deriving (Show, Generic)

instance Binary GlobalName
