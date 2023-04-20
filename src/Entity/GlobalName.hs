module Entity.GlobalName (GlobalName (..)) where

import Data.Binary
import Entity.Arity
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.IsConstLike
import Entity.PrimOp
import Entity.PrimType qualified as PT
import GHC.Generics (Generic)

data GlobalName
  = TopLevelFunc Arity IsConstLike
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data Arity [DD.DefiniteDescription] IsConstLike
  | DataIntro Arity Arity D.Discriminant IsConstLike
  | Resource
  | Alias DD.DefiniteDescription GlobalName
  | AliasData DD.DefiniteDescription [DD.DefiniteDescription] GlobalName
  | Projection Arity IsConstLike
  deriving (Show, Generic)

instance Binary GlobalName
