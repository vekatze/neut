module Entity.GlobalName (GlobalName (..)) where

import Entity.Arity
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.PrimOp
import Entity.PrimType qualified as PT
import Entity.Stmt

data GlobalName
  = TopLevelFunc Arity IsConstLike
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data Arity [DD.DefiniteDescription] IsConstLike
  | DataIntro Arity Arity D.Discriminant
  | Resource
  deriving (Show)
