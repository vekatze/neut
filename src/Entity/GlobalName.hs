module Entity.GlobalName (GlobalName (..)) where

import Entity.Arity
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.PrimOp
import Entity.PrimType qualified as PT

data GlobalName
  = TopLevelFunc Arity
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data Arity [DD.DefiniteDescription]
  | DataIntro Arity Arity D.Discriminant
  deriving (Show)
