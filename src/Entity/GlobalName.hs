module Entity.GlobalName (GlobalName (..)) where

import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.PrimOp
import qualified Entity.PrimType as PT

data GlobalName
  = TopLevelFunc Arity
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data Arity [DD.DefiniteDescription]
  | DataIntro Arity Arity D.Discriminant
  | Resource
  deriving (Show)
