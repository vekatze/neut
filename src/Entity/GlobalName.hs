module Entity.GlobalName (GlobalName (..)) where

import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import Entity.Discriminant
import qualified Entity.Discriminant as D
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import Entity.PrimOp
import qualified Entity.PrimType as PT

data GlobalName
  = TopLevelFunc Arity
  | EnumType [EnumValue]
  | EnumIntro ET.EnumTypeName Discriminant
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data Arity [DD.DefiniteDescription]
  | DataIntro Arity Arity D.Discriminant
  deriving (Show)
