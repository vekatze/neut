module Entity.GlobalName (GlobalName (..)) where

import Entity.Discriminant
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import Entity.PrimNum
import Entity.PrimOp

data GlobalName
  = TopLevelFunc
  | Resource
  | EnumType [EnumValue]
  | EnumIntro ET.EnumTypeName Discriminant
  | PrimType PrimNum
  | PrimOp PrimOp
  deriving (Show)
