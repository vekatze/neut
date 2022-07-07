module Entity.GlobalName where

import Entity.Discriminant
import Entity.EnumInfo

data GlobalName
  = TopLevelFunc
  | EnumType [EnumValue]
  | EnumIntro EnumTypeName Discriminant
  | Constant
  deriving (Show)
