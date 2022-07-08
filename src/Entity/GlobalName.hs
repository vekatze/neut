module Entity.GlobalName where

import Entity.Discriminant
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET

data GlobalName
  = TopLevelFunc
  | EnumType [EnumValue]
  | EnumIntro ET.EnumTypeName Discriminant
  | Constant
  deriving (Show)
