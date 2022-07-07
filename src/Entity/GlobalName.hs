module Entity.GlobalName where

import Entity.Discriminant
import Entity.EnumInfo

data GlobalName
  = TopLevelFunc
  | Enum [EnumItem]
  | EnumIntro EnumTypeName Discriminant
  | Constant
  deriving (Show)
