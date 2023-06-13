module Entity.Decl where

import Entity.ExternalName qualified as EN
import Entity.LowType qualified as LT

data Decl
  = Decl EN.ExternalName [LT.LowType] LT.LowType
