module Entity.Decl where

import Data.Binary
import Entity.ExternalName qualified as EN
import Entity.LowType qualified as LT
import GHC.Generics

data Decl
  = Decl EN.ExternalName [LT.LowType] LT.LowType
  deriving (Generic)

instance Binary Decl
