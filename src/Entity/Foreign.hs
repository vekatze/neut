module Entity.Foreign where

import Data.Binary
import Entity.Arch qualified as A
import Entity.ExternalName qualified as EN
import Entity.LowType qualified as LT
import GHC.Generics

data Foreign
  = Foreign EN.ExternalName [LT.LowType] LT.LowType
  deriving (Generic)

instance Binary Foreign

defaultForeignList :: A.Arch -> [Foreign]
defaultForeignList arch =
  [ Foreign EN.malloc [LT.getDefaultInt arch] LT.Pointer,
    Foreign EN.free [LT.Pointer] LT.Void
  ]
