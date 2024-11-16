module Entity.Foreign where

import Data.Binary
import Entity.Arch qualified as A
import Entity.BaseLowType qualified as BLT
import Entity.ExternalName qualified as EN
import Entity.ForeignCodType
import GHC.Generics

data Foreign
  = Foreign EN.ExternalName [BLT.BaseLowType] (ForeignCodType BLT.BaseLowType)
  deriving (Generic)

instance Binary Foreign

defaultForeignList :: A.Arch -> [Foreign]
defaultForeignList arch =
  [ Foreign EN.malloc [BLT.getWordType arch] (Cod BLT.Pointer),
    Foreign EN.free [BLT.Pointer] Void
  ]
