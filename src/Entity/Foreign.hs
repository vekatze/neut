module Entity.Foreign where

import Data.Binary
import Entity.ExternalName qualified as EN
import Entity.LowType qualified as LT
import Entity.PrimNumSize qualified as PNS
import Entity.PrimType qualified as PT
import GHC.Generics

data Foreign
  = Foreign EN.ExternalName [LT.LowType] LT.LowType
  deriving (Generic)

instance Binary Foreign

defaultForeignList :: Int -> [Foreign]
defaultForeignList baseIntSize =
  [ Foreign EN.malloc [LT.PrimNum (PT.Int (PNS.IntSize baseIntSize))] LT.Pointer,
    Foreign EN.free [LT.Pointer] LT.Void
  ]
