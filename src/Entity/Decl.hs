module Entity.Decl where

import Data.Binary
import Entity.ExternalName qualified as EN
import Entity.LowType qualified as LT
import Entity.PrimNumSize qualified as PNS
import Entity.PrimType qualified as PT
import GHC.Generics

data Decl
  = Decl EN.ExternalName [LT.LowType] LT.LowType
  deriving (Generic)

instance Binary Decl

defaultDeclList :: Int -> [Decl]
defaultDeclList baseIntSize =
  [ Decl EN.malloc [LT.PrimNum (PT.Int (PNS.IntSize baseIntSize))] LT.Pointer,
    Decl EN.free [LT.Pointer] LT.Void
  ]
