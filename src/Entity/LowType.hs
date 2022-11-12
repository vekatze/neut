module Entity.LowType where

import Data.Binary
import qualified Entity.Arity as A
import Entity.PrimNum
import Entity.PrimNumSize
import qualified GHC.Generics as G

data LowType
  = PrimNum PrimNum
  | Pointer LowType
  | Array Int LowType -- [n x LOWTYPE]
  | Struct [LowType]
  | Function [LowType] LowType
  deriving (Eq, Ord, Show, G.Generic)

instance Binary LowType

voidPtr :: LowType
voidPtr =
  Pointer
    (PrimNum (PrimNumInt (IntSize 8)))

toVoidPtrSeq :: A.Arity -> [LowType]
toVoidPtrSeq arity =
  map (const voidPtr) [1 .. A.reify arity]
