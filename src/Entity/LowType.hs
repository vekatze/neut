module Entity.LowType where

import Data.Binary
import qualified Entity.Arity as A
import Entity.PrimNumSize
import qualified Entity.PrimNumType as PNT
import qualified GHC.Generics as G

data LowType
  = PrimNum PNT.PrimNumType
  | Pointer LowType
  | Array Int LowType -- [n x LOWTYPE]
  | Struct [LowType]
  | Function [LowType] LowType
  deriving (Eq, Ord, Show, G.Generic)

instance Binary LowType

voidPtr :: LowType
voidPtr =
  Pointer
    (PrimNum (PNT.Int (IntSize 8)))

toVoidPtrSeq :: A.Arity -> [LowType]
toVoidPtrSeq arity =
  map (const voidPtr) [1 .. A.reify arity]
