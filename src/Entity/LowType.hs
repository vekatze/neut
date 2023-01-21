module Entity.LowType where

import Data.Binary
import Entity.Arity qualified as A
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import GHC.Generics qualified as G

data LowType
  = PrimNum PT.PrimType
  | Pointer LowType
  | Array Int LowType -- [n x LOWTYPE]
  | Struct [LowType]
  | Function [LowType] LowType
  deriving (Eq, Ord, G.Generic)

instance Show LowType where
  show _ = "<LT>"

instance Binary LowType

voidPtr :: LowType
voidPtr =
  Pointer
    (PrimNum (PT.Int (IntSize 8)))

toVoidPtrSeq :: A.Arity -> [LowType]
toVoidPtrSeq arity =
  map (const voidPtr) [1 .. A.reify arity]
