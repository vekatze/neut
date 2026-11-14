module Language.Common.BaseLowType
  ( BaseLowType (..),
    slot,
    toSlotSeq,
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.Arity qualified as Arity
import Language.Common.BasePrimType qualified as BPT
import Language.Common.SlotSize

data BaseLowType
  = PrimNum BPT.BasePrimType
  | Pointer
  deriving (Show, Eq, Ord)

slot :: BaseLowType
slot =
  PrimNum (BPT.Int (BPT.Explicit slotIntSize))

toSlotSeq :: Arity.Arity -> [BaseLowType]
toSlotSeq arity =
  map (const slot) [1 .. Arity.reify arity]

deriveBinary [''BaseLowType]
