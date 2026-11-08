module Language.Common.BaseLowType
  ( BaseLowType (..),
    slot,
    toSlotSeq,
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.ArgNum qualified as AN
import Language.Common.BasePrimType qualified as BPT
import Language.Common.SlotSize

data BaseLowType
  = PrimNum BPT.BasePrimType
  | Pointer
  deriving (Show, Eq, Ord)

slot :: BaseLowType
slot =
  PrimNum (BPT.Int (BPT.Explicit slotIntSize))

toSlotSeq :: AN.ArgNum -> [BaseLowType]
toSlotSeq argNum =
  map (const slot) [1 .. AN.reify argNum]

deriveBinary [''BaseLowType]
