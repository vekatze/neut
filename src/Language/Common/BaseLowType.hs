module Language.Common.BaseLowType
  ( BaseLowType (..),
    slot,
    toSlotSeq,
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.ArgNum qualified as AN
import Language.Common.BasePrimType qualified as BPT
import Language.Common.SlotSize

data BaseLowType
  = PrimNum BPT.BasePrimType
  | Pointer
  deriving (Show, Eq, Ord, G.Generic)

instance Binary BaseLowType

slot :: BaseLowType
slot =
  PrimNum (BPT.Int (BPT.Explicit slotIntSize))

toSlotSeq :: AN.ArgNum -> [BaseLowType]
toSlotSeq argNum =
  map (const slot) [1 .. AN.reify argNum]
