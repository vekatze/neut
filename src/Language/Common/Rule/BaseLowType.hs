module Language.Common.Rule.BaseLowType
  ( BaseLowType (..),
    toVoidPtrSeq,
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.BasePrimType qualified as BPT

data BaseLowType
  = PrimNum BPT.BasePrimType
  | Pointer
  deriving (Show, Eq, Ord, G.Generic)

instance Binary BaseLowType

toVoidPtrSeq :: AN.ArgNum -> [BaseLowType]
toVoidPtrSeq argNum =
  map (const Pointer) [1 .. AN.reify argNum]
