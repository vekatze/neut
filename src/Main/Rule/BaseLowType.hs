module Main.Rule.BaseLowType
  ( BaseLowType (..),
    getWordType,
    toVoidPtrSeq,
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Main.Rule.Arch qualified as A
import Main.Rule.ArgNum qualified as AN
import Main.Rule.BasePrimType qualified as BPT
import Main.Rule.DataSize qualified as DS
import Main.Rule.PrimNumSize

data BaseLowType
  = PrimNum BPT.BasePrimType
  | Pointer
  deriving (Show, Eq, Ord, G.Generic)

instance Binary BaseLowType

getWordType :: A.Arch -> BaseLowType
getWordType arch =
  PrimNum $ BPT.Int $ BPT.Explicit $ IntSize $ DS.reify $ A.dataSizeOf arch

toVoidPtrSeq :: AN.ArgNum -> [BaseLowType]
toVoidPtrSeq argNum =
  map (const Pointer) [1 .. AN.reify argNum]
