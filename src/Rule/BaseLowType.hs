module Rule.BaseLowType
  ( BaseLowType (..),
    getWordType,
    toVoidPtrSeq,
  )
where

import Data.Binary
import Rule.Arch qualified as A
import Rule.ArgNum qualified as AN
import Rule.BasePrimType qualified as BPT
import Rule.DataSize qualified as DS
import Rule.PrimNumSize
import GHC.Generics qualified as G

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
