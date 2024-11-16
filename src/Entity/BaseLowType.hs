module Entity.BaseLowType
  ( BaseLowType (..),
    getWordType,
    toVoidPtrSeq,
  )
where

import Data.Binary
import Entity.Arch qualified as A
import Entity.ArgNum qualified as AN
import Entity.DataSize qualified as DS
import Entity.PrimNumSize
import Entity.WeakPrimType qualified as WPT
import GHC.Generics qualified as G

data BaseLowType
  = PrimNum WPT.WeakPrimType
  | Pointer
  deriving (Show, Eq, Ord, G.Generic)

instance Binary BaseLowType

getWordType :: A.Arch -> BaseLowType
getWordType arch =
  PrimNum $ WPT.Int $ WPT.Explicit $ IntSize $ DS.reify $ A.dataSizeOf arch

toVoidPtrSeq :: AN.ArgNum -> [BaseLowType]
toVoidPtrSeq argNum =
  map (const Pointer) [1 .. AN.reify argNum]
