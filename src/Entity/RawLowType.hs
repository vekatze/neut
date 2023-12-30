module Entity.RawLowType where

import Data.Binary
import Entity.ArgNum qualified as AN
import Entity.WeakPrimType qualified as WPT
import GHC.Generics qualified as G

data RawLowType
  = PrimNum WPT.WeakPrimType
  | Pointer
  | Void
  deriving (Eq, Ord, G.Generic)

instance Show RawLowType where
  show _ = "<LT>"

instance Binary RawLowType

toVoidPtrSeq :: AN.ArgNum -> [RawLowType]
toVoidPtrSeq argNum =
  map (const Pointer) [1 .. AN.reify argNum]
