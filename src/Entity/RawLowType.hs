module Entity.RawLowType (RawLowType (..)) where

import Entity.Arch qualified as A
import Entity.WeakPrimType qualified as WPT
import GHC.Generics qualified as G

data RawLowType
  = PrimNum WPT.WeakPrimType
  | Pointer
  | Void
  | Word A.Arch
  deriving (Eq, Ord, G.Generic)
