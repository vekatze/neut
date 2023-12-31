module Entity.RawLowType (RawLowType (..)) where

import Entity.WeakPrimType qualified as WPT
import GHC.Generics qualified as G

data RawLowType
  = PrimNum WPT.WeakPrimType
  | Pointer
  | Void
  deriving (Eq, Ord, G.Generic)
