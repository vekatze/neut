module Entity.BaseLowType (BaseLowType (..)) where

import Entity.WeakPrimType qualified as WPT
import GHC.Generics qualified as G

data BaseLowType
  = PrimNum WPT.WeakPrimType
  | Pointer
  | Void
  deriving (Eq, Ord, G.Generic)
