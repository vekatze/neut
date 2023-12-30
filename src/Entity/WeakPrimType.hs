module Entity.WeakPrimType where

import Data.Binary
import GHC.Generics qualified as G

data WeakPrimType
  = Int (Maybe Int)
  | Float (Maybe Int)
  deriving (Show, G.Generic, Eq, Ord)

instance Binary WeakPrimType
