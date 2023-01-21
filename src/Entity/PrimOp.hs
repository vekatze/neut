module Entity.PrimOp where

import Data.Binary
import Data.Text qualified as T
import Entity.PrimType qualified as PT
import GHC.Generics qualified as G

data PrimOp
  = PrimOp T.Text [PT.PrimType] PT.PrimType
  deriving (Show, Eq, Ord, G.Generic)

instance Binary PrimOp
