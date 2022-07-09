module Entity.PrimOp where

import Data.Binary
import qualified Data.Text as T
import Entity.PrimNum
import qualified GHC.Generics as G

data PrimOp
  = PrimOp T.Text [PrimNum] PrimNum
  deriving (Show, Eq, Ord, G.Generic)

instance Binary PrimOp
