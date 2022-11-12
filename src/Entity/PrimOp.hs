module Entity.PrimOp where

import Data.Binary
import qualified Data.Text as T
import qualified Entity.PrimType as PT
import qualified GHC.Generics as G

data PrimOp
  = PrimOp T.Text [PT.PrimType] PT.PrimType
  deriving (Show, Eq, Ord, G.Generic)

instance Binary PrimOp
