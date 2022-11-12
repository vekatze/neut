module Entity.PrimOp where

import Data.Binary
import qualified Data.Text as T
import qualified Entity.PrimNumType as PNT
import qualified GHC.Generics as G

data PrimOp
  = PrimOp T.Text [PNT.PrimNumType] PNT.PrimNumType
  deriving (Show, Eq, Ord, G.Generic)

instance Binary PrimOp
