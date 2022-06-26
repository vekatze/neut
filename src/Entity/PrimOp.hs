module Entity.PrimOp where

import qualified Data.Text as T
import Entity.PrimNum

data PrimOp
  = PrimOp T.Text [PrimNum] PrimNum
  deriving (Show)
