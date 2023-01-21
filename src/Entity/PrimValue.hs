module Entity.PrimValue
  ( PrimValue (..),
  )
where

import Data.Binary
import Entity.PrimNumSize
import Entity.PrimOp
import GHC.Generics (Generic)

data PrimValue
  = Int IntSize Integer
  | Float FloatSize Double
  | Op PrimOp
  deriving (Show, Generic)

instance Binary PrimValue
