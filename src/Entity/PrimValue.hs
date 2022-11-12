module Entity.PrimValue
  ( PrimValue (..),
  )
where

import GHC.Generics (Generic)
import Data.Binary
import Entity.PrimNumSize
import Entity.PrimOp

data PrimValue
  = Int IntSize Integer
  | Float FloatSize Double
  | Op PrimOp
  deriving (Show, Generic)

instance Binary PrimValue

