module Entity.PrimValue
  ( PrimValue (..),
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.PrimNumSize
import Entity.PrimOp
import Entity.Rune qualified as RU
import GHC.Generics (Generic)

data PrimValue a
  = Int IntSize Integer
  | Float FloatSize Double
  | Op PrimOp
  | StaticText a T.Text
  | Rune RU.Rune
  deriving (Show, Generic)

instance (Binary a) => Binary (PrimValue a)
