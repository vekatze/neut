module Rule.PrimValue
  ( PrimValue (..),
  )
where

import Data.Binary
import Data.Text qualified as T
import Rule.PrimNumSize
import Rule.PrimOp
import Rule.Rune qualified as RU
import GHC.Generics (Generic)

data PrimValue a
  = Int a IntSize Integer
  | Float a FloatSize Double
  | Op PrimOp
  | StaticText a T.Text
  | Rune RU.Rune
  deriving (Show, Generic)

instance (Binary a) => Binary (PrimValue a)
