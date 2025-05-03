module Main.Rule.PrimValue
  ( PrimValue (..),
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)
import Main.Rule.PrimNumSize
import Main.Rule.PrimOp
import Main.Rule.Rune qualified as RU

data PrimValue a
  = Int a IntSize Integer
  | Float a FloatSize Double
  | Op PrimOp
  | StaticText a T.Text
  | Rune RU.Rune
  deriving (Show, Generic)

instance (Binary a) => Binary (PrimValue a)
