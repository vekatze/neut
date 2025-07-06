module Language.Term.PrimValue
  ( PrimValue (..),
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.Common.Rune qualified as RU

data PrimValue a
  = Int a IntSize Integer
  | Float a FloatSize Double
  | Op PrimOp
  | StaticText a T.Text
  | Rune RU.Rune
  deriving (Show, Generic)

instance (Binary a) => Binary (PrimValue a)
