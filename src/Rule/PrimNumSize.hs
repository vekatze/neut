module Rule.PrimNumSize
  ( IntSize (..),
    FloatSize (..),
    intSize32,
  )
where

import Data.Binary
import GHC.Generics qualified as G

newtype IntSize
  = IntSize Int
  deriving (Eq, Ord, Show, G.Generic)

instance Binary IntSize

data FloatSize
  = FloatSize16
  | FloatSize32
  | FloatSize64
  deriving (Eq, Ord, Show, G.Generic)

instance Binary FloatSize

intSize32 :: IntSize
intSize32 =
  IntSize 32
