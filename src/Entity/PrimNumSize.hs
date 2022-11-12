module Entity.PrimNumSize where

import Data.Binary
import qualified GHC.Generics as G

data PrimNumSize
  = PrimIntSize IntSize
  | PrimFloatSize FloatSize

newtype IntSize
  = IntSize Int
  deriving (Eq, Ord, Show, G.Generic)

instance Binary IntSize

-- fixme: provide a "smart constructor" for IntSize (ensure 1 <= size <= 64)

data FloatSize
  = FloatSize16
  | FloatSize32
  | FloatSize64
  deriving (Eq, Ord, Show, G.Generic)

instance Binary FloatSize
