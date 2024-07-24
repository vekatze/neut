module Entity.Literal (Literal (..)) where

import Data.Binary
import Entity.Rune qualified as RU
import GHC.Generics (Generic)

data Literal
  = Int Integer
  | Rune RU.Rune
  deriving (Eq, Ord, Show, Generic)

instance Binary Literal
