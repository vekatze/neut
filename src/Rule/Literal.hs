module Rule.Literal (Literal (..)) where

import Data.Binary
import Rule.Rune qualified as RU
import GHC.Generics (Generic)

data Literal
  = Int Integer
  | Rune RU.Rune
  deriving (Eq, Ord, Show, Generic)

instance Binary Literal
