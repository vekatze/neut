module Language.Common.Literal (Literal (..)) where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Rune qualified as RU

data Literal
  = Int Integer
  | Rune RU.Rune
  deriving (Eq, Ord, Show, Generic)

instance Binary Literal
