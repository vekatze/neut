module Main.Rule.Literal (Literal (..)) where

import Data.Binary
import GHC.Generics (Generic)
import Main.Rule.Rune qualified as RU

data Literal
  = Int Integer
  | Rune RU.Rune
  deriving (Eq, Ord, Show, Generic)

instance Binary Literal
