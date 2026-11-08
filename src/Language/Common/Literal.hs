module Language.Common.Literal (Literal (..)) where

import Binary.Derive (deriveBinary)
import Language.Common.Rune qualified as RU

data Literal
  = Int Integer
  | Rune RU.Rune
  deriving (Eq, Ord, Show)

deriveBinary [''Literal]
