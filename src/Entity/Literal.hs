module Entity.Literal (Literal (..)) where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)

data Literal
  = Int Integer
  | Rune T.Text
  deriving (Eq, Ord, Show, Generic)

instance Binary Literal
