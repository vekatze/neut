module Entity.Ident where

import Data.Binary
import qualified Data.Text as T
import GHC.Generics

newtype Ident
  = I (T.Text, Int)
  deriving (Ord, Generic)

instance Eq Ident where
  I (_, i1) == I (_, i2) = i1 == i2

instance Show Ident where
  show (I (s, i)) =
    T.unpack s ++ "-" ++ show i

instance Binary Ident
