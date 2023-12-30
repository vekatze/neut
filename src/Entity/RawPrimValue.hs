module Entity.RawPrimValue (RawPrimValue (..)) where

import Data.Text qualified as T
import GHC.Generics qualified as G

data RawPrimValue a
  = Int Integer
  | Float Double
  | StaticText a T.Text
  deriving (Show, G.Generic)
