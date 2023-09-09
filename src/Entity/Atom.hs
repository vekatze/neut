module Entity.Atom (Atom (..)) where

import Data.Text qualified as T

data Atom
  = Symbol T.Text
  | String T.Text
  deriving (Show)
